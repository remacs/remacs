//! Lock files for editing.

use remacs_macros::lisp_fn;

use std::fs::{remove_file, File};
use std::io::prelude::Read;
use std::io::Error;
use std::io::ErrorKind::{InvalidData, InvalidInput, NotFound, PermissionDenied};
use std::io::Result;
use std::path::{Path, PathBuf};
use systemstat::Platform;

#[cfg(unix)]
use libc::{ELOOP, O_NOFOLLOW, O_RDONLY};
#[cfg(unix)]
use std::{ffi::CString, ffi::OsStr, os::unix::ffi::OsStrExt, os::unix::io::FromRawFd};

use crate::{
    coding::encode_file_name,
    editfns::system_name,
    fileio::expand_file_name,
    lisp::LispObject,
    multibyte::LispStringRef,
    remacs_sys::{emacs_open, lock_file, maybe_quit, unlock_file},
    remacs_sys::{Qnil, Qstringp, Qt},
    threads::ThreadState,
};

/// An arbitrary limit on lock contents length when it is stored in the
/// contents of the lock file.  8 K should be plenty big enough in practice.
const MAX_LOCK_INFO: usize = 8 * 1024;

/// The lock info as parsed from a string like `USER@HOST.PID:BOOT_TIME`.
#[derive(PartialEq, Eq, Debug)]
struct LockInfo {
    /// The user that owns the lock.
    user: String,
    /// The host on which the file was locked.
    host: String,
    /// The ID of the process that locked the file.
    pid: i32,
    /// Boot time of the system that locked the file, as Unix epoch seconds.
    boot_time: Option<i64>,
}

impl LockInfo {
    /// Parses a string like `USER@HOST.PID:BOOT_TIME` into a [`LockInfo`].
    /// Returns [`None`] if the parse fails.
    fn parse(data: &str) -> Option<Self> {
        // Treat "\357\200\242" (U+F022 in UTF-8) as if it were ":" (Bug#24656).
        // This works around a bug in the Linux CIFS kernel client, which can
        // mistakenly transliterate ':' to U+F022 in symlink contents.
        // See https://bugzilla.redhat.com/show_bug.cgi?id=1384153
        let boot_time_sep = |c| c == ':' || c == '\u{F022}';

        // The USER is everything before the last @.
        let (user, rest) = data.split_at(data.rfind('@')?);
        let user = user.to_string();

        // The HOST is everything after the '@' to the last '.'.
        let rest = rest.trim_start_matches('@');
        let (host, rest) = rest.split_at(rest.rfind('.')?);
        let host = host.to_string();

        // The PID is everything from the last '.' to the ':' or equivalent.
        // NOTE: Unlike GNU Emacs, we do not tolerate negative PID values
        // or over/underflow.
        let (pid_str, boot_time_str) =
            rest.split_at(rest.find(boot_time_sep).unwrap_or_else(|| rest.len()));
        let pid = pid_str[1..].parse::<i32>().ok().filter(|pid| *pid > 0)?;

        // After the ':' or equivalent, if there is one, comes the boot time.
        let boot_time = if boot_time_str.is_empty() {
            None
        } else {
            // Invalid boot time results in a failed parse of the LockInfo
            // rather than just boot_time = None.
            // NOTE: Unlike GNU Emacs, this also applies to over/underflow.
            // NOTE: Negative value is allowed.
            Some(
                boot_time_str
                    .trim_start_matches(boot_time_sep)
                    .parse()
                    .ok()?,
            )
        };

        Some(Self {
            user,
            host,
            pid,
            boot_time,
        })
    }
}

/// The current locking state of a file.
#[derive(PartialEq, Eq, Debug)]
enum LockState {
    /// The file is not locked.
    NotLocked,
    /// The file is locked by this Emacs process.
    LockedByUs,
    /// The file is locked by some other Emacs process identified
    /// by [`LockInfo`].
    LockedBy(LockInfo),
}

/// Opens a file in `O_NOFOLLOW` mode. Returns [`None`] if the open failed
/// because the `path` refers to a symbolic link.
#[cfg(unix)]
fn open_nofollow(path: &Path) -> Result<Option<File>> {
    let c_path =
        CString::new(path.as_os_str().as_bytes()).map_err(|err| Error::new(InvalidInput, err))?;
    let open_res = unsafe { emacs_open(c_path.as_ptr(), O_RDONLY | O_NOFOLLOW, 0) };

    match open_res {
        -1 => {
            let os_error = Error::last_os_error();
            if os_error.raw_os_error() == Some(ELOOP) {
                Ok(None)
            } else {
                Err(os_error)
            }
        }

        fd => {
            let file = unsafe { File::from_raw_fd(fd) };
            Ok(Some(file))
        }
    }
}

#[cfg(windows)]
fn open_nofollow(path: &Path) -> Result<Option<File>> {} // TODO

/// Reads the contents of a regular file at `path` up to `max_content_length` bytes.
/// Returns [`None`] if the `path` is a symbolic link.
fn read_nofollow(path: &Path, max_content_length: usize) -> Result<Option<String>> {
    let result = match open_nofollow(path)? {
        Some(file) => {
            let mut content = String::with_capacity(max_content_length);
            file.take(max_content_length as u64)
                .read_to_string(&mut content)?;
            Some(content)
        }
        None => None,
    };

    Ok(result)
}

/// Reads the target of the symbolic link at `path` and converts it
/// to a [`String`]. Fails if the target is not valid UTF-8.
fn read_link_as_string(path: &Path) -> Result<String> {
    path.read_link()?
        .into_os_string()
        .into_string()
        .map_err(|_| {
            Error::new(
                InvalidData,
                format!("Target of link '{}' is not valid UTF-8", path.display()),
            )
        })
}

/// Reads the info string for the lock file `path`.
fn read_lock_data(path: &Path) -> Result<String> {
    loop {
        match read_link_as_string(path) {
            ok @ Ok(_) => return ok,

            Err(ref e) if e.kind() == InvalidInput => {
                if let Some(target) = read_nofollow(path, MAX_LOCK_INFO)? {
                    return Ok(target);
                }
            }

            err => return err,
        }

        // `read_link` saw a non-symlink, but `open` saw a symlink.
        // The former must have been removed and replaced by the latter.
        // Try again.
        unsafe { maybe_quit() }
    }
}

/// Attempts to read and parse the lock information from the lock file `path`.
/// Returns [`None`] if the lock file does not exist.
fn read_lock_info(path: &Path) -> Result<Option<LockInfo>> {
    match read_lock_data(path) {
        Ok(data) => {
            let info = LockInfo::parse(&data).ok_or_else(|| {
                Error::new(
                    InvalidData,
                    format!("Invalid lock information in '{}'", path.display()),
                )
            })?;
            Ok(Some(info))
        }

        Err(ref e) if e.kind() == NotFound => Ok(None),

        Err(e) => Err(e),
    }
}

/// Returns [`true`] if a process with PID `pid` is running.
#[cfg(unix)]
fn process_exists(pid: i32) -> bool {
    match unsafe { libc::kill(pid, 0) } {
        0 => true,
        // `PermissionDenied` indicates that there _is_ a process but it’s
        // owned by another user
        _ => Error::last_os_error().kind() == PermissionDenied,
    }
}

#[cfg(windows)]
fn process_exists(pid: i32) -> bool {} // TODO

/// Returns the system’s last boot time as Unix epoch seconds.
fn get_boot_time() -> Result<i64> {
    let sys = systemstat::System::new();
    sys.boot_time().map(|t| t.timestamp())
}

/// Returns [`true`] if there is no boot time in the lock info or if the
/// boot time is within one second of the system’s boot time.
fn boot_time_within_one_second(info: &LockInfo) -> bool {
    info.boot_time.map_or(true, |boot_time| {
        let system_boot_time = get_boot_time().unwrap_or(0);
        (boot_time - system_boot_time).abs() <= 1
    })
}

/// Returns the current state of the lock from lock file `path`.
fn current_lock_owner(path: &Path) -> Result<LockState> {
    let result = match read_lock_info(path)? {
        Some(info) => {
            // On current host?
            if info.host.as_bytes() == system_name().as_slice() {
                if info.pid == std::process::id() as i32 {
                    // We own it.
                    LockState::LockedByUs
                } else if process_exists(info.pid) && boot_time_within_one_second(&info) {
                    // An existing process on this machine owns it.
                    LockState::LockedBy(info)
                } else {
                    // The owner process is dead or has a strange pid, so try to
                    // zap the lockfile.
                    remove_file(path)?;
                    LockState::NotLocked
                }
            } else {
                LockState::LockedBy(info)
            }
        }

        None => LockState::NotLocked,
    };

    Ok(result)
}

#[cfg(unix)]
fn to_path_buf(path: LispStringRef) -> PathBuf {
    let path = encode_file_name(path);
    PathBuf::from(OsStr::from_bytes(path.as_slice()))
}

#[cfg(windows)]
fn to_path_buf(path: LispStringRef) -> PathBuf {} // TODO (may have to return a Result)

/// Generates a path to a lock file corresponding to the given
/// file name in `path`.
fn make_lock_name(path: LispStringRef) -> PathBuf {
    let mut path = to_path_buf(path);

    let mut lock_file_name = std::ffi::OsString::from(".#");
    lock_file_name.push(path.file_name().unwrap_or_else(|| std::ffi::OsStr::new("")));

    path.set_file_name(lock_file_name);

    path
}

/// Lock FILE, if current buffer is modified.
/// FILE defaults to current buffer's visited file,
/// or else nothing is done if current buffer isn't visiting a file.
///
/// If the option `create-lockfiles' is nil, this does nothing.
#[lisp_fn(min = "0")]
pub fn lock_buffer(file: LispObject) {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let file = if file.is_nil() {
        cur_buf.truename()
    } else if file.is_string() {
        file
    } else {
        wrong_type!(Qstringp, file)
    };

    if cur_buf.modified_since_save() && !file.is_nil() {
        unsafe { lock_file(file) }
    }
}

/// Unlock the file visited in the current buffer.
/// If the buffer is not modified, this does nothing because the file
/// should not be locked in that case.
#[lisp_fn(name = "unlock-buffer")]
pub fn unlock_buffer_lisp() {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let truename = cur_buf.truename();

    if cur_buf.modified_since_save() && truename.is_string() {
        unsafe { unlock_file(truename) }
    }
}

/// Return a value indicating whether FILENAME is locked.
/// The value is nil if the FILENAME is not locked,
/// t if it is locked by you, else a string saying which user has locked it.
#[lisp_fn]
pub fn file_locked_p(filename: LispStringRef) -> LispObject {
    let path = make_lock_name(expand_file_name(filename, None));

    match current_lock_owner(&path) {
        Ok(LockState::NotLocked) | Err(_) => Qnil,
        Ok(LockState::LockedByUs) => Qt,
        Ok(LockState::LockedBy(info)) => LispObject::from(info.user.as_str()),
    }
}

include!(concat!(env!("OUT_DIR"), "/filelock_exports.rs"));

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::remove_file;
    use tempfile::NamedTempFile;

    #[cfg(unix)]
    #[test]
    fn test_read_link_as_string() -> Result<()> {
        use std::os::unix::fs::symlink;

        let lock_temp_file = NamedTempFile::new()?;
        let lock_file = lock_temp_file.path();
        let target = "test";

        remove_file(lock_file)?;
        symlink(target, lock_file)?;

        let link = read_link_as_string(lock_file)?;
        assert_eq!(link, target);

        Ok(())
    }

    fn example() -> LockInfo {
        LockInfo {
            user: "some-user.name".to_string(),
            host: "test.example.org".to_string(),
            pid: 123,
            boot_time: None,
        }
    }

    #[test]
    fn test_parse_lock_info_without_boot_time() {
        let info = LockInfo::parse("some-user.name@test.example.org.123");
        let expected = example();

        assert_eq!(info, Some(expected));
    }

    #[test]
    fn test_parse_lock_info_with_boot_time() {
        let info = LockInfo::parse("some-user.name@test.example.org.123:1553441466");
        let expected = LockInfo {
            boot_time: Some(1553441466),
            ..example()
        };

        assert_eq!(info, Some(expected));
    }

    #[test]
    fn test_parse_lock_info_with_cifs_boot_time() {
        let info = LockInfo::parse("some-user.name@test.example.org.123\u{F022}1553441466");
        let expected = LockInfo {
            boot_time: Some(1553441466),
            ..example()
        };

        assert_eq!(info, Some(expected));
    }

    #[test]
    fn test_parse_lock_info_with_invalid_boot_time() {
        let info = LockInfo::parse("user@test.123:bogus");

        assert_eq!(info, None);
    }

    #[test]
    fn test_parse_lock_info_with_negative_pid() {
        let info = LockInfo::parse("user@test.-123");

        assert_eq!(info, None);
    }

    #[test]
    fn test_parse_lock_info_with_overflow_pid() {
        let info = LockInfo::parse("user@test.99999999999999999");

        assert_eq!(info, None);
    }

    #[test]
    fn test_parse_lock_info_with_overflow_boot_time() {
        let info = LockInfo::parse("user@test.123:999999999999999999999999999");

        assert_eq!(info, None);
    }
}
