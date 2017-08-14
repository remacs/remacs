use rand;
use errno;

use std::ffi::{CStr, CString};
use std::io;

use libc::{self, c_int, c_char, EEXIST, EINVAL};

#[cfg(unix)]
use libc::{O_CLOEXEC, O_EXCL, O_RDWR, O_CREAT, open};

#[cfg(windows)]
extern "C" {
    fn sys_open(filename: *const c_char, flags: c_int, mode: c_int) -> c_int;
}

#[cfg(test)]
use std::env;

use self::rand::Rng;

const NUM_RETRIES: usize = 50;

#[no_mangle]
pub extern "C" fn rust_make_temp(template: *mut c_char, flags: c_int) -> c_int {
    let save_errno = errno::errno();
    let template_string = unsafe { CStr::from_ptr(template).to_string_lossy().into_owned() };

    match make_temporary_file(template_string, flags) {
        Ok(result) => {
            errno::set_errno(save_errno);
            let name = CString::new(result.1).unwrap();
            unsafe { libc::strcpy(template, name.as_ptr()) };
            result.0
        }

        Err(error_code) => {
            errno::set_errno(errno::Errno(error_code));
            -1
        }
    }

}

pub fn make_temporary_file(template: String, flags: i32) -> Result<(i32, String), i32> {
    let mut validated_template = try!(validate_template(template));
    for _ in 0..NUM_RETRIES {
        generate_temporary_filename(&mut validated_template);
        let attempt = try!(CString::new(validated_template.clone()).map_err(|_| EEXIST));
        let file_handle = match open_temporary_file(&attempt, flags) {
            Ok(file) => file,
            Err(_) => continue,
        };

        return Ok((file_handle, validated_template));
    }

    Err(EEXIST)
}

fn validate_template(template: String) -> Result<String, i32> {
    if !template.ends_with("XXXXXX") {
        Err(EINVAL)
    } else {
        Ok(template)
    }
}

fn generate_temporary_filename(name: &mut String) {
    let len = name.len();
    assert!(len >= 6);
    let name_vec = unsafe { &mut name.as_mut_vec() };

    let bytes = &mut name_vec[len - 6..len];
    rand::thread_rng().fill_bytes(bytes);
    for byte in bytes.iter_mut() {
        *byte = match *byte % 62 {
            v @ 0...9 => (v + b'0' as u8),
            v @ 10...35 => (v - 10 + b'a' as u8),
            v @ 36...61 => (v - 36 + b'A' as u8),
            _ => unreachable!(),
        }
    }
}

#[cfg(unix)]
fn open_temporary_file(name: &CString, flags: c_int) -> io::Result<c_int> {
    unsafe {
        match open(
            name.as_ptr(),
            O_CLOEXEC | O_EXCL | O_RDWR | O_CREAT | flags,
            0o600,
        ) {
            -1 => Err(io::Error::last_os_error()),
            fd => Ok(fd),
        }
    }
}

#[cfg(windows)]
fn open_temporary_file(name: &CString, flags: c_int) -> io::Result<c_int> {
    let _O_CREAT = 0x0100;
    let _O_EXCL = 0x0400;
    let _O_RDWR = 0x0002;
    unsafe {
        match sys_open(name.as_ptr(), _O_CREAT | _O_EXCL | _O_RDWR | flags, 0o600) {
            -1 => Err(io::Error::last_os_error()),
            fd => Ok(fd),
        }
    }
}

#[test]
#[should_panic]
fn test_generate_bad_temporary_filename() {
    let mut bad_name = String::from("1234");
    generate_temporary_filename(&mut bad_name);
}

#[test]
#[should_panic]
fn test_bad_temporary_filename_validation() {
    let bad_name = String::from("123");
    validate_template(bad_name).unwrap();
}

#[test]
fn test_good_temporary_filename_validation() {
    let good_name_1 = String::from("XXXXXX");
    let good_name_2 = String::from(".emacsXXXXXX");
    let good_name_3 = String::from("âââââ薔薔薔薔薔XXXXXX");
    validate_template(good_name_1).unwrap();
    validate_template(good_name_2).unwrap();
    validate_template(good_name_3).unwrap();
}

#[test]
fn test_generate_temporary_filename() {
    let mut name = String::from(".emacs-XXXXXX");
    let name_copy = name.clone();
    generate_temporary_filename(&mut name);
    assert!(
        name != name_copy,
        "Temporary filename should always mutate the name string"
    );
}

#[test]
fn test_generate_temporary_filename_change() {
    let mut name = String::from(".emacs-XXXXXX");
    generate_temporary_filename(&mut name);
    assert!(!name.ends_with("XXXXXX"));
}

#[test]
fn test_rust_make_temp() {
    let mut tmpdir = env::temp_dir();
    tmpdir.push(".emacs-XXXXXX");
    let fullpath = tmpdir.to_string_lossy().into_owned();
    let name = CString::new(fullpath).unwrap();
    let name_copy = name.clone();
    let raw_ptr = name.into_raw();
    let save_errno = errno::errno();
    let file_handle = rust_make_temp(raw_ptr, 0);
    assert!(file_handle != -1);
    let new_name = unsafe { CString::from_raw(raw_ptr) };
    assert!(new_name != name_copy);
    assert!(save_errno == errno::errno());
}

#[test]
fn test_rust_make_temp_error_inval() {
    let mut tmpdir = env::temp_dir();
    tmpdir.push(".emacs-XxXxx");
    let fullpath = tmpdir.to_string_lossy().into_owned();
    let name = CString::new(fullpath).unwrap();
    let raw_ptr = name.into_raw();
    let file_handle = rust_make_temp(raw_ptr, 0);
    let error = errno::errno();
    assert!(file_handle == -1 && error == errno::Errno(EINVAL));
}
