//! Lisp functions for making directory listings.

use std::io;
use std::ptr;
use std::ffi::CStr;
use std::cmp::Ordering;

use std::path::Path;
use std::fs;
use std::os::unix::fs::MetadataExt;

use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{Fexpand_file_name, Qfile_missing, Qnil, compile_pattern, re_search,
                 re_pattern_buffer, filemode_string, Ffind_file_name_handler, Qfile_attributes,
                 Qdirectory_files, Qdirectory_files_and_attributes, decode_file_name};

use lisp::defsubr;
use lisp::LispObject;
use lists::{list, car};
use symbols::symbol_name;

// We keep both a name string and LispObject object because
// on osx type conversions (which generate alloc:: calls) in the
// sort predicate fn core dumped on largeish (500ish) directeries.
struct DirEntry {
    name_string: String,
    name_and_attrs: LispObject,
}

macro_rules! de_pushnamesandattrs {
    ($entries:expr, $fname_string:expr, $fname:expr,
     $abpath:expr, $attrs:expr, $idformat:expr) => {{
        let name_and_attrs;
        if $attrs {
            let fileattrs = file_attributes_internal($abpath,
                                                     $idformat);
            name_and_attrs =  LispObject::cons($fname, fileattrs);
        } else {
            name_and_attrs = $fname;
        }
        $entries.push(DirEntry {
            name_string: $fname_string,
	    name_and_attrs: name_and_attrs });
    }};
}

fn get_entries(
    directory: LispObject,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
    attrs: bool,
    id_format: LispObject,
) -> io::Result<LispObject> {

    let dirS0 = String::from(directory.to_string());
    let dirS1 = dirS0.clone();
    let dirP = Path::new(&dirS0);
    let slash = String::from("/");

    if !dirP.is_dir() {
        xsignal!(
            Qfile_missing,
            LispObject::from_str("Opening directory: no such file or directory"),
            directory
        );
    }

    let mut recomp = ptr::null_mut();
    if match_re.is_not_nil() {
        recomp = unsafe { compile_pattern(match_re.to_raw(), ptr::null_mut(), Qnil, false, true) };
    }
    let mut entries = Vec::new();
    for entry in fs::read_dir(dirP)? {
        let entry = entry?;
        let fname_string_enc = entry.file_name().into_string().unwrap();
        let fname_lo_enc = LispObject::from_str(&fname_string_enc);
        let fname_lo_raw = unsafe { decode_file_name(fname_lo_enc.to_raw()) };
        let fname_string = LispObject::from(fname_lo_raw).to_string();
        let fname_string2 = fname_string.clone();
        let abpath_str = dirS1.clone() + &slash + &fname_string;
        let abpath = LispObject::from_str(&abpath_str);
        if match_re.is_not_nil() {
            let fname_str = &*fname_string;
            if c_regex_matchp(recomp, fname_str) {
                if full.is_not_nil() {
                    de_pushnamesandattrs!(entries, fname_string2, abpath, abpath, attrs, id_format);
                } else {
                    de_pushnamesandattrs!(
                        entries,
                        fname_string2,
                        LispObject::from_str(&fname_string),
                        abpath,
                        attrs,
                        id_format
                    );
                }
            }
        } else {
            if full.is_not_nil() {
                de_pushnamesandattrs!(entries, fname_string, abpath, abpath, attrs, id_format);
            } else {
                de_pushnamesandattrs!(
                    entries,
                    fname_string,
                    LispObject::from_str(&fname_string),
                    abpath,
                    attrs,
                    id_format
                );
            }
        }
    }

    // read_dir() does not return .&.. so bolt them on
    let mut dirdots = Vec::new();
    if match_re.is_not_nil() {
        if c_regex_matchp(recomp, "..") {
            dirdots.push(String::from(".."));
        }
        if c_regex_matchp(recomp, ".") {
            dirdots.push(String::from("."));
        }
    } else {
        dirdots.push(String::from(".."));
        dirdots.push(String::from("."));
    }

    for dir in dirdots {
        let abpath_string = dirS1.clone() + &slash + &dir.clone();
        let abpath = LispObject::from_str(&abpath_string);
        if full.is_not_nil() {
            de_pushnamesandattrs!(entries, abpath_string, abpath, abpath, attrs, id_format);
        } else {
            de_pushnamesandattrs!(
                entries,
                dir,
                LispObject::from_str(&dir),
                abpath,
                attrs,
                id_format
            );
        }
    }

    if nosort.is_nil() {
        entries.sort_by(|a, b| if a.name_string < b.name_string {
            Ordering::Less
        } else {
            Ordering::Greater
        });
    }

    // stripoff name_string
    let mut ents = Vec::new();
    for ent in entries {
        ents.push(ent.name_and_attrs);
    }

    Ok(list(&mut ents))
}

// Function shared by Fdirectory_files and Fdirectory_files_and_attributes.
// If not ATTRS, return a list of directory filenames;
// if ATTRS, return a list of directory filenames and their attributes.
// In the latter case, ID_FORMAT is passed to Ffile_attributes.

#[no_mangle]
pub extern "C" fn directory_files_internal(
    directory: LispObject,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
    attrs: bool,
    id_format: LispObject,
) -> LispObject {

    //match get_files(directory, full, match_re, nosort, attrs, id_format) {
    match get_entries(directory, full, match_re, nosort, attrs, id_format) {
        Ok(files) => files,
        Err(e) => {
            error!("directory-files fail: {}", e);
        }
    }
}

/// Return a list of names of files in DIRECTORY.
/// There are three optional arguments:
/// If FULL is non-nil, return absolute file names.  Otherwise return names
///  that are relative to the specified directory.
/// If MATCH is non-nil, mention only file names that match the regexp MATCH.
/// If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
///  Otherwise, the list returned is sorted with `string-lessp'.
///  NOSORT is useful if you plan to sort the result yourself.
#[lisp_fn(min = "1")]
pub fn directory_files(
    directory: LispObject,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
) -> LispObject {

    let dnexp_raw = unsafe { Fexpand_file_name(directory.to_raw(), Qnil) };
    let dnexp = LispObject::from(dnexp_raw);

    let handler_raw = unsafe { Ffind_file_name_handler(dnexp.to_raw(), Qdirectory_files) };
    let handler = LispObject::from(handler_raw);
    if handler.is_not_nil() {
        return call!(
            handler,
            LispObject::from(Qdirectory_files),
            dnexp,
            full,
            match_re,
            nosort
        );
    }

    directory_files_internal(
        dnexp,
        full,
        match_re,
        nosort,
        false,
        LispObject::constant_nil(),
    )
}

/// Return a list of names of files and their attributes in DIRECTORY.
/// There are four optional arguments:
/// If FULL is non-nil, return absolute file names.  Otherwise return names
///  that are relative to the specified directory.
/// If MATCH is non-nil, mention only file names that match the regexp MATCH.
/// If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
///  NOSORT is useful if you plan to sort the result yourself.
/// ID-FORMAT specifies the preferred format of attributes uid and gid, see
/// `file-attributes' for further documentation.
/// On MS-Windows, performance depends on `w32-get-true-file-attributes',
/// which see.
#[lisp_fn(min = "1")]
pub fn directory_files_and_attributes(
    directory: LispObject,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
    id_format: LispObject,
) -> LispObject {

    let dnexp_raw = unsafe { Fexpand_file_name(directory.to_raw(), Qnil) };
    let dnexp = LispObject::from(dnexp_raw);

    let handler_raw =
        unsafe { Ffind_file_name_handler(dnexp.to_raw(), Qdirectory_files_and_attributes) };
    let handler = LispObject::from(handler_raw);
    if handler.is_not_nil() {
        return call!(
            handler,
            LispObject::from(Qdirectory_files_and_attributes),
            dnexp,
            full,
            match_re,
            nosort,
            id_format
        );
    }

    directory_files_internal(dnexp, full, match_re, nosort, true, id_format)
}

const LO_TIME_BITS: i32 = 16;

fn hi_time(t: i64) -> i64 {
    let ovfl = t.overflowing_shr(LO_TIME_BITS as u32);
    if ovfl.1 {
        error!("hi_time: overflowing_shl")
    }

    t >> LO_TIME_BITS
}

fn lo_time(t: i64) -> i32 {
    let ti32 = t as i32;
    ti32 & ((1 << LO_TIME_BITS) - 1)
}

fn file_attributes_internal(abpath: LispObject, id_format: LispObject) -> LispObject {
    let mut attrs = Vec::new();
    let abpath_string = abpath.to_string();
    let abpath_str = &*abpath_string;

    let md_res = fs::metadata(abpath_str);
    if md_res.is_err() {
        // ENOENT
        return LispObject::constant_nil();
    }
    let md = md_res.unwrap();

    // file_type
    let ft = md.file_type();
    // ft.is_symlink() inexplicably always -> false on Linux (ditto Python).
    if let Ok(symmemaybe) = fs::read_link(abpath_str) {
        attrs.push(LispObject::from_str(symmemaybe.to_str().unwrap()));
    } else {
        if ft.is_dir() {
            attrs.push(LispObject::constant_t());
        } else {
            attrs.push(LispObject::constant_nil());
        }
    }

    // number of links to file
    attrs.push(LispObject::from_natnum(md.nlink() as i64));

    // user id num & group id num
    let mut idf_is_int = true;
    if id_format.is_not_nil() {
        let idf = symbol_name(id_format);
        let idf_string = idf.to_string().to_lowercase();
        idf_is_int = !("string".to_owned() == idf_string);
    }

    if idf_is_int {
        attrs.push(LispObject::from_natnum(i64::from(md.uid())));
        attrs.push(LispObject::from_natnum(i64::from(md.gid())));
    } else {
        let pw: *mut libc::passwd = unsafe { libc::getpwuid(md.uid()) };
        let pwnam_string = unsafe { CStr::from_ptr((*pw).pw_name).to_string_lossy().into_owned() };
        let pwnam_str = &*pwnam_string;
        attrs.push(LispObject::from_str(pwnam_str));

        let gr: *mut libc::group = unsafe { libc::getgrgid(md.gid()) };
        let grnam_string = unsafe { CStr::from_ptr((*gr).gr_name).to_string_lossy().into_owned() };
        let grnam_str = &*grnam_string;
        attrs.push(LispObject::from_str(grnam_str));
    }

    // access time&modded time&creat time
    fn make_time_list(s: i64, ns: i64) -> LispObject {
        let mut atl = Vec::new();
        let ht = hi_time(s);
        atl.push(LispObject::from_natnum(ht));
        let lt = lo_time(s);
        atl.push(LispObject::from_natnum(lt as i64));
        let usec = ns / 1000;
        atl.push(LispObject::from_natnum(usec));
        let psec = ns % 1000 * 1000;
        atl.push(LispObject::from_natnum(psec));
        list(&mut atl)
    }

    attrs.push(make_time_list(md.atime(), md.atime_nsec()));
    attrs.push(make_time_list(md.mtime(), md.mtime_nsec()));
    attrs.push(make_time_list(md.ctime(), md.ctime_nsec()));

    // file size in bytes, fixnum or float.
    attrs.push(LispObject::from_natnum(md.size() as i64));

    // file modes string of 1- letters aka ls -l
    let modestr_raw = unsafe { filemode_string(abpath.to_raw()) };
    attrs.push(LispObject::from(modestr_raw));

    // an unspecified value, present only for backward compatibility.
    attrs.push(LispObject::constant_t());

    // inode & device
    attrs.push(LispObject::from_natnum(md.ino() as i64));
    attrs.push(LispObject::from_natnum(md.dev() as i64));

    list(&mut attrs)
}

/// Return a list of attributes of file FILENAME.
/// Value is nil if specified file cannot be opened.
///
/// ID-FORMAT specifies the preferred format of attributes uid and gid (see
/// below) - valid values are `string' and `integer'.  The latter is the
/// default, but we plan to change that, so you should specify a non-nil value
/// for ID-FORMAT if you use the returned uid or gid.
///
/// To access the elements returned, the following access functions are
/// provided: `file-attribute-type', `file-attribute-link-number',
/// `file-attribute-user-id', `file-attribute-group-id',
/// `file-attribute-access-time', `file-attribute-modification-time',
/// `file-attribute-status-change-time', `file-attribute-size',
/// `file-attribute-modes', `file-attribute-inode-number', and
/// `file-attribute-device-number'.
///
/// Elements of the attribute list are:
///  0. t for directory, string (name linked to) for symbolic link, or nil.
///  1. Number of links to file.
///  2. File uid as a string or a number.  If a string value cannot be
///   looked up, a numeric value, either an integer or a float, is returned.
///  3. File gid, likewise.
///  4. Last access time, as a list of integers (HIGH LOW USEC PSEC) in the
///   same style as (current-time).
///   (See a note below about access time on FAT-based filesystems.)
///  5. Last modification time, likewise.  This is the time of the last
///   change to the file's contents.
///  6. Last status change time, likewise.  This is the time of last change
///   to the file's attributes: owner and group, access mode bits, etc.
///  7. Size in bytes.
///   This is a floating point number if the size is too large for an integer.
///  8. File modes, as a string of ten letters or dashes as in ls -l.
///  9. An unspecified value, present only for backward compatibility.
/// 10. inode number.  If it is larger than what an Emacs integer can hold,
///   this is of the form (HIGH . LOW): first the high bits, then the low 16 bits.
///   If even HIGH is too large for an Emacs integer, this is instead of the form
///   (HIGH MIDDLE . LOW): first the high bits, then the middle 24 bits,
///   and finally the low 16 bits.
/// 11. Filesystem device number.  If it is larger than what the Emacs
///   integer can hold, this is a cons cell, similar to the inode number.
///
/// On most filesystems, the combination of the inode and the device
/// number uniquely identifies the file.
///
/// On MS-Windows, performance depends on `w32-get-true-file-attributes',
/// which see.
///
/// On some FAT-based filesystems, only the date of last access is recorded,
/// so last access time will always be midnight of that day.
#[lisp_fn(min = "1")]
pub fn file_attributes(filename: LispObject, id_format: LispObject) -> LispObject {
    let fnexp_raw = unsafe { Fexpand_file_name(filename.to_raw(), Qnil) };
    let fnexp = LispObject::from(fnexp_raw);

    let handler_raw = unsafe { Ffind_file_name_handler(fnexp_raw, Qfile_attributes) };
    let handler = LispObject::from(handler_raw);
    if handler.is_not_nil() {
        if id_format.is_not_nil() {
            return call!(
                handler,
                LispObject::from(Qfile_attributes),
                fnexp,
                id_format
            );
        } else {
            return call!(
                handler,
                LispObject::from(Qfile_attributes),
                fnexp
            );
        }
    }

    file_attributes_internal(fnexp, id_format)
}

/// Return t if first arg file attributes list is less than second.
/// Comparison is in lexicographic order and case is significant.  */)
#[lisp_fn]
pub fn file_attributes_lessp(f1: LispObject, f2: LispObject) -> LispObject {
    LispObject::from_bool(car(f1).to_string() < car(f2).to_string())
}

/// Return a list of user names currently registered in the system.
/// If we don't know how to determine that on this platform, just
/// return a list with one element, taken from `user-real-login-name'.  */)
#[lisp_fn]
pub fn system_users() -> LispObject {
    let mut done = false;
    let mut unames = Vec::new();

    while !done {
        let pw: *mut libc::passwd = unsafe { libc::getpwent() };
        if pw.is_null() {
            done = true;
        } else {
            let pwnam_string =
                unsafe { CStr::from_ptr((*pw).pw_name).to_string_lossy().into_owned() };
            let pwnam_str = &*pwnam_string;
            unames.push(LispObject::from_str(pwnam_str));
        }
    }
    unsafe { libc::endpwent() };

    if unames.len() < 1 {
        unames.push(LispObject::from_str("rms"));
    }

    list(&mut unames)
}

fn c_regex_matchp(recomp: *mut re_pattern_buffer, s: &str) -> bool {
    unsafe {
        re_search(
            recomp,
            s.as_ptr() as *const libc::c_char,
            s.len() as libc::size_t,
            0,
            s.len() as libc::ssize_t,
            ptr::null_mut(),
        ) >= 0
    }
}

// QandD recipe to test unicode multibyte file name (Linux):
//
// See http://ergoemacs.org/emacs/emacs_n_unicode.html (props to Xah for this info)
// and set-language-environment and set-default-coding-systems like so:
//
//   (set-language-environment "UTF-8")
//   (set-default-coding-systems 'utf-8)
//
// And pick a unicode char and define-key it.
// For the right arrow example on that page: <f9> <right>
//
// Then in shell mode: touch /tmp/foo<f9> <right>
//
// Then test new rust code:
//   (directory-files "/tmp" nil "foo<f9> <right>")
// and eyeball the result as it should match the file you created.

include!(concat!(env!("OUT_DIR"), "/dired_exports.rs"));
