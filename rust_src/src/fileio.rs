//! Functions to deal with files
use errno::{set_errno, Errno};

use std::path;

use remacs_macros::lisp_fn;

use crate::{
    coding::encode_file_name,
    lisp::defsubr,
    lisp::LispObject,
    lists::LispCons,
    math::{arithcompare, ArithComparison},
    multibyte::LispStringRef,
    remacs_sys::{
        check_executable, check_existing, file_name_absolute_p, file_name_case_insensitive_p,
    },
    remacs_sys::{Fexpand_file_name, Ffind_file_name_handler},
    remacs_sys::{Qfile_executable_p, Qfile_exists_p, Qfile_name_case_insensitive_p},
    threads::ThreadState,
};

/// Return t if (car A) is numerically less than (car B).
#[lisp_fn]
pub fn car_less_than_car(a: LispCons, b: LispCons) -> bool {
    arithcompare(a.car(), b.car(), ArithComparison::Less)
}

def_lisp_sym!(Qcar_less_than_car, "car-less-than-car");

/// Return non-nil if NAME ends with a directory separator character.
#[lisp_fn]
pub fn directory_name_p(name: LispStringRef) -> bool {
    if name.len_bytes() == 0 {
        return false;
    }

    let b = name.byte_at(name.len_bytes() - 1);
    b as char == path::MAIN_SEPARATOR
}

/// Clear any record of a recent auto-save failure in the current buffer.
#[lisp_fn]
pub fn clear_buffer_auto_save_failure() {
    ThreadState::current_buffer_unchecked().auto_save_failure_time = 0;
}

/// Return t if current buffer has been auto-saved recently.
/// More precisely, if it has been auto-saved since last read from or saved
/// in the visited file.  If the buffer has no visited file,
/// then any auto-save counts as "recent".
#[lisp_fn]
pub fn recent_auto_save_p() -> bool {
    let cur_buf = ThreadState::current_buffer_unchecked();

    // FIXME: maybe we should return nil for indirect buffers since
    //  they're never autosaved.
    cur_buf.modifications_since_save() < cur_buf.auto_save_modified
}

/// Return t if file FILENAME is on a case-insensitive filesystem.
/// The arg must be a string.
#[lisp_fn(
    name = "file-name-case-insensitive-p",
    c_name = "file_name_case_insensitive_p"
)]
pub fn file_name_case_insensitive_p_lisp(filename: LispStringRef) -> bool {
    let absname = expand_file_name(filename, None);

    // If the file name has special constructs in it,
    // call the corresponding file handler.
    let handler = find_file_name_handler(absname, Qfile_name_case_insensitive_p);
    if handler.is_not_nil() {
        call!(handler, Qfile_name_case_insensitive_p, absname.into()).into()
    } else {
        unsafe {
            file_name_case_insensitive_p(encode_file_name(absname).const_data_ptr() as *const i8)
        }
    }
}

/// Return t if FILENAME is an absolute file name or starts with `~'.
/// On Unix, absolute file names start with `/'.
#[lisp_fn(name = "file-name-absolute-p", c_name = "file_name_absolute_p")]
pub fn file_name_absolute_p_lisp(filename: LispStringRef) -> bool {
    unsafe { file_name_absolute_p(filename.const_data_ptr() as *const i8) }
}

/// Return t if file FILENAME exists (whether or not you can read it.)
/// See also `file-readable-p' and `file-attributes'.
/// This returns nil for a symlink to a nonexistent file.
/// Use `file-symlink-p' to test for such links.
#[lisp_fn]
pub fn file_exists_p(filename: LispStringRef) -> bool {
    let absname = expand_file_name(filename, None);

    // If the file name has special constructs in it,
    // call the corresponding file handler.
    let handler = find_file_name_handler(absname, Qfile_exists_p);

    if handler.is_not_nil() {
        let result = call!(handler, Qfile_exists_p, absname.into());
        set_errno(Errno(0));
        result.into()
    } else {
        unsafe { check_existing(encode_file_name(absname).const_data_ptr() as *const i8) }
    }
}

/// Return t if FILENAME can be executed by you.
/// For a directory, this means you can access files in that directory.
/// (It is generally better to use `file-accessible-directory-p' for that purpose, though.)
#[lisp_fn]
pub fn file_executable_p(filename: LispStringRef) -> bool {
    let absname = expand_file_name(filename, None);

    // If the file name has special constructs in it,
    // call the corresponding file handler.
    let handler = find_file_name_handler(absname, Qfile_executable_p);

    if handler.is_not_nil() {
        call!(handler, Qfile_executable_p, absname.into()).into()
    } else {
        unsafe { check_executable(encode_file_name(absname).data_ptr() as *mut i8) }
    }
}

/// Wrapper for Fexpand_file_name (NOT PORTED)
pub fn expand_file_name(
    name: LispStringRef,
    default_directory: Option<LispStringRef>,
) -> LispStringRef {
    unsafe { Fexpand_file_name(name.into(), default_directory.into()) }.into()
}

/// Wrapper for Ffind_file_name_handler (NOT PORTED)
pub fn find_file_name_handler(filename: LispStringRef, operation: LispObject) -> LispObject {
    unsafe { Ffind_file_name_handler(filename.into(), operation) }
}

include!(concat!(env!("OUT_DIR"), "/fileio_exports.rs"));
