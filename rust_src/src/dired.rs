//! Lisp functions for making directory listings.

use remacs_macros::lisp_fn;

#[cfg(unix)]
use crate::dired_unix::{
    directory_files_and_attributes_intro, directory_files_intro, file_attributes_intro, get_users,
};
#[cfg(windows)]
use dired_windows::{file_attributes_intro, get_users};

use crate::{lisp::LispObject, lists::car, multibyte::LispStringRef, strings::string_lessp};

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
    directory: LispStringRef,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
) -> LispObject {
    directory_files_intro(directory, full, match_re, nosort)
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
    directory: LispStringRef,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
    id_format: LispObject,
) -> LispObject {
    directory_files_and_attributes_intro(directory, full, match_re, nosort, id_format)
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
pub fn file_attributes(filename: LispStringRef, id_format: LispObject) -> LispObject {
    file_attributes_intro(filename, id_format)
}

/// Return t if first arg file attributes list is less than second.
/// Comparison is in lexicographic order and case is significant.
#[lisp_fn]
pub fn file_attributes_lessp(f1: LispObject, f2: LispObject) -> bool {
    string_lessp(car(f1), car(f2))
}

/// Return a list of user names currently registered in the system.
/// If we don't know how to determine that on this platform, just
/// return a list with one element, taken from `user-real-login-name'.
#[lisp_fn]
pub fn system_users() -> LispObject {
    get_users()
}

include!(concat!(env!("OUT_DIR"), "/dired_exports.rs"));
