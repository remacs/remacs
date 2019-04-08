//! Functions operating on strings.

use std::ptr;

use libc;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    multibyte,
    multibyte::{LispStringRef, LispSymbolOrString},
    remacs_sys::EmacsInt,
    remacs_sys::{
        make_unibyte_string, make_uninit_multibyte_string,
        string_to_multibyte as c_string_to_multibyte,
    },
};

/// Return t if OBJECT is a string.
#[lisp_fn]
pub fn stringp(object: LispObject) -> bool {
    object.is_string()
}

/// Return the number of bytes in STRING.
/// If STRING is multibyte, this may be greater than the length of STRING.
#[lisp_fn]
pub fn string_bytes(string: LispStringRef) -> EmacsInt {
    string.len_bytes() as EmacsInt
}

pub fn string_equal(s1: impl Into<LispSymbolOrString>, s2: impl Into<LispSymbolOrString>) -> bool {
    let s1 = LispStringRef::from(s1.into());
    let s2 = LispStringRef::from(s2.into());

    s1.len_chars() == s2.len_chars()
        && s1.len_bytes() == s2.len_bytes()
        && s1.as_slice() == s2.as_slice()
}

/// Return t if two strings have identical contents.
/// Case is significant, but text properties are ignored.
/// Symbols are also allowed; their print names are used instead.
#[lisp_fn(name = "string-equal", c_name = "string_equal")]
pub fn string_equal_lisp(s1: LispSymbolOrString, s2: LispSymbolOrString) -> bool {
    string_equal(s1, s2)
}

/// Return a multibyte string with the same individual bytes as STRING.
/// If STRING is multibyte, the result is STRING itself.
/// Otherwise it is a newly created string, with no text properties.
///
/// If STRING is unibyte and contains an individual 8-bit byte (i.e. not
/// part of a correct utf-8 sequence), it is converted to the corresponding
/// multibyte character of charset `eight-bit'.
/// See also `string-to-multibyte'.
///
/// Beware, this often doesn't really do what you think it does.
/// It is similar to (decode-coding-string STRING \\='utf-8-emacs).
/// If you're not sure, whether to use `string-as-multibyte' or
/// `string-to-multibyte', use `string-to-multibyte'.
#[lisp_fn]
pub fn string_as_multibyte(string: LispStringRef) -> LispObject {
    if string.is_multibyte() {
        return string.into();
    }

    let mut nchars = 0;
    let mut nbytes = 0;
    unsafe {
        multibyte::parse_str_as_multibyte(
            string.const_data_ptr(),
            string.len_bytes(),
            &mut nchars,
            &mut nbytes,
        )
    };

    let new_string =
        unsafe { make_uninit_multibyte_string(nchars as EmacsInt, nbytes as EmacsInt) };
    let mut new_s = new_string.as_string().unwrap();
    unsafe {
        ptr::copy_nonoverlapping(
            string.const_data_ptr(),
            new_s.data_ptr(),
            string.len_bytes() as usize,
        );
    }
    if nbytes != string.len_bytes() {
        unsafe {
            multibyte::str_as_multibyte(
                new_s.data_ptr(),
                nbytes,
                string.len_bytes(),
                ptr::null_mut(),
            )
        };
    }
    new_string
}

/// Return a multibyte string with the same individual chars as STRING.
/// If STRING is multibyte, the result is STRING itself.
/// Otherwise it is a newly created string, with no text properties.
///
/// If STRING is unibyte and contains an 8-bit byte, it is converted to
/// the corresponding multibyte character of charset `eight-bit'.
///
/// This differs from `string-as-multibyte' by converting each byte of a correct
/// utf-8 sequence to an eight-bit character, not just bytes that don't form a
/// correct sequence.
#[lisp_fn]
pub fn string_to_multibyte(string: LispStringRef) -> LispObject {
    unsafe { c_string_to_multibyte(string.into()) }
}

/// Return a unibyte string with the same individual chars as STRING.
/// If STRING is unibyte, the result is STRING itself.
/// Otherwise it is a newly created string, with no text properties,
/// where each `eight-bit' character is converted to the corresponding byte.
/// If STRING contains a non-ASCII, non-`eight-bit' character,
/// an error is signaled.
#[lisp_fn]
pub fn string_to_unibyte(string: LispStringRef) -> LispObject {
    if string.is_multibyte() {
        let size = string.len_bytes();
        let mut buffer: Vec<libc::c_uchar> = Vec::with_capacity(size as usize);
        let converted_size = unsafe {
            multibyte::str_to_unibyte(string.const_data_ptr(), buffer.as_mut_ptr(), size)
        };

        if converted_size < size {
            error!("Can't convert {}th character to unibyte", converted_size);
        }

        unsafe { make_unibyte_string(buffer.as_ptr() as *const libc::c_char, size) }
    } else {
        string.into()
    }
}

pub fn string_lessp(
    string1: impl Into<LispSymbolOrString>,
    string2: impl Into<LispSymbolOrString>,
) -> bool {
    let s1 = LispStringRef::from(string1.into());
    let s2 = LispStringRef::from(string2.into());

    s1.as_slice() < s2.as_slice()
}

/// Return non-nil if STRING1 is less than STRING2 in lexicographic order.
/// Case is significant.
#[lisp_fn(name = "string-lessp", c_name = "string_lessp")]
pub fn string_lessp_lisp(string1: LispSymbolOrString, string2: LispSymbolOrString) -> bool {
    string_lessp(string1, string2)
}

/// Return t if OBJECT is a multibyte string.
/// Return nil if OBJECT is either a unibyte string, or not a string.
#[lisp_fn]
pub fn multibyte_string_p(object: LispObject) -> bool {
    object
        .as_string()
        .map_or(false, LispStringRef::is_multibyte)
}

/// Clear the contents of STRING.
/// This makes STRING unibyte and may change its length.
#[lisp_fn]
pub fn clear_string(mut string: LispStringRef) {
    string.clear_data();
    unsafe {
        string.set_num_chars(string.len_bytes());
    }
    string.mark_as_unibyte();
}

/// Return width of STRING when displayed in the current buffer. Width is
/// measured by how many columns it occupies on the screen. When calculating
/// width of a multibyte character in STRING, only the base leading-code is
/// considered; the validity of the following bytes is not checked.  Tabs in
/// STRING are always taken to occupy `tab-width' columns.
#[lisp_fn]
pub fn string_width(string: LispStringRef) -> usize {
    string.width()
}

include!(concat!(env!("OUT_DIR"), "/strings_exports.rs"));

#[test]
fn test_multibyte_stringp() {
    let string = mock_unibyte_string!();
    assert!(!multibyte_string_p(string));

    let flt = mock_float!();
    assert!(!multibyte_string_p(flt));

    let multi = mock_multibyte_string!();
    assert!(multibyte_string_p(multi));
}

#[test]
fn already_unibyte() {
    let single = mock_unibyte_string!();
    assert!(string_to_unibyte(LispStringRef::from(single)) == single);
}

#[test]
fn str_equality() {
    let string1 = mock_unibyte_string!("Hello World");
    let string2 = mock_unibyte_string!("Hello World");
    let string3 = mock_unibyte_string!("Goodbye World");
    assert!(string_equal(string1, string2));
    assert!(string_equal(string2, string1));
    assert!(!string_equal(string1, string3));
    assert!(!string_equal(string2, string3));
}

#[test]
fn test_stringlessp() {
    let string = mock_unibyte_string!("Hello World");
    let string2 = mock_unibyte_string!("World Hello");
    assert!(string_lessp(string, string2));
    assert!(!string_lessp(string2, string));
}
