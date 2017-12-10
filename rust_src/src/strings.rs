//! Functions operating on strings.

use std::ptr;

use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{make_unibyte_string, make_uninit_multibyte_string,
                 string_to_multibyte as c_string_to_multibyte};
use remacs_sys::EmacsInt;

use lisp::LispObject;
use lisp::defsubr;
use multibyte;

pub static MIME_LINE_LENGTH: isize = 76;

/// Return t if OBJECT is a string.
#[lisp_fn]
pub fn stringp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_string())
}

/// Return the number of bytes in STRING.
/// If STRING is multibyte, this may be greater than the length of STRING.
#[lisp_fn]
pub fn string_bytes(string: LispObject) -> LispObject {
    let string = string.as_string_or_error();
    LispObject::from_natnum(string.len_bytes() as EmacsInt)
}

/// Return t if two strings have identical contents.
/// Case is significant, but text properties are ignored.
/// Symbols are also allowed; their print names are used instead.
#[lisp_fn]
pub fn string_equal(s1: LispObject, s2: LispObject) -> LispObject {
    let s1 = LispObject::symbol_or_string_as_string(s1);
    let s2 = LispObject::symbol_or_string_as_string(s2);

    LispObject::from_bool(
        s1.len_chars() == s2.len_chars() && s1.len_bytes() == s2.len_bytes()
            && s1.as_slice() == s2.as_slice(),
    )
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
pub fn string_as_multibyte(string: LispObject) -> LispObject {
    let s = string.as_string_or_error();
    if s.is_multibyte() {
        return string;
    }

    let mut nchars = 0;
    let mut nbytes = 0;
    multibyte::parse_str_as_multibyte(s.const_data_ptr(), s.len_bytes(), &mut nchars, &mut nbytes);

    let new_string = LispObject::from(unsafe {
        make_uninit_multibyte_string(nchars as EmacsInt, nbytes as EmacsInt)
    });

    let mut new_s = new_string.as_string().unwrap();
    unsafe {
        ptr::copy_nonoverlapping(s.const_data_ptr(), new_s.data_ptr(), s.len_bytes() as usize);
    }
    if nbytes != s.len_bytes() {
        multibyte::str_as_multibyte(new_s.data_ptr(), nbytes, s.len_bytes(), ptr::null_mut());
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
pub fn string_to_multibyte(string: LispObject) -> LispObject {
    let _ = string.as_string_or_error();
    unsafe { LispObject::from(c_string_to_multibyte(string.to_raw())) }
}

/// Return a unibyte string with the same individual chars as STRING.
/// If STRING is unibyte, the result is STRING itself.
/// Otherwise it is a newly created string, with no text properties,
/// where each `eight-bit' character is converted to the corresponding byte.
/// If STRING contains a non-ASCII, non-`eight-bit' character,
/// an error is signaled.
#[lisp_fn]
pub fn string_to_unibyte(string: LispObject) -> LispObject {
    let lispstr = string.as_string_or_error();
    if lispstr.is_multibyte() {
        let size = lispstr.len_bytes();
        let mut buffer: Vec<libc::c_uchar> = Vec::with_capacity(size as usize);
        let converted_size =
            multibyte::str_to_unibyte(lispstr.const_data_ptr(), buffer.as_mut_ptr(), size);

        if converted_size < size {
            error!("Can't convert {}th character to unibyte", converted_size);
        }

        let raw_ptr = unsafe { make_unibyte_string(buffer.as_ptr() as *const libc::c_char, size) };
        LispObject::from(raw_ptr)
    } else {
        string
    }
}

/// Return non-nil if STRING1 is less than STRING2 in lexicographic order.
/// Case is significant.
#[lisp_fn]
pub fn string_lessp(string1: LispObject, string2: LispObject) -> LispObject {
    let s1 = LispObject::symbol_or_string_as_string(string1);
    let s2 = LispObject::symbol_or_string_as_string(string2);

    LispObject::from_bool(s1.as_slice() < s2.as_slice())
}

/// Return t if OBJECT is a multibyte string.
/// Return nil if OBJECT is either a unibyte string, or not a string.
#[lisp_fn]
pub fn multibyte_string_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_string().map_or(false, |s| s.is_multibyte()))
}

/// Clear the contents of STRING.
/// This makes STRING unibyte and may change its length.
#[lisp_fn]
pub fn clear_string(mut string: LispObject) -> LispObject {
    let lisp_string = string.as_string_or_error();
    lisp_string.clear_data();
    unsafe {
        lisp_string.set_num_chars(lisp_string.len_bytes());
    }
    LispObject::set_string_unibyte(&mut string);

    LispObject::constant_nil()
}

include!(concat!(env!("OUT_DIR"), "/strings_exports.rs"));

#[test]
fn test_multibyte_stringp() {
    let string = mock_unibyte_string!();
    assert_nil!(multibyte_string_p(string));

    let flt = mock_float!();
    assert_nil!(multibyte_string_p(flt));

    let multi = mock_multibyte_string!();
    assert_t!(multibyte_string_p(multi));
}

#[test]
fn already_unibyte() {
    let single = mock_unibyte_string!();
    assert!(string_to_unibyte(single) == single);
}

#[test]
fn str_equality() {
    let string1 = mock_unibyte_string!("Hello World");
    let string2 = mock_unibyte_string!("Hello World");
    let string3 = mock_unibyte_string!("Goodbye World");
    assert_t!(string_equal(string1, string2));
    assert_t!(string_equal(string2, string1));
    assert_nil!(string_equal(string1, string3));
    assert_nil!(string_equal(string2, string3));
}

#[test]
fn test_stringlessp() {
    let string = mock_unibyte_string!("Hello World");
    let string2 = mock_unibyte_string!("World Hello");
    assert_t!(string_lessp(string, string2));
    assert_nil!(string_lessp(string2, string));
}
