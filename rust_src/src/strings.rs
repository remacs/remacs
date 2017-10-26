//! Functions operating on strings.

use std::ptr;

use libc::{self, c_void};

use lisp::LispObject;
use multibyte;
use remacs_sys::{SYMBOL_NAME, EmacsInt, make_unibyte_string, make_uninit_multibyte_string,
                 string_to_multibyte as c_string_to_multibyte};
use remacs_macros::lisp_fn;

pub static MIME_LINE_LENGTH: isize = 76;

/// Return t if OBJECT is a string.
#[lisp_fn]
pub fn stringp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_string())
}

/// Return the number of bytes in STRING.
/// If STRING is multibyte, this may be greater than the length of STRING.
#[lisp_fn]
fn string_bytes(string: LispObject) -> LispObject {
    let string = string.as_string_or_error();
    LispObject::from_natnum(string.len_bytes() as EmacsInt)
}

/// Return t if two strings have identical contents.
/// Case is significant, but text properties are ignored.
/// Symbols are also allowed; their print names are used instead.
#[lisp_fn]
pub fn string_equal(mut s1: LispObject, mut s2: LispObject) -> LispObject {
    if s1.is_symbol() {
        s1 = LispObject::from(unsafe { SYMBOL_NAME(s1.to_raw()) });
    }
    if s2.is_symbol() {
        s2 = LispObject::from(unsafe { SYMBOL_NAME(s2.to_raw()) });
    }
    let mut s1 = s1.as_string_or_error();
    let mut s2 = s2.as_string_or_error();

    LispObject::from_bool(
        s1.len_chars() == s2.len_chars() && s1.len_bytes() == s2.len_bytes() &&
            unsafe {
                libc::memcmp(
                    s1.data_ptr() as *mut c_void,
                    s2.data_ptr() as *mut c_void,
                    s1.len_bytes() as usize,
                ) == 0
            },
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
fn string_as_multibyte(string: LispObject) -> LispObject {
    let mut s = string.as_string_or_error();
    if s.is_multibyte() {
        return string;
    }
    let mut nchars = 0;
    let mut nbytes = 0;
    multibyte::parse_str_as_multibyte(s.data_ptr(), s.len_bytes(), &mut nchars, &mut nbytes);
    let new_string =
        unsafe { make_uninit_multibyte_string(nchars as EmacsInt, nbytes as EmacsInt) };
    let new_string = LispObject::from(new_string);
    let mut new_s = new_string.as_string().unwrap();
    unsafe {
        ptr::copy_nonoverlapping(s.data_ptr(), new_s.data_ptr(), s.len_bytes() as usize);
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
fn string_to_multibyte(string: LispObject) -> LispObject {
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
fn string_to_unibyte(string: LispObject) -> LispObject {
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
fn string_lessp(string1: LispObject, string2: LispObject) -> LispObject {
    let lispstr1 = LispObject::symbol_or_string_as_string(string1);
    let lispstr2 = LispObject::symbol_or_string_as_string(string2);

    let zip = lispstr1.chars().zip(lispstr2.chars());
    for (codept1, codept2) in zip {
        if codept1 != codept2 {
            return LispObject::from_bool(codept1 < codept2);
        }
    }

    LispObject::from_bool(lispstr1.len_chars() < lispstr2.len_chars())
}

/// Return t if OBJECT is a multibyte string.
/// Return nil if OBJECT is either a unibyte string, or not a string.
#[lisp_fn]
fn multibyte_string_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_string().map_or(false, |s| s.is_multibyte()))
}
