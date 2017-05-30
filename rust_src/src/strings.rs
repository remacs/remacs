//! Functions operating on strings.

use std::ptr;

use libc;

use lisp::LispObject;
use remacs_sys::{SYMBOL_NAME, EmacsInt, error, base64_encode_1, base64_decode_1, make_string};
use remacs_macros::lisp_fn;

pub static MIME_LINE_LENGTH: isize = 76;

/// Return t if OBJECT is a string.
/// (fn OBJECT)
#[lisp_fn]
fn stringp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_string())
}

/// Base64-encode STRING and return the result.
/// Optional second argument NO-LINE-BREAK means do not break long lines
/// into shorter lines.
/// (fn STRING &optional NO-LINE-BREAK)
#[lisp_fn(min = "1")]
fn base64_encode_string(string: LispObject, no_line_break: LispObject) -> LispObject {
    let mut string = string.as_string_or_error();

    // We need to allocate enough room for the encoded text
    // We will need 33 1/3% more space, plus a newline every 76 characters(MIME_LINE_LENGTH)
    // and then round up
    let length = string.len_bytes();
    let mut allength: libc::ptrdiff_t = length + length / 3 + 1;
    allength += allength / MIME_LINE_LENGTH + 1 + 6;

    // This function uses SAFE_ALLOCA in the c layer, however I cannot find an equivalent
    // for rust. Instead, we will use a Vec to store the temporary char buffer.
    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(allength as usize);
    unsafe {
        let encoded = buffer.as_mut_ptr();
        let encodedLength = base64_encode_1(string.data_ptr(),
                                            encoded,
                                            length,
                                            no_line_break.is_nil(),
                                            string.is_multibyte());

        if encodedLength > allength {
            panic!("base64 encoded length is larger then allocated buffer");
        }

        if encodedLength < 0 {
            error("Multibyte character in data for base64 encoding\0".as_ptr());
        }

        LispObject::from_raw(make_string(encoded, encodedLength))
    }
}

/// Base64-decode STRING and return the result.
/// (fn STRING)
#[lisp_fn]
fn base64_decode_string(string: LispObject) -> LispObject {
    let mut string = string.as_string_or_error();

    let length = string.len_bytes();
    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(length as usize);
    let mut decoded_string: LispObject = LispObject::constant_nil();

    unsafe {
        let decoded = buffer.as_mut_ptr();
        let decoded_length =
            base64_decode_1(string.data_ptr(), decoded, length, false, ptr::null_mut());

        if decoded_length > length {
            panic!("Decoded length is above length");
        } else if decoded_length >= 0 {
            decoded_string = LispObject::from_raw(make_string(decoded, decoded_length));
        }

        if !decoded_string.is_string() {
            error("Invalid base64 data\0".as_ptr());
        }

        decoded_string
    }
}

/// Return the number of bytes in STRING.
/// If STRING is multibyte, this may be greater than the length of STRING.
/// (fn STRING)
#[lisp_fn]
fn string_bytes(string: LispObject) -> LispObject {
    let string = string.as_string_or_error();
    LispObject::from_natnum(string.len_bytes() as EmacsInt)
}

/// Return t if two strings have identical contents.
/// Case is significant, but text properties are ignored.
/// Symbols are also allowed; their print names are used instead.
/// (fn S1 S2)
#[lisp_fn]
fn string_equal(mut s1: LispObject, mut s2: LispObject) -> LispObject {
    if s1.is_symbol() {
        s1 = LispObject::from_raw(unsafe { SYMBOL_NAME(s1.to_raw()) });
    }
    if s2.is_symbol() {
        s2 = LispObject::from_raw(unsafe { SYMBOL_NAME(s2.to_raw()) });
    }
    let mut s1 = s1.as_string_or_error();
    let mut s2 = s2.as_string_or_error();

    LispObject::from_bool(s1.len_chars() == s2.len_chars() && s1.len_bytes() == s2.len_bytes() &&
                          unsafe {
                              libc::memcmp(s1.data_ptr() as *mut libc::c_void,
                                           s2.data_ptr() as *mut libc::c_void,
                                           s1.len_bytes() as usize) ==
                              0
                          })
}
