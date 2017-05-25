use std::ptr;

use libc;

use lisp::{LispObject, SBYTES, SCHARS, CHECK_STRING};
use remacs_sys::{Lisp_Object, SDATA, SSDATA, STRING_MULTIBYTE, SYMBOL_NAME};
use remacs_macros::lisp_fn;

extern "C" {
    fn make_string(s: *const libc::c_char, length: libc::ptrdiff_t) -> Lisp_Object;
    fn base64_encode_1(from: *const libc::c_char,
                       to: *mut libc::c_char,
                       length: libc::ptrdiff_t,
                       line_break: bool,
                       multibyte: bool)
                       -> libc::ptrdiff_t;
    fn base64_decode_1(from: *const libc::c_char,
                       to: *mut libc::c_char,
                       length: libc::ptrdiff_t,
                       multibyte: bool,
                       nchars_return: *mut libc::ptrdiff_t)
                       -> libc::ptrdiff_t;
    fn error(m: *const u8, ...);
}

pub static MIME_LINE_LENGTH: isize = 76;

/// Return t if OBJECT is a string.
/// (fn OBJECT)
#[lisp_fn(name = "stringp", min = "1")]
fn stringp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_string())
}

#[lisp_fn(name = "base64-encode-string", min = "1")]
fn base64_encode_string(string: LispObject, noLineBreak: LispObject) -> LispObject {
    CHECK_STRING(string.to_raw());

    // We need to allocate enough room for the encoded text
    // We will need 33 1/3% more space, plus a newline every 76 characters(MIME_LINE_LENGTH)
    // and then round up
    let length = SBYTES(string);
    let mut allength: libc::ptrdiff_t = length + length / 3 + 1;
    allength += allength / MIME_LINE_LENGTH + 1 + 6;

    // This function uses SAFE_ALLOCA in the c layer, however I cannot find an equivalent
    // for rust. Instead, we will use a Vec to store the temporary char buffer.
    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(allength as usize);
    unsafe {
        let encoded = buffer.as_mut_ptr();
        let encodedLength = base64_encode_1(SSDATA(string.to_raw()),
                                            encoded,
                                            length,
                                            noLineBreak.is_nil(),
                                            STRING_MULTIBYTE(string.to_raw()));

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
#[lisp_fn(name = "base64-decode-string", min = "1")]
fn base64_decode_string(string: LispObject) -> LispObject {
    CHECK_STRING(string.to_raw());

    let length = SBYTES(string);
    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(length as usize);
    let mut decoded_string: LispObject = LispObject::constant_nil();

    unsafe {
        let decoded = buffer.as_mut_ptr();
        let decoded_length = base64_decode_1(SSDATA(string.to_raw()),
                                             decoded,
                                             length,
                                             false,
                                             ptr::null_mut());

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
#[lisp_fn(name = "string-bytes", min = "1")]
fn string_bytes(string: LispObject) -> LispObject {
    CHECK_STRING(string.to_raw());
    unsafe { LispObject::from_fixnum_unchecked(SBYTES(string) as ::remacs_sys::EmacsInt) }
}

fn string_equal(mut s1: LispObject, mut s2: LispObject) -> LispObject {
    if s1.is_symbol() {
        s1 = LispObject::from_raw(unsafe { SYMBOL_NAME(s1.to_raw()) });
    }
    if s2.is_symbol() {
        s2 = LispObject::from_raw(unsafe { SYMBOL_NAME(s2.to_raw()) });
    }
    CHECK_STRING(s1.to_raw());
    CHECK_STRING(s2.to_raw());

    LispObject::from_bool(SCHARS(s1) == SCHARS(s2) && SBYTES(s1) == SBYTES(s2) &&
                          unsafe {
                              libc::memcmp(SDATA(s1.to_raw()) as *mut libc::c_void,
                                           SDATA(s2.to_raw()) as *mut libc::c_void,
                                           SBYTES(s1) as usize) == 0
                          })
}

defun!("string-equal",
       Fstring_equal(s1, s2),
       Sstring_equal,
       string_equal,
       2,
       2,
       ptr::null(),
       "Return t if two strings have identical contents.
Case is significant, but text properties are ignored.
Symbols are also allowed; their print names are used instead.

(fn S1 S2)");
