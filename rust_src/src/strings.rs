//! Functions operating on strings.

use std::{ptr, cmp};

use libc::{self, c_char, c_void, ptrdiff_t};

use lisp::LispObject;
use multibyte;
use remacs_sys::{SYMBOL_NAME, EmacsInt, error, base64_encode_1, base64_decode_1, make_string,
                 make_uninit_multibyte_string, string_to_multibyte as c_string_to_multibyte,
                 make_unibyte_string};
use remacs_macros::lisp_fn;

pub static MIME_LINE_LENGTH: isize = 76;

/// Return t if OBJECT is a string.
#[lisp_fn]
fn stringp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_string())
}

/// Base64-encode STRING and return the result.
/// Optional second argument NO-LINE-BREAK means do not break long lines
/// into shorter lines.
#[lisp_fn(min = "1")]
fn base64_encode_string(string: LispObject, no_line_break: LispObject) -> LispObject {
    let mut string = string.as_string_or_error();

    // We need to allocate enough room for the encoded text
    // We will need 33 1/3% more space, plus a newline every 76 characters(MIME_LINE_LENGTH)
    // and then round up
    let length = string.len_bytes();
    let mut allength: ptrdiff_t = length + length / 3 + 1;
    allength += allength / MIME_LINE_LENGTH + 1 + 6;

    // This function uses SAFE_ALLOCA in the c layer, however I cannot find an equivalent
    // for rust. Instead, we will use a Vec to store the temporary char buffer.
    let mut buffer: Vec<c_char> = Vec::with_capacity(allength as usize);
    unsafe {
        let encoded = buffer.as_mut_ptr();
        let encodedLength = base64_encode_1(
            string.sdata_ptr(),
            encoded,
            length,
            no_line_break.is_nil(),
            string.is_multibyte(),
        );

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
#[lisp_fn]
fn base64_decode_string(string: LispObject) -> LispObject {
    let mut string = string.as_string_or_error();

    let length = string.len_bytes();
    let mut buffer: Vec<c_char> = Vec::with_capacity(length as usize);
    let mut decoded_string: LispObject = LispObject::constant_nil();

    unsafe {
        let decoded = buffer.as_mut_ptr();
        let decoded_length =
            base64_decode_1(string.sdata_ptr(), decoded, length, false, ptr::null_mut());

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
#[lisp_fn]
fn string_bytes(string: LispObject) -> LispObject {
    let string = string.as_string_or_error();
    LispObject::from_natnum(string.len_bytes() as EmacsInt)
}

/// Return t if two strings have identical contents.
/// Case is significant, but text properties are ignored.
/// Symbols are also allowed; their print names are used instead.
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
    let new_string = LispObject::from_raw(new_string);
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
    unsafe { LispObject::from_raw(c_string_to_multibyte(string.to_raw())) }
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

        unsafe {
            if converted_size < size {
                error(
                    "Can't convert %ldth character to unibyte\0".as_ptr(),
                    converted_size,
                );
            }

            let raw_ptr = make_unibyte_string(buffer.as_ptr() as *const libc::c_char, size);
            LispObject::from_raw(raw_ptr)
        }
    } else {
        string
    }
}

fn get_string_or_symbol(mut string: LispObject) -> multibyte::LispStringRef {
    if string.is_symbol() {
        string = string.symbol_name()
    }

    string.as_string_or_error()
}

fn string_lessp(string1: LispObject, string2: LispObject) -> LispObject {
    let mut lispstr1 = get_string_or_symbol(string1);
    let mut lispstr2 = get_string_or_symbol(string2);

    let end = cmp::min(lispstr1.len_bytes(), lispstr2.len_bytes());
    let mut i1 = 0;
    while i1 < end {
        // Unwraps should be fine here, due to our manual tracking of
        // valid length
        let (codept1, i1_bytes) = lispstr1.next().unwrap();
        let (codept2, _) = lispstr2.next().unwrap();

        i1 += i1_bytes as isize;
        
        if codept1 != codept2 {
            if codept1 < codept2 {
                return LispObject::constant_t();
            } else {
                return LispObject::constant_nil();
            }
        }
    }

    if i1 < lispstr2.len_bytes() {
        return LispObject::constant_t();
    } else {
        return LispObject::constant_nil();
    }
}
