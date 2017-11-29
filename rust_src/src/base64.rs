//! Base64 de- and encoding functions.

use libc::{c_char, c_uchar, ptrdiff_t};
use std::ptr;
use std::slice;

use base64_crate;

use remacs_macros::lisp_fn;
use remacs_sys::make_unibyte_string;

use lisp::LispObject;
use lisp::defsubr;
use multibyte::{multibyte_char_at, raw_byte_from_codepoint, MAX_5_BYTE_CHAR};
use strings::MIME_LINE_LENGTH;

#[no_mangle]
pub extern "C" fn base64_encode_1(
    from: *const c_char,
    to: *mut c_char,
    length: ptrdiff_t,
    line_break: bool,
    multibyte: bool,
) -> ptrdiff_t {
    let config = if line_break {
        // base64_crate::MIME, but with LF instead of CRLF
        base64_crate::Config::new(
            base64_crate::CharacterSet::Standard,
            true, // pad
            true, // strip whitespace
            base64_crate::LineWrap::Wrap(76, base64_crate::LineEnding::LF),
        )
    } else {
        base64_crate::STANDARD
    };
    let bytes = unsafe { slice::from_raw_parts(from as *const u8, length as usize) };

    let output = if multibyte {
        // Transform non-ASCII characters in multibyte string to Latin1,
        // erroring out for non-Latin1 codepoints, and resolve raw 8-bit bytes.
        let mut input = Vec::with_capacity(bytes.len());
        let mut i = 0;
        while i < bytes.len() {
            let (cp, len) = multibyte_char_at(&bytes[i..]);
            if cp > MAX_5_BYTE_CHAR {
                input.push(raw_byte_from_codepoint(cp));
            } else if cp < 256 {
                input.push(cp as c_uchar);
            } else {
                return -1;
            }
            i += len;
        }
        base64_crate::encode_config(&input, config)
    } else {
        // Just encode the raw bytes.
        base64_crate::encode_config(bytes, config)
    };
    let size = output.len();
    unsafe {
        ptr::copy_nonoverlapping(output.as_ptr(), to as *mut u8, size);
    }
    size as ptrdiff_t
}

/// Base64-decode the data at FROM of LENGTH bytes into TO.  If MULTIBYTE, the
/// decoded result should be in multibyte form.  If `NCHARS_RETURN` is not NULL,
/// store the number of produced characters in `*NCHARS_RETURN`.
#[no_mangle]
pub extern "C" fn base64_decode_1(
    from: *const c_char,
    to: *mut c_char,
    length: ptrdiff_t,
    multibyte: bool,
    nchars_return: *mut ptrdiff_t,
) -> ptrdiff_t {
    let encoded = unsafe { slice::from_raw_parts(from as *const u8, length as usize) };

    // Use the MIME config to allow embedded newlines.
    if let Ok(decoded) = base64_crate::decode_config(encoded, base64_crate::MIME) {
        if !nchars_return.is_null() {
            unsafe {
                *nchars_return = decoded.len() as ptrdiff_t;
            }
        }
        if multibyte {
            // Decode non-ASCII bytes into UTF-8 pairs.
            let s: String = decoded.iter().map(|&byte| byte as char).collect();
            unsafe {
                ptr::copy_nonoverlapping(s.as_ptr(), to as *mut u8, s.len());
            }
            s.len() as ptrdiff_t
        } else {
            unsafe {
                ptr::copy_nonoverlapping(decoded.as_ptr(), to as *mut u8, decoded.len());
            }
            decoded.len() as ptrdiff_t
        }
    } else {
        -1
    }
}

#[test]
fn test_base64_encode_1() {
    let input = "hello world";
    let mut encoded = [0u8; 20];

    let length = base64_encode_1(
        input.as_ptr() as *mut c_char,
        encoded.as_mut_ptr() as *mut c_char,
        input.as_bytes().len() as ptrdiff_t,
        false,
        false,
    );

    assert!(length != -1);
    assert_eq!(b"aGVsbG8gd29ybGQ=", &encoded[..length as usize]);
}

#[test]
fn test_base64_decode_1() {
    use std::str;

    let input = "aGVsbG8gd29ybGQ=";
    let mut decoded = [0u8; 20];

    let mut n: isize = 0;
    let nchars: *mut isize = &mut n;

    let length = base64_decode_1(
        input.as_ptr() as *mut c_char,
        decoded.as_mut_ptr() as *mut c_char,
        input.as_bytes().len() as ptrdiff_t,
        true,
        nchars,
    );
    assert!(length != -1);

    let answer = str::from_utf8(&decoded[..length as usize]).unwrap();

    assert_eq!(n, length);
    assert_eq!("hello world", answer);
}

/// Base64-encode STRING and return the result.
/// Optional second argument NO-LINE-BREAK means do not break long lines
/// into shorter lines.
#[lisp_fn(min = "1")]
pub fn base64_encode_string(string: LispObject, no_line_break: LispObject) -> LispObject {
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
    let encoded = buffer.as_mut_ptr();
    let encoded_length = base64_encode_1(
        string.sdata_ptr(),
        encoded,
        length,
        no_line_break.is_nil(),
        string.is_multibyte(),
    );

    if encoded_length > allength {
        panic!("base64 encoded length is larger then allocated buffer");
    }

    if encoded_length < 0 {
        error!("Multibyte character in data for base64 encoding");
    }

    unsafe { LispObject::from(make_unibyte_string(encoded, encoded_length)) }
}

/// Base64-decode STRING and return the result.
#[lisp_fn]
pub fn base64_decode_string(string: LispObject) -> LispObject {
    let mut string = string.as_string_or_error();

    let length = string.len_bytes();
    let mut buffer: Vec<c_char> = Vec::with_capacity(length as usize);

    let decoded = buffer.as_mut_ptr();
    let decoded_length =
        base64_decode_1(string.sdata_ptr(), decoded, length, false, ptr::null_mut());

    if decoded_length > length {
        panic!("Decoded length is above length");
    } else if decoded_length < 0 {
        error!("Invalid base64 data");
    }
    unsafe { LispObject::from(make_unibyte_string(decoded, decoded_length)) }
}

include!(concat!(env!("OUT_DIR"), "/base64_exports.rs"));
