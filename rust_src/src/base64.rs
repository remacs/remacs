//! Base64 de- and encoding functions.

use std::ptr;
use std::slice;
use libc::{ptrdiff_t, c_char, c_uchar};
use base64_crate;

use multibyte::{MAX_5_BYTE_CHAR, multibyte_char_at, raw_byte_from_codepoint};

#[no_mangle]
pub extern "C" fn base64_encode_1(
    from: *const c_char,
    to: *mut c_char,
    length: ptrdiff_t,
    line_break: bool,
    multibyte: bool,
) -> ptrdiff_t {
    let config = if line_break {
        base64_crate::MIME
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
/// decoded result should be in multibyte form.  If NCHARS_RETURN is not NULL,
/// store the number of produced characters in *NCHARS_RETURN.
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
