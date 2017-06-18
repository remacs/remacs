//! Base64 de- and encoding functions.

use std::ptr;
use std::str;
use std::slice;
use libc::{ptrdiff_t, c_char};
use base64_crate;

// We don't use the multibyte arg as we are able to encode multibyte strings. It
// still needs to be there because of the C API.
#[no_mangle]
pub extern "C" fn base64_encode_1(
    from: *const c_char,
    to: *mut c_char,
    length: ptrdiff_t,
    line_break: bool,
    _multibyte: bool,
) -> ptrdiff_t {
    let bytes = unsafe { slice::from_raw_parts(from as *const u8, length as usize) };
    let config = if line_break { base64_crate::MIME } else { base64_crate::STANDARD };
    let coded = base64_crate::encode_config(bytes, config);
    let size = coded.len();
    unsafe {
        ptr::copy_nonoverlapping(coded.as_ptr(), to as *mut u8, size);
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

    if let Ok(decoded) = base64_crate::decode_config(encoded, base64_crate::MIME) {
        let size = decoded.len();
        if !nchars_return.is_null() {
            unsafe {
                *nchars_return = if !multibyte {
                    size
                } else {
                    match str::from_utf8(&decoded) {
                        Ok(s) => s.chars().count(),
                        Err(_) => size,
                    }
                } as ptrdiff_t;
            }
        }
        unsafe {
            ptr::copy_nonoverlapping(decoded.as_ptr(), to as *mut u8, size);
        }
        size as ptrdiff_t
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
