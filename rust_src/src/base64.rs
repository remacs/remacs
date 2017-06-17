extern crate libc;
extern crate rustc_serialize;

use std::ffi::CStr;
use std::ffi::CString;
use std::ptr;
use std::slice;
use strings::MIME_LINE_LENGTH;
use self::rustc_serialize::base64::{FromBase64, ToBase64, STANDARD};

// We don't use the length arg as CStr::from_ptr handles checking the
// length for us. We don't use the multibyte arg either as we are able
// to encode multibyte strings. They need to still be there because of
// the C API.
#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn base64_encode_1(
    from: *const libc::c_char,
    to: *mut libc::c_char,
    length: libc::ptrdiff_t,
    line_break: bool,
    multibyte: bool,
) -> libc::ptrdiff_t {
    let bytes = unsafe { slice::from_raw_parts(from as *const u8, length as usize) };
    let coded = bytes.to_base64(STANDARD);
    let size = coded.len() as libc::ptrdiff_t;
    let string = CString::new(coded).unwrap();

    if line_break && MIME_LINE_LENGTH < size {
        // If we want to break lines at every MIME_LINE_LENGTH, and
        // the line is longer, then
        let bytes = string.into_bytes();
        let mut n = 0;
        let mut sink = to;
        // split the byte slice into chunks the size of
        // MIME_LINE_LENGTH.
        for chunk in bytes.chunks(MIME_LINE_LENGTH as usize) {
            let l = chunk.len();
            let c = chunk.as_ptr();
            unsafe {
                ptr::copy(c as *const libc::c_char, sink, l);
                sink = sink.offset(l as isize);

                // Add a newline after the current chunk.
                *sink = b'\n' as libc::c_char;
                sink = sink.offset(1);
            }
            n = n + 1;
        }
        size + n
    } else {
        unsafe {
            ptr::copy(string.into_raw(), to, size as usize);
        }
        size
    }
}

fn decode(encoded: &CStr) -> Result<Vec<u8>, String> {
    encoded.to_str().map_err(|err| err.to_string()).and_then(
        |encoded| {
            encoded.from_base64().map_err(|err| err.to_string())
        },
    )
}

///  Base64-decode the data at FROM of LENGTH bytes into TO.  If
///  MULTIBYTE, the decoded result should be in multibyte form.  If
///  NCHARS_RETURN is not NULL, store the number of produced
///  characters in *NCHARS_RETURN.
// We don't use the length arg as CStr::from_ptr handles the length
// for us. It still needs to be there to conform to the C API.
#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn base64_decode_1(
    from: *const libc::c_char,
    to: *mut libc::c_char,
    length: libc::ptrdiff_t,
    multibyte: bool,
    nchars_return: *mut libc::ptrdiff_t,
) -> libc::ptrdiff_t {
    let encoded = unsafe { CStr::from_ptr(from) };

    match decode(encoded) {
        Ok(decoded) => {

            let size = decoded.len() as libc::ptrdiff_t;
            let d = decoded.as_ptr() as *const libc::c_char;

            unsafe {
                if !multibyte && !nchars_return.is_null() {
                    *nchars_return = size
                } else if !nchars_return.is_null() {
                    match String::from_utf8(decoded) {
                        Ok(s) => {
                            *nchars_return = s.chars().count() as libc::ptrdiff_t;
                        }
                        Err(_) => *nchars_return = size,
                    }
                }

                ptr::copy(d, to, size as usize);
                size
            }

        }
        Err(_) => -1,
    }

}

#[test]
fn test_base64_encode_1() {
    let input = CString::new("hello world").unwrap();
    let uncoded: *const libc::c_char = input.as_ptr();
    let mut encoded = [0; 17];

    let length = base64_encode_1(
        uncoded,
        encoded.as_mut_ptr(),
        input.as_bytes().len() as libc::ptrdiff_t,
        false,
        false,
    );

    assert!(length != -1);
    let answer = unsafe { CStr::from_ptr(encoded.as_ptr()).to_str().unwrap() };

    assert_eq!(answer.len(), length as usize);
    assert_eq!("aGVsbG8gd29ybGQ=", answer);
}

#[test]
fn test_base64_decode_1() {
    let input = CString::new("aGVsbG8gd29ybGQ=").unwrap();
    let encoded: *const libc::c_char = input.as_ptr();

    let mut decoded = [0; 20];

    let mut n: isize = 0;
    let nchars: *mut isize = &mut n;

    let length = base64_decode_1(
        encoded,
        decoded.as_mut_ptr(),
        input.as_bytes().len() as libc::ptrdiff_t,
        true,
        nchars,
    );
    assert!(length != -1);

    let answer = unsafe { CStr::from_ptr(decoded.as_ptr()).to_str().unwrap() };

    assert_eq!(n, length);
    assert_eq!("hello world", answer);
}
