//! Base64 de- and encoding functions.

use libc::{c_char, c_uchar};
use std::ptr;
use std::slice;

use base64_crate;

use remacs_macros::lisp_fn;
use remacs_sys::make_unibyte_string;

use lisp::LispObject;
use lisp::defsubr;
use multibyte::{multibyte_char_at, raw_byte_from_codepoint, LispStringRef, MAX_5_BYTE_CHAR};
use strings::MIME_LINE_LENGTH;

#[no_mangle]
pub extern "C" fn base64_encode_1(
    from: *const c_char,
    in_length: usize,
    to: *mut c_char,
    out_length: usize,
    line_break: bool,
    multibyte: bool,
) -> isize {
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
    let bytes = unsafe { slice::from_raw_parts(from as *const u8, in_length as usize) };
    let mut output = unsafe { slice::from_raw_parts_mut(to as *mut u8, out_length as usize) };

    let encoded_size = if multibyte {
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
        base64_crate::encode_config_slice(&input, config, &mut output)
    } else {
        // Just encode the raw bytes.
        base64_crate::encode_config_slice(bytes, config, &mut output)
    };

    encoded_size as isize
}

/// Base64-decode the data at FROM of LENGTH bytes into TO.  If MULTIBYTE, the
/// decoded result should be in multibyte form.  If `NCHARS_RETURN` is not NULL,
/// store the number of produced characters in `*NCHARS_RETURN`.
#[no_mangle]
pub extern "C" fn base64_decode_1(
    from: *const c_char,
    length: usize,
    to: *mut c_char,
    out_length: usize,
    multibyte: bool,
    nchars_return: *mut isize,
) -> isize {
    let encoded = unsafe { slice::from_raw_parts(from as *const u8, length as usize) };
    let decoded = unsafe { slice::from_raw_parts_mut(to as *mut u8, out_length as usize) };

    // Use the MIME config to allow embedded newlines.
    if let Ok(decoded_length) =
        base64_crate::decode_config_slice(encoded, base64_crate::MIME, decoded)
    {
        if !nchars_return.is_null() {
            unsafe {
                *nchars_return = decoded_length as isize;
            }
        }
        if multibyte {
            // Decode non-ASCII bytes into UTF-8 pairs.
            let s: String = decoded[..decoded_length]
                .iter()
                .map(|&byte| byte as char)
                .collect();
            let s_len = s.len();
            unsafe {
                ptr::copy_nonoverlapping(s.as_ptr(), to as *mut u8, s_len);
            }
            s.len() as isize
        } else {
            decoded_length as isize
        }
    } else {
        -1
    }
}

#[test]
fn test_simple_base64_encode_1() {
    let input = "hello world";
    let mut encoded = [0u8; 20];

    let length = base64_encode_1(
        input.as_ptr() as *mut c_char,
        input.as_bytes().len(),
        encoded.as_mut_ptr() as *mut c_char,
        encoded.len(),
        false,
        false,
    );
    assert!(length != -1);

    assert_eq!(b"aGVsbG8gd29ybGQ=", &encoded[..length as usize]);
}

#[test]
fn test_linewrap_base64_encode_1() {
    let input = "Emacs is a widely used tool with a long history, broad platform
support and strong backward compatibility requirements. The core team
is understandably cautious in making far-reaching changes.";
    let mut encoded = [0u8; 500];

    let length = base64_encode_1(
        input.as_ptr() as *mut c_char,
        input.as_bytes().len(),
        (&mut encoded).as_mut_ptr() as *mut c_char,
        500,
        true,
        false,
    );
    assert!(length != -1);

    let expected = "RW1hY3MgaXMgYSB3aWRlbHkgdXNlZCB0b29sIHdpdGggYSBsb25nIGhpc3RvcnksIGJyb2FkIHBs
YXRmb3JtCnN1cHBvcnQgYW5kIHN0cm9uZyBiYWNrd2FyZCBjb21wYXRpYmlsaXR5IHJlcXVpcmVt
ZW50cy4gVGhlIGNvcmUgdGVhbQppcyB1bmRlcnN0YW5kYWJseSBjYXV0aW91cyBpbiBtYWtpbmcg
ZmFyLXJlYWNoaW5nIGNoYW5nZXMu"
        .to_string();
    assert_eq!(
        expected,
        String::from_utf8_lossy(&encoded[..length as usize])
    );
    assert_eq!(expected.len(), length as usize);
}

#[no_mangle]
pub extern "C" fn compute_decode_size(len: usize) -> usize {
    ((len + 3) / 4) * 3
}

#[no_mangle]
pub extern "C" fn compute_encode_size(len: usize) -> usize {
    ((len * 4) / 3) + 4
}

#[no_mangle]
pub extern "C" fn pad_base64_size(len: usize) -> usize {
    len + (len / (MIME_LINE_LENGTH as usize)) + 1 + 6
}

#[test]
fn test_compute_decode_size() {
    assert_eq!(3, compute_decode_size(1));
    assert_eq!(3, compute_decode_size(2));
    assert_eq!(3, compute_decode_size(3));
    assert_eq!(3, compute_decode_size(4));
    assert_eq!(6, compute_decode_size(5));
    assert_eq!(6, compute_decode_size(6));
    assert_eq!(6, compute_decode_size(7));
    assert_eq!(6, compute_decode_size(8));
}

#[test]
fn test_compute_encode_size() {
    assert_eq!(5, compute_encode_size(1));
    assert_eq!(6, compute_encode_size(2));
    assert_eq!(8, compute_encode_size(3));
    assert_eq!(9, compute_encode_size(4));
    assert_eq!(10, compute_encode_size(5));
    assert_eq!(12, compute_encode_size(6));
    assert_eq!(13, compute_encode_size(7));
    assert_eq!(14, compute_encode_size(8));
}

#[test]
fn test_pad_base64_size() {
    assert_eq!(8, pad_base64_size(1));
    assert_eq!(9, pad_base64_size(2));
    assert_eq!(10, pad_base64_size(3));
    assert_eq!(11, pad_base64_size(4));
    assert_eq!(12, pad_base64_size(5));
    assert_eq!(13, pad_base64_size(6));
    assert_eq!(14, pad_base64_size(7));
    assert_eq!(15, pad_base64_size(8));
    assert_eq!(84, pad_base64_size(76));
}

#[test]
fn test_simple_base64_decode_1() {
    let input = "aGVsbG8gd29ybGQ=";
    let clear = "hello world";
    let mut n = 0;
    let mut decoded: Vec<u8> = Vec::new();
    decoded.resize(compute_decode_size(input.len()), 0);

    let length = base64_decode_1(
        input.as_ptr() as *mut c_char,
        input.as_bytes().len(),
        decoded.as_mut_ptr() as *mut c_char,
        decoded.len(),
        true,
        &mut n,
    );
    unsafe { decoded.set_len(length as usize) };

    assert_eq!(clear.len(), length as usize);
    assert_eq!(clear, String::from_utf8(decoded).unwrap());
}

#[test]
fn test_multibyte_base64_decode_1() {
    let input = "RG9icv0gZGVu";

    let clear = "Dobrý den"; // Czech
    let mut n = 0;
    let mut decoded: Vec<u8> = Vec::new();
    decoded.resize(compute_decode_size(input.len()), 0);

    let length = base64_decode_1(
        input.as_ptr() as *mut c_char,
        input.as_bytes().len(),
        decoded.as_mut_ptr() as *mut c_char,
        decoded.len(),
        true,
        &mut n,
    );
    unsafe { decoded.set_len(length as usize) };

    assert_eq!(clear.len(), length as usize);
    assert_eq!(clear, String::from_utf8(decoded).unwrap());

    // Now run again, but disable multibyte

    let clear = b"Dobr\xFD den"; // Czech
    n = 0;
    decoded = Vec::new();
    decoded.resize(compute_decode_size(input.len()), 0);

    let length = base64_decode_1(
        input.as_ptr() as *mut c_char,
        input.as_bytes().len(),
        decoded.as_mut_ptr() as *mut c_char,
        decoded.len(),
        false,
        &mut n,
    );
    unsafe { decoded.set_len(length as usize) };

    assert_eq!(clear.len(), length as usize);
    assert_eq!(clear, &decoded[..]);
}

#[test]
fn test_linewrap_base64_decode_1() {
    let input1 = "
WW91IG1heSBlbmNvdW50ZXIgYnVncyBpbiB0aGlzIHJlbGVhc2UuICBJZiB5b3UgZG8sIHBsZWFz
ZSByZXBvcnQKdGhlbTsgeW91ciBidWcgcmVwb3J0cyBhcmUgdmFsdWFibGUgY29udHJpYnV0aW9u
cyB0byB0aGUgRlNGLCBzaW5jZQp0aGV5IGFsbG93IHVzIHRvIG5vdGljZSBhbmQgZml4IHByb2Js
ZW1zIG9uIG1hY2hpbmVzIHdlIGRvbid0IGhhdmUsIG9yCmluIGNvZGUgd2UgZG9uJ3QgdXNlIG9m
dGVuLiAgUGxlYXNlIHNlbmQgYnVnIHJlcG9ydHMgdG8gdGhlIG1haWxpbmcKbGlzdCBidWctZ251
LWVtYWNzQGdudS5vcmcuICBJZiBwb3NzaWJsZSwgdXNlIE0teCByZXBvcnQtZW1hY3MtYnVnLgoK
U2VlIHRoZSAiQnVncyIgc2VjdGlvbiBvZiB0aGUgRW1hY3MgbWFudWFsIGZvciBtb3JlIGluZm9y
bWF0aW9uIG9uIGhvdwp0byByZXBvcnQgYnVncy4gIChUaGUgZmlsZSAnQlVHUycgaW4gdGhpcyBk
aXJlY3RvcnkgZXhwbGFpbnMgaG93IHlvdQpjYW4gZmluZCBhbmQgcmVhZCB0aGF0IHNlY3Rpb24g
dXNpbmcgdGhlIEluZm8gZmlsZXMgdGhhdCBjb21lIHdpdGgKRW1hY3MuKSAgRm9yIGEgbGlzdCBv
ZiBtYWlsaW5nIGxpc3RzIHJlbGF0ZWQgdG8gRW1hY3MsIHNlZQo8aHR0cHM6Ly9zYXZhbm5haC5n
bnUub3JnL21haWwvP2dyb3VwPWVtYWNzPi4gIEZvciB0aGUgY29tcGxldGUKbGlzdCBvZiBHTlUg
bWFpbGluZyBsaXN0cywgc2VlIDxodHRwOi8vbGlzdHMuZ251Lm9yZy8+LgoK";

    let input2 = "
WW91IG1heSBlbmNvdW50ZXIgYnVncyBpbiB0aGlzIHJlbGVhc2UuICBJZiB5b3UgZG8sIHBsZWFz
ZSByZXBvcnQKdGhlbTsgeW91ciBidWcgcmVwb3J0cyBhcmUgdmFsdWFibGUgY29udHJpYnV0aW9u
cyB0byB0aGUgRlNGLCBzaW5jZQp0aGV5IGFsbG93IHVzIHRvIG5vdGljZSBhbmQgZml4IHByb2Js
ZW1zIG9uIG1hY2hpbmVzIHdlIGRvbid0IGhhdmUsIG9yCmluIGNvZGUgd2UgZG9uJ3QgdXNlIG9m
dGVuLiAgUGxlYXNlIHNlbmQgYnVnIHJlcG9ydHMgdG8gdGhlIG1haWxpbmcKbGlzdCBidWctZ251
LWVtYWNzQGdudS5vcmcuICBJZiBwb3NzaWJsZSwgdXNlIE0teCByZXBvcnQtZW1hY3MtYnVnLgoK
U2VlIHRoZSAiQnVncyIgc2VjdGlvbiBvZiB0aGUgRW1hY3MgbWFudWFsIGZvciBtb3JlIGluZm9y
bWF0aW9uIG9uIGhvdwp0byByZXBvcnQgYnVncy4gIChUaGUgZmlsZSAnQlVHUycgaW4gdGhpcyBk
aXJlY3RvcnkgZXhwbGFpbnMgaG93IHlvdQpjYW4gZmluZCBhbmQgcmVhZCB0aGF0IHNlY3Rpb24g
dXNpbmcgdGhlIEluZm8gZmlsZXMgdGhhdCBjb21lIHdpdGgKRW1hY3MuKSAgRm9yIGEgbGlzdCBv
ZiBtYWlsaW5nIGxpc3RzIHJlbGF0ZWQgdG8gRW1hY3MsIHNlZQo8aHR0cHM6Ly9zYXZhbm5haC5n
bnUub3JnL21haWwvP2dyb3VwPWVtYWNzPi4gIEZvciB0aGUgY29tcGxldGUKbGlzdCBvZiBHTlUg
bWFpbGluZyBsaXN0cywgc2VlIDxodHRwOi8vbGlzdHMuZ251Lm9yZy8+LgoK";

    let mut decoded1: Vec<u8> = Vec::with_capacity(730);
    let mut decoded2: Vec<u8> = Vec::with_capacity(730);

    let mut n1: isize = 0;
    let nchars1: *mut isize = &mut n1;

    let mut n2: isize = 0;
    let nchars2: *mut isize = &mut n2;

    let length1 = base64_decode_1(
        input1.as_ptr() as *mut c_char,
        input1.as_bytes().len(),
        decoded1.as_mut_ptr() as *mut c_char,
        decoded1.capacity(),
        true,
        nchars1,
    );

    let length2 = base64_decode_1(
        input2.as_ptr() as *mut c_char,
        input2.as_bytes().len(),
        decoded2.as_mut_ptr() as *mut c_char,
        decoded2.capacity(),
        true,
        nchars2,
    );

    assert_eq!(length1, length2);
    assert_eq!(n1, n2);
    assert_eq!(
        String::from_utf8(decoded1).unwrap(),
        String::from_utf8(decoded2).unwrap()
    );
}

/// Base64-encode STRING and return the result.
/// Optional second argument NO-LINE-BREAK means do not break long lines
/// into shorter lines.
#[lisp_fn(min = "1")]
pub fn base64_encode_string(mut string: LispStringRef, no_line_break: bool) -> LispObject {
    // We need to allocate enough room for the encoded text
    let length = string.len_bytes() as usize;
    let allength = pad_base64_size(compute_encode_size(length));

    // This function uses SAFE_ALLOCA in the c layer, however I cannot find an equivalent
    // for rust. Instead, we will use a Vec to store the temporary char buffer.
    let mut buffer: Vec<c_char> = Vec::with_capacity(allength as usize);
    let encoded = buffer.as_mut_ptr();
    let encoded_length = base64_encode_1(
        string.sdata_ptr(),
        length,
        encoded,
        allength,
        no_line_break,
        string.is_multibyte(),
    );
    if encoded_length < 0 {
        error!("Multibyte character in data for base64 encoding");
    }

    unsafe { LispObject::from_raw(make_unibyte_string(encoded, encoded_length)) }
}

/// Base64-decode STRING and return the result.
#[lisp_fn]
pub fn base64_decode_string(mut string: LispStringRef) -> LispObject {
    let length = compute_decode_size(string.len_bytes() as usize);
    let mut buffer: Vec<c_char> = Vec::with_capacity(length);

    let decoded = buffer.as_mut_ptr();
    let decoded_length = base64_decode_1(
        string.sdata_ptr(),
        string.len_bytes() as usize,
        decoded,
        length,
        false,
        ptr::null_mut(),
    );

    if decoded_length < 0 {
        error!("Invalid base64 data");
    }
    unsafe {
        buffer.set_len(decoded_length as usize);
        LispObject::from_raw(make_unibyte_string(decoded, decoded_length))
    }
}

include!(concat!(env!("OUT_DIR"), "/base64_exports.rs"));
