//! Base64 de- and encoding functions.
use std::ptr;
use std::{cmp::max, slice};

use libc::{c_char, c_uchar};
use remacs_macros::lisp_fn;

use crate::{
    base64_crate,
    buffers::validate_region,
    lisp::defsubr,
    lisp::LispObject,
    marker::buf_charpos_to_bytepos,
    multibyte::{multibyte_char_at, raw_byte_from_codepoint, LispStringRef, MAX_5_BYTE_CHAR},
    remacs_sys::EmacsInt,
    remacs_sys::{
        del_range_both, del_range_byte, insert, insert_1_both, make_unibyte_string, move_gap_both,
        set_point, set_point_both, signal_after_change, temp_set_point_both,
    },
    strings::MIME_LINE_LENGTH,
    threads::ThreadState,
};

pub fn base64_encode_1(bytes: &[u8], line_break: bool, multibyte: bool) -> Result<String, ()> {
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

    let encoded_string = if multibyte {
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
                return Err(());
            }
            i += len;
        }
        base64_crate::encode_config(&input, config)
    } else {
        // Just encode the raw bytes.
        base64_crate::encode_config(bytes, config)
    };

    Ok(encoded_string)
}

/// Base64-decode the data at FROM of LENGTH bytes into TO.  If MULTIBYTE, the
/// decoded result should be in multibyte form.  If `NCHARS_RETURN` is not NULL,
/// store the number of produced characters in `*NCHARS_RETURN`.
#[no_mangle]
pub unsafe extern "C" fn base64_decode_1(
    from: *const c_char,
    length: usize,
    to: *mut c_char,
    out_length: usize,
    multibyte: bool,
    nchars_return: *mut isize,
) -> isize {
    let encoded = slice::from_raw_parts(from as *const u8, length as usize);
    let decoded = slice::from_raw_parts_mut(to as *mut u8, out_length as usize);

    // Use the MIME config to allow embedded newlines.
    match base64_crate::decode_config_slice(encoded, base64_crate::MIME, decoded) {
        Ok(decoded_length) => {
            if !nchars_return.is_null() {
                *nchars_return = decoded_length as isize;
            }
            if multibyte {
                // Decode non-ASCII bytes into UTF-8 pairs.
                let s = encode_multibyte_string(&decoded[..decoded_length]);
                let s_len = s.len();
                ptr::copy_nonoverlapping(s.as_ptr(), to as *mut u8, s_len);
                s_len as isize
            } else {
                decoded_length as isize
            }
        }
        _ => -1,
    }
}

/// Encode some text we just got from decoding base64 data like C implementation does via
/// BYTE8_STRING.
fn encode_multibyte_string(v: &[u8]) -> Vec<u8> {
    let mut res = Vec::with_capacity(v.len());

    for &c in v {
        if c >= 128 {
            res.push(0xc0 | ((c) >> 6) & 0x01);
            res.push(0x80 | (c & 0x3f));
        } else {
            res.push(c);
        }
    }

    res
}

#[test]
fn test_simple_base64_encode_1() {
    let input = "hello world";
    let mut encoded = [0u8; 20];

    let encoded = base64_encode_1(input.as_bytes(), false, false).unwrap();
    assert_eq!("aGVsbG8gd29ybGQ=", encoded);
}

#[test]
fn test_linewrap_base64_encode_1() {
    let input = "Emacs is a widely used tool with a long history, broad platform
support and strong backward compatibility requirements. The core team
is understandably cautious in making far-reaching changes.";

    let encoded = base64_encode_1(input.as_bytes(), true, false).unwrap();
    let expected = "RW1hY3MgaXMgYSB3aWRlbHkgdXNlZCB0b29sIHdpdGggYSBsb25nIGhpc3RvcnksIGJyb2FkIHBs
YXRmb3JtCnN1cHBvcnQgYW5kIHN0cm9uZyBiYWNrd2FyZCBjb21wYXRpYmlsaXR5IHJlcXVpcmVt
ZW50cy4gVGhlIGNvcmUgdGVhbQppcyB1bmRlcnN0YW5kYWJseSBjYXV0aW91cyBpbiBtYWtpbmcg
ZmFyLXJlYWNoaW5nIGNoYW5nZXMu"
        .to_string();
    assert_eq!(expected, encoded);
    assert_eq!(expected.len(), encoded.len());
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

    let length = unsafe {
        base64_decode_1(
            input.as_ptr() as *mut c_char,
            input.as_bytes().len(),
            decoded.as_mut_ptr() as *mut c_char,
            decoded.len(),
            true,
            &mut n,
        )
    };
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

    let length = unsafe {
        base64_decode_1(
            input.as_ptr() as *mut c_char,
            input.as_bytes().len(),
            decoded.as_mut_ptr() as *mut c_char,
            decoded.len(),
            true,
            &mut n,
        )
    };
    unsafe { decoded.set_len(length as usize) };

    // We don't round-trip on multibyte decode but use a particular encoding
    let decoded_multibyte = vec![68, 111, 98, 114, 193, 189, 32, 100, 101, 110];

    assert_eq!(clear.len(), length as usize);
    assert_eq!(decoded_multibyte, decoded);

    // Now run again, but disable multibyte

    let clear = b"Dobr\xFD den"; // Czech
    n = 0;
    decoded = Vec::new();
    decoded.resize(compute_decode_size(input.len()), 0);

    let length = unsafe {
        base64_decode_1(
            input.as_ptr() as *mut c_char,
            input.as_bytes().len(),
            decoded.as_mut_ptr() as *mut c_char,
            decoded.len(),
            false,
            &mut n,
        )
    };
    unsafe { decoded.set_len(length as usize) };

    assert_eq!(clear.len(), length as usize);
    assert_eq!(clear, &decoded[..]);

    // Run it again with multibyte but with a more complex string
    let input = "w7HDscOxw7HDocOhw6HDocOhw6HEkcSRxJHEkcSRxJHEkcOwCg==";
    let clear = "ññññááááááđđđđđđđð\n";
    let decoded_multibyte = vec![
        193, 131, 192, 177, 193, 131, 192, 177, 193, 131, 192, 177, 193, 131, 192, 177, 193, 131,
        192, 161, 193, 131, 192, 161, 193, 131, 192, 161, 193, 131, 192, 161, 193, 131, 192, 161,
        193, 131, 192, 161, 193, 132, 192, 145, 193, 132, 192, 145, 193, 132, 192, 145, 193, 132,
        192, 145, 193, 132, 192, 145, 193, 132, 192, 145, 193, 132, 192, 145, 193, 131, 192, 176,
        10,
    ];
    let mut n = 0;
    let mut decoded: Vec<u8> = Vec::new();
    decoded.resize(compute_decode_size(input.len()), 0);

    let length = unsafe {
        base64_decode_1(
            input.as_ptr() as *mut c_char,
            input.as_bytes().len(),
            decoded.as_mut_ptr() as *mut c_char,
            decoded.len(),
            true,
            &mut n,
        )
    };
    unsafe { decoded.set_len(length as usize) };

    // We don't round-trip on multibyte decode but use a particular encoding
    assert_eq!(clear.len(), n as usize);
    assert_eq!(decoded_multibyte.len(), length as usize);
    assert_eq!(decoded_multibyte, decoded);
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

    let length1 = unsafe {
        base64_decode_1(
            input1.as_ptr() as *mut c_char,
            input1.as_bytes().len(),
            decoded1.as_mut_ptr() as *mut c_char,
            decoded1.capacity(),
            true,
            nchars1,
        )
    };

    let length2 = unsafe {
        base64_decode_1(
            input2.as_ptr() as *mut c_char,
            input2.as_bytes().len(),
            decoded2.as_mut_ptr() as *mut c_char,
            decoded2.capacity(),
            true,
            nchars2,
        )
    };

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
pub fn base64_encode_string(string: LispStringRef, no_line_break: bool) -> LispObject {
    match base64_encode_1(string.as_slice(), !no_line_break, string.is_multibyte()) {
        Ok(encoded) => unsafe {
            make_unibyte_string(encoded.as_ptr() as *const i8, encoded.len() as isize)
        },
        Err(_) => error!("Multibyte character in data for base64 encoding"),
    }
}

/// Base64-decode STRING and return the result.
#[lisp_fn]
pub fn base64_decode_string(mut string: LispStringRef) -> LispObject {
    let length = compute_decode_size(string.len_bytes() as usize);
    let mut buffer: Vec<c_char> = Vec::with_capacity(length);

    let decoded = buffer.as_mut_ptr();
    let decoded_length = unsafe {
        base64_decode_1(
            string.sdata_ptr(),
            string.len_bytes() as usize,
            decoded,
            length,
            false,
            ptr::null_mut(),
        )
    };

    if decoded_length < 0 {
        error!("Invalid base64 data");
    }
    unsafe { buffer.set_len(decoded_length as usize) };
    unsafe { make_unibyte_string(decoded, decoded_length) }
}

/// Base64-encode the region between BEG and END. Return the length of the encoded text. Optional
/// third argument NO-LINE-BREAK means do not break long lines into shorter lines.
#[lisp_fn(min = "2", intspec = "r")]
pub fn base64_encode_region(
    mut beg: LispObject,
    mut end: LispObject,
    no_line_break: bool,
) -> EmacsInt {
    unsafe { validate_region(&mut beg, &mut end) };
    let mut current_buffer = ThreadState::current_buffer();
    let old_pos = current_buffer.pt;

    let ibeg = beg.as_natnum_or_error() as isize;
    let begpos = unsafe { buf_charpos_to_bytepos(current_buffer.as_mut(), ibeg) };
    let iend = end.as_natnum_or_error() as isize;
    let endpos = unsafe { buf_charpos_to_bytepos(current_buffer.as_mut(), iend) };

    unsafe { move_gap_both(ibeg, begpos) };

    // Allocate room for the extra 33% plus newlines
    let length = (endpos - begpos) as usize;
    let input = unsafe { slice::from_raw_parts(current_buffer.byte_pos_addr(begpos), length) };

    let multibyte = current_buffer.multibyte_characters_enabled();
    let encoded = match base64_encode_1(input, !no_line_break, multibyte) {
        Ok(encoded) => encoded,
        Err(_) => error!("Multibyte character in data for base64 encoding"),
    };
    let encoded_length = encoded.len() as isize;

    // We now insert the new contents and delete the old in the region
    unsafe {
        set_point_both(begpos, ibeg);
        insert(encoded.as_ptr() as *const c_char, encoded_length);
        del_range_byte(begpos + encoded_length, endpos + encoded_length);
    }

    let pos_to_set = if old_pos >= iend {
        old_pos + encoded_length - (iend - ibeg)
    } else if old_pos > ibeg {
        old_pos - ibeg
    } else {
        old_pos
    };
    unsafe { set_point(pos_to_set) };

    encoded_length as i64
}

#[lisp_fn(intspec = "r")]
pub fn base64_decode_region(mut beg: LispObject, mut end: LispObject) -> EmacsInt {
    unsafe { validate_region(&mut beg, &mut end) };

    let mut current_buffer = ThreadState::current_buffer();
    let mut old_pos = current_buffer.pt;

    let ibeg = beg.as_natnum_or_error() as isize;
    let begpos = unsafe { buf_charpos_to_bytepos(current_buffer.as_mut(), ibeg) };
    let iend = end.as_natnum_or_error() as isize;
    let endpos = unsafe { buf_charpos_to_bytepos(current_buffer.as_mut(), iend) };

    let multibyte = current_buffer.multibyte_characters_enabled();
    let length = (endpos - begpos) as usize;
    let allength = if multibyte { length * 2 } else { length };

    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(allength as usize);
    let decoded = buffer.as_mut_ptr();
    let mut inserted_chars: libc::ptrdiff_t = 0;
    let decoded_length = unsafe {
        base64_decode_1(
            current_buffer.byte_pos_addr(begpos) as *const libc::c_char,
            length,
            decoded,
            allength,
            multibyte,
            &mut inserted_chars,
        )
    };

    if decoded_length < 0 {
        error!("Invalid base64 data");
    }

    // We've decoded it so insert the new contents and delete the old.
    unsafe {
        temp_set_point_both(current_buffer.as_mut(), ibeg, begpos);
        insert_1_both(decoded, inserted_chars, decoded_length, false, true, false);
        signal_after_change(ibeg, 0, inserted_chars);
        del_range_both(
            current_buffer.pt,
            current_buffer.pt_byte,
            iend + inserted_chars,
            endpos + decoded_length,
            true,
        );
    }

    if old_pos >= iend {
        old_pos += inserted_chars - (iend - ibeg);
    } else if old_pos > ibeg {
        old_pos = ibeg;
    }
    unsafe { set_point(max(current_buffer.zv, old_pos)) };

    inserted_chars as i64
}

include!(concat!(env!("OUT_DIR"), "/base64_exports.rs"));
