//! Base64 de- and encoding functions.
use std::{cmp::max, slice};

use libc::{c_char, c_uchar};
use line_wrap::LineEnding;

use remacs_macros::lisp_fn;

use crate::{
    base64_crate,
    buffers::validate_region_rust,
    lisp::LispObject,
    multibyte::{multibyte_char_at, raw_byte_from_codepoint, LispStringRef, MAX_5_BYTE_CHAR},
    remacs_sys::EmacsInt,
    remacs_sys::{
        del_range_both, del_range_byte, insert, insert_1_both, make_unibyte_string, move_gap_both,
        set_point, set_point_both, signal_after_change, temp_set_point_both,
    },
    threads::ThreadState,
};

fn base64_encode_1(bytes: &[u8], line_break: bool, multibyte: bool) -> Result<String, ()> {
    let config = base64_crate::STANDARD;

    let mut encoded_string = if multibyte {
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

    if line_break {
        line_wrap(&mut encoded_string, 76, &line_wrap::lf());
    }

    Ok(encoded_string)
}

/// Insert LINE_ENDING into the STRING per LINE_LEN bytes.
fn line_wrap<L: LineEnding>(string: &mut String, line_len: usize, line_ending: &L) {
    let capacity = string.capacity();
    let input_len = string.len();
    // This overestimating of the number of lines with ending possibly result in allocating
    // `line_ending.len()` bytes more than needed, but leave it as is for now.
    let lines_with_ending = input_len / line_len + 1;

    let mut raw_vec = unsafe { string.as_mut_vec() };

    if let Some(adding) = (lines_with_ending * (line_len + line_ending.len())).checked_sub(capacity)
    {
        raw_vec.resize(input_len + adding, 0);
    }
    let added = line_wrap::line_wrap(&mut raw_vec, input_len, line_len, line_ending);
    raw_vec.truncate(input_len + added);
}

/// Base64-decode the data in ENCODED. If MULTIBYTE, the decoded result should be in multibyte
/// form. It returns the decoded data and the number of bytes in the original decoded string.
fn base64_decode_1(encoded: &[u8], multibyte: bool) -> Result<(Vec<u8>, usize), ()> {
    // Input string is allowed to have emmbed newlines, delete before decoding.
    let mut buf: Vec<u8> = Vec::with_capacity(encoded.len());
    buf.extend(encoded.iter().filter(|b| !b"\n\t\r\x0b\x0c".contains(b)));

    match base64_crate::decode_config(&buf, base64_crate::STANDARD) {
        Ok(decoded) => {
            if multibyte {
                // Decode non-ASCII bytes into UTF-8 pairs.
                let nchars = decoded.len();
                let s = encode_multibyte_string(&decoded);
                Ok((s, nchars))
            } else {
                let len = decoded.len();
                Ok((decoded, len))
            }
        }
        _ => Err(()),
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
fn test_multibyte_base64_encode_1() {
    let input = "Dobrý den"; // Czech

    // Treat the input as unibyte, meaning just a buffer of bytes
    let encoded = base64_encode_1(input.as_bytes(), false, false).unwrap();
    assert_eq!("RG9icsO9IGRlbg==", encoded);

    // When we specify 'mutlibyte' we mean the input is encoded with emacs' own encoding
    let as_multibyte = encode_multibyte_string(input.as_bytes());
    let encoded = base64_encode_1(&as_multibyte, false, true).unwrap();
    assert_eq!("RG9icsO9IGRlbg==", encoded);
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

#[test]
fn test_simple_base64_decode_1() {
    let input = "aGVsbG8gd29ybGQ=";
    let clear = "hello world";

    let (decoded, nchars) = base64_decode_1(input.as_bytes(), true).unwrap();
    assert_eq!(clear.len(), nchars);
    assert_eq!(clear, String::from_utf8(decoded).unwrap());
}

#[test]
fn test_multibyte_base64_decode_1() {
    let input = "RG9icsO9IGRlbg==";
    let clear = "Dobrý den"; // Czech

    // When we specify multibyte we want the return to be encoded with bytes/chars > 128 using
    // emacs' own encoding

    let (decoded, nchars) = base64_decode_1(input.as_bytes(), true).unwrap();

    let decoded_multibyte = vec![68, 111, 98, 114, 193, 131, 192, 189, 32, 100, 101, 110];

    assert_eq!(clear.len(), nchars);
    assert_eq!(decoded_multibyte, decoded);
    assert_eq!(encode_multibyte_string(clear.as_bytes()), decoded_multibyte);

    // Now run again, but disable multibyte so we get the unchanged result of base64-decoding

    let (decoded, nchars) = base64_decode_1(input.as_bytes(), false).unwrap();

    assert_eq!(clear.len(), nchars);
    assert_eq!(clear.as_bytes(), decoded.as_slice());

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

    let (decoded, nchars) = base64_decode_1(input.as_bytes(), true).unwrap();

    // We don't round-trip on multibyte decode but use a particular encoding
    assert_eq!(clear.len(), nchars);
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

    let (decoded1, _) = base64_decode_1(input1.as_bytes(), true).unwrap();
    let (decoded2, _) = base64_decode_1(input2.as_bytes(), true).unwrap();

    assert_eq!(decoded1.len(), decoded2.len());
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
            make_unibyte_string(encoded.as_ptr() as *const c_char, encoded.len() as isize)
        },
        Err(_) => error!("Multibyte character in data for base64 encoding"),
    }
}

/// Base64-decode STRING and return the result.
#[lisp_fn]
pub fn base64_decode_string(string: LispStringRef) -> LispObject {
    let decoded = match base64_decode_1(string.as_slice(), false) {
        Ok((decoded, _)) => decoded,
        Err(_) => error!("Invalid base64 data"),
    };

    unsafe { make_unibyte_string(decoded.as_ptr() as *const c_char, decoded.len() as isize) }
}

/// Base64-encode the region between BEG and END. Return the length of the encoded text. Optional
/// third argument NO-LINE-BREAK means do not break long lines into shorter lines.
#[lisp_fn(min = "2", intspec = "r")]
pub fn base64_encode_region(beg: LispObject, end: LispObject, no_line_break: bool) -> EmacsInt {
    let (beg, end) = validate_region_rust(beg, end);
    let current_buffer = ThreadState::current_buffer_unchecked();
    let old_pos = current_buffer.pt;

    let begpos = current_buffer.charpos_to_bytepos(beg);
    let endpos = current_buffer.charpos_to_bytepos(end);

    unsafe { move_gap_both(beg, begpos) };

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
        set_point_both(begpos, beg);
        insert(encoded.as_ptr() as *const c_char, encoded_length);
        del_range_byte(begpos + encoded_length, endpos + encoded_length);
    }

    let pos_to_set = if old_pos >= end {
        old_pos + encoded_length - (end - beg)
    } else if old_pos > beg {
        old_pos - beg
    } else {
        old_pos
    };
    unsafe { set_point(pos_to_set) };

    encoded_length as i64
}

#[lisp_fn(intspec = "r")]
pub fn base64_decode_region(beg: LispObject, end: LispObject) -> EmacsInt {
    let (beg, end) = validate_region_rust(beg, end);

    let mut current_buffer = ThreadState::current_buffer_unchecked();
    let mut old_pos = current_buffer.pt;

    let begpos = current_buffer.charpos_to_bytepos(beg);
    let endpos = current_buffer.charpos_to_bytepos(end);

    let multibyte = current_buffer.multibyte_characters_enabled();
    let length = (endpos - begpos) as usize;

    let input = unsafe { slice::from_raw_parts(current_buffer.byte_pos_addr(begpos), length) };
    let (decoded, nchars) = match base64_decode_1(input, multibyte) {
        Ok(decoded) => decoded,
        Err(_) => error!("Invalid base64 data"),
    };

    let decoded_length = decoded.len() as libc::ptrdiff_t;
    let inserted_chars = nchars as libc::ptrdiff_t;

    // We've decoded it so insert the new contents and delete the old.
    unsafe {
        temp_set_point_both(current_buffer.as_mut(), beg, begpos);
        insert_1_both(
            decoded.as_ptr() as *const c_char,
            inserted_chars,
            decoded_length,
            false,
            true,
            false,
        );
        signal_after_change(beg, 0, inserted_chars);
        del_range_both(
            current_buffer.pt,
            current_buffer.pt_byte,
            end + inserted_chars,
            endpos + decoded_length,
            true,
        );
    }

    if old_pos >= end {
        old_pos += inserted_chars - (end - beg);
    } else if old_pos > beg {
        old_pos = beg;
    }
    unsafe { set_point(max(current_buffer.zv, old_pos)) };

    inserted_chars as i64
}

include!(concat!(env!("OUT_DIR"), "/base64_exports.rs"));
