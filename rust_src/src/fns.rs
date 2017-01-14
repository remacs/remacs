extern crate libc;

use std::ptr;
use std::os::raw::c_char;
use lisp::{LispSubr, PSEUDOVECTOR_AREA_BITS, PvecType, VectorLikeHeader, LispObject, SBYTES};
use strings::STRINGP;
use cons::NILP;

static MIME_LINE_LENGTH: isize = 76;

extern "C" {
    fn make_unibyte_string(s: *const libc::c_char, length: libc::ptrdiff_t) -> LispObject;
    fn base64_encode_1(from: *const libc::c_char, to: *mut libc::c_char, length: libc::ptrdiff_t,
                       line_break: bool, multibyte: bool) -> libc::ptrdiff_t;
    fn STRING_MULTIBYTE(a: LispObject) -> bool;
    fn SSDATA(string: LispObject) -> *mut libc::c_char;
}

pub fn Base64EncodeString (string: LispObject, noLineBreak: LispObject) -> LispObject {
    debug_assert!(STRINGP(string));
    let length = SBYTES(string);
    let mut allength: libc::ptrdiff_t = length + length / 3 + 1;
    allength += allength / MIME_LINE_LENGTH + 1 + 6;
    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(allength as usize);
    unsafe {
        let encoded = buffer.as_mut_ptr();
        let encodedLength = base64_encode_1(SSDATA(string), encoded, length,
                                            NILP(noLineBreak), STRING_MULTIBYTE(string));
        debug_assert!(encodedLength <= allength);
        make_unibyte_string(encoded, encodedLength)
    }
}

defun!("base64-encode-string", Base64EncodeString, Sbase64EncodeString, 2, 2, ptr::null(),
       "Base64-encode STRING and return the result.
       Optional second argument NO-LINE-BREAK means do not break long lines
       into shorter lines.");
