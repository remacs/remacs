use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{LispObject, LispSubr, Qnil, SBYTES, SSDATA, STRING_MULTIBYTE, STRINGP, CHECK_STRING};
use lists::NILP;

extern "C" {
    fn make_unibyte_string(s: *const libc::c_char, length: libc::ptrdiff_t) -> LispObject;
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

static MIME_LINE_LENGTH: isize = 76;

fn Fstringp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_string())
}

defun!("stringp",
       Fstringp,
       Sstringp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a string.

(fn OBJECT)");

fn Feq(firstObject: LispObject, secondObject: LispObject) -> LispObject {
    LispObject::from_bool(firstObject == secondObject)
}

defun!("eq",
       Feq,
       Seq,
       2,
       2,
       ptr::null(),
       "Return t if the two args are the same Lisp object.

(fn OBJECT OBJECT)");

fn Fnull(object: LispObject) -> LispObject {
    LispObject::from_bool(object == Qnil)
}

defun!("null",
       Fnull,
       Snull,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is nil, and return nil otherwise.

(fn OBJECT)");


fn Fbase64_encode_string(string: LispObject, noLineBreak: LispObject) -> LispObject {
    CHECK_STRING(string);

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
        let encodedLength = base64_encode_1(SSDATA(string),
                                            encoded,
                                            length,
                                            NILP(noLineBreak),
                                            STRING_MULTIBYTE(string));

        if encodedLength > allength {
            panic!("base64 encoded length is larger then allocated buffer");
        }

        if encodedLength < 0 {
            error("Multibyte character in data for base64 encoding\0".as_ptr());
        }

        make_unibyte_string(encoded, encodedLength)
    }
}

defun!("base64-encode-string",
       Fbase64_encode_string,
       Sbase64_encode_string,
       1,
       2,
       ptr::null(),
       "Base64-encode STRING and return the result.
       Optional second argument NO-LINE-BREAK means do not break long lines
       into shorter lines.

(fn STRING &optional NO-LINE-BREAK)");

fn Fbase64_decode_string(string: LispObject) -> LispObject {
    CHECK_STRING(string);

    let length = SBYTES(string);
    let mut buffer: Vec<libc::c_char> = Vec::with_capacity(length as usize);

    unsafe {
        let decoded = buffer.as_mut_ptr();
        let decoded_length = base64_decode_1(SSDATA(string), 
                                             decoded, 
                                             length, 
                                             false, 
                                             ptr::null_mut());

        let mut decoded_string: LispObject = Qnil;
        if decoded_length > length {
            panic!("Decoded length is above length");
        } else if decoded_length >= 0 {
            decoded_string = make_unibyte_string(decoded, decoded_length);
        }

        if !STRINGP(decoded_string) {
            error("Invalid base64 data\0".as_ptr());
        }

        decoded_string
    }
}

defun!("base64-decode-string",
       Fbase64_decode_string,
       Sbase64_decode_string,
       1,
       1,
       ptr::null(),
       "Base64-decode STRING and return the result.

(fn STRING)");
