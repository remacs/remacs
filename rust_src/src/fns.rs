extern crate libc;
extern crate rustc_serialize;

use self::rustc_serialize::base64::{ToBase64, STANDARD};

use std::ptr;
use lisp;
use std::os::raw::c_char;
use libc::ptrdiff_t;
use strings;
use lisp::{LispSubr, PSEUDOVECTOR_AREA_BITS, PvecType, VectorLikeHeader, LispObject, XUNTAG, LispType,
          LispString, XSTRING};
use std::ffi::CString;
use std::ffi::IntoStringError;

extern "C" {
    fn build_unibyte_string(s: *const libc::c_char) -> LispObject;
}

fn Base64EncodeString (object: LispObject, noLineBreak: LispObject) -> LispObject {
    // let strPointer = XSTRING(object);
    unsafe {
//        build_unibyte_string(CString::new(base64Str).unwrap().into_raw())
        build_unibyte_string(("foobar".as_ptr()) as *const c_char)
    }
}

lazy_static! {
    pub static ref Sbase64EncodeString: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Base64EncodeString as *const libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("base64-encode-string\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Base64-encode STRING and return the result.
Optional second argument NO-LINE-BREAK means do not break long lines
into shorter lines.".as_ptr()) as *const c_char,
    };
}
