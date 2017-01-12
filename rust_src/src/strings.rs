use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{XTYPE, LispObject, LispType, LispSubr, Qnil, VectorLikeHeader, PvecType,
           PSEUDOVECTOR_AREA_BITS};

extern "C" {
    static Qt: LispObject;
}

fn STRINGP(value: LispObject) -> bool {
    XTYPE(value) == LispType::Lisp_String
}

fn Fstringp(object: LispObject) -> LispObject {
    if STRINGP(object) { unsafe { Qt } } else { Qnil }
}

lazy_static! {
    pub static ref Sstringp: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fstringp as *const libc::c_void),
        min_args: 1,
        max_args: 1,
        symbol_name: ("stringp\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return t if OBJECT is a string.

(fn OBJECT)\0".as_ptr()) as *const c_char,
    };
}

fn Feq (firstObject: LispObject, secondObject: LispObject) -> LispObject {
    if firstObject == secondObject {
        unsafe {
            Qt
        }
    } else {
        Qnil
    }
}

lazy_static! {
    pub static ref Seq: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Feq as *const libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("eq\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return t if the two args are the same Lisp object.

(fn OBJECT OBJECT)\0".as_ptr()) as *const c_char,
    };
}
