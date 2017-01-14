use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{XTYPE, LispObject, LispType, LispSubr, Qnil, VectorLikeHeader, PvecType,
           PSEUDOVECTOR_AREA_BITS};

extern "C" {
    static Qt: LispObject;
}

pub fn STRINGP(value: LispObject) -> bool {
    XTYPE(value) == LispType::Lisp_String
}

fn Fstringp(object: LispObject) -> LispObject {
    if STRINGP(object) { unsafe { Qt } } else { Qnil }
}

defun!("stringp", Fstringp, Sstringp, 1, 1, ptr::null(), "Return t if OBJECT is a string.

(fn OBJECT)");

fn Feq (firstObject: LispObject, secondObject: LispObject) -> LispObject {
    if firstObject == secondObject {
        unsafe {
            Qt
        }
    } else {
        Qnil
    }
}

defun!("eq", Feq, Seq, 2, 2, ptr::null(), "Return t if the two args are the same Lisp object.

(fn OBJECT OBJECT)");
