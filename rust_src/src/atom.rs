use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use cons::CONSP;
use lisp::{LispObject, LispSubr, Qnil, Qt, VectorLikeHeader, PvecType,
           PSEUDOVECTOR_AREA_BITS};


fn Fatom(object: LispObject) -> LispObject {
    if CONSP(object) {
        Qnil
    } else {
        unsafe { Qt }
    }
}

defun!("atom", Fatom, Satom, 1, 1, ptr::null(),
"Return t if OBJECT is not a cons cell.  This includes nil.");
