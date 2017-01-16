use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{LispObject, LispSubr, LispType, Qnil, Qt, XTYPE};

pub fn CONSP(x: LispObject) -> bool {
    XTYPE(x) == LispType::Lisp_Cons
}

fn Fatom(object: LispObject) -> LispObject {
if CONSP(object) {
    Qnil
} else {
    unsafe { Qt }
}
}

defun!("atom", Fatom, Satom, 1, 1, ptr::null(),
"Return t if OBJECT is not a cons cell.  This includes nil.");
