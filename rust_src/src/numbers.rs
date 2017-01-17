extern crate libc;

use std::os::raw::c_char;
use std::ptr;

use lisp::{LispObject, LispSubr, Qnil, Qt, FLOATP};

#[no_mangle]
pub fn Ffloatp(object: LispObject) -> LispObject {
    if FLOATP(object) {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("floatp",
       Ffloatp,
       Sfloatp,
       1, 1,
       ptr::null(),
       "Return t if OBJECT is a floating point number.

(fn OBJECT)");
