use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{LispObject, LispSubr};

fn Fsymbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}

defun!("symbolp",
       Fsymbolp,
       Ssymbolp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a symbol.

(fn OBJECT)");
