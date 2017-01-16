use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{LispObject, LispSubr, Qnil, STRINGP};

extern "C" {
    static Qt: LispObject;
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

fn Fnull(object: LispObject) -> LispObject {
    if object == Qnil {
        unsafe {
            Qt
        }
    } else {
        Qnil
    }
}

defun!("null", Fnull, Snull, 1, 1, ptr::null(), "Return t if OBJECT is nil, and return nil otherwise.

(fn OBJECT)");
