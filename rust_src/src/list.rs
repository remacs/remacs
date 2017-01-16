use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{LispObject, LispSubr, Qnil, Qt};
use cons::{CONSP, NILP};

fn Fatom(object: LispObject) -> LispObject {
    if CONSP(object) {
        Qnil
    } else {
        unsafe { Qt }
    }
}

defun!("atom", Fatom, Satom, 1, 1, ptr::null(),
"Return t if OBJECT is not a cons cell.  This includes nil. (fn OBJECT)");

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



#[no_mangle]
pub extern "C" fn Flistp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("listp",
       Flistp,
       Slistp,
       1, 1,
       ptr::null(),
       "Return t if OBJECT is a list, that is, a cons cell or nil. Otherwise, return nil.

(fn OBJECT)");

fn Fnlistp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP (object) {
        Qnil
    } else {
        unsafe { Qt }
    }
}

defun!("nlistp",
       Fnlistp,
       Snlistp,
       1, 1,
       ptr::null(),
       "Return t if OBJECT is not a list.  Lists include nil.

(fn OBJECT)");
