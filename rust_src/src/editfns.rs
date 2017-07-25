//! Lisp functions pertaining to editing.

use remacs_macros::lisp_fn;
use lisp::{LispObject};
use remacs_sys::{Fcurrent_buffer, EmacsInt};


/// Return value of point, as an integer.
#[lisp_fn]
pub fn point() -> LispObject {
    let buffer = LispObject::from_raw(unsafe { Fcurrent_buffer() });
    let b = buffer.as_vectorlike().unwrap().as_buffer().unwrap();
    LispObject::from_natnum(b.pt as EmacsInt)
}
