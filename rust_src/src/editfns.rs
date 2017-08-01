//! Lisp functions pertaining to editing.

use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys::EmacsInt;
use threads::ThreadState;
use buffers::{get_buffer, BEG_BYTE};


/// Return value of point, as an integer.
/// Beginning of buffer is position (point-min).
#[lisp_fn]
pub fn point() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_natnum(buffer_ref.pt as EmacsInt)
}

#[lisp_fn(min = "0")]
pub fn buffer_size(object: LispObject) -> LispObject {
    let buffer_ref = if object.is_not_nil() {
        get_buffer(object).as_buffer_or_error()
    } else {
        ThreadState::current_buffer()
    };
    LispObject::from_natnum((buffer_ref.z_byte() - BEG_BYTE) as EmacsInt)
}
