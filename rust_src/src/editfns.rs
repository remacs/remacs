//! Lisp functions pertaining to editing.

use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys::EmacsInt;
use threads::ThreadState;


/// Return value of point, as an integer.
/// Beginning of buffer is position (point-min).
#[lisp_fn]
pub fn point() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_natnum(buffer_ref.pt as EmacsInt)
}
