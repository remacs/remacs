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

/// Return the number of characters in the current buffer.
/// If BUFFER is not nil, return the number of characters in that buffer
/// instead.

/// This does not take narrowing into account; to count the number of
/// characters in the accessible portion of the current buffer, use
/// `(- (point-max) (point-min))', and to count the number of characters
/// in some other BUFFER, use
/// `(with-current-buffer BUFFER (- (point-max) (point-min)))'.
#[lisp_fn(min = "0")]
pub fn buffer_size(object: LispObject) -> LispObject {
    let buffer_ref = if object.is_not_nil() {
        get_buffer(object).as_buffer_or_error()
    } else {
        ThreadState::current_buffer()
    };
    LispObject::from_natnum((buffer_ref.z() - BEG_BYTE) as EmacsInt)
}

/// Return t if point is at the end of the buffer.
/// If the buffer is narrowed, this means the end of the narrowed part.
#[lisp_fn]
pub fn eobp() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    let zv = LispObject::from_natnum(buffer_ref.zv() as EmacsInt);
    if point() == zv {
        LispObject::constant_t()
    } else {
        LispObject::constant_nil()
    }
}
