//! Lisp functions pertaining to editing.

use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys::EmacsInt;
use threads::ThreadState;
use buffers::get_buffer;


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
///
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
    LispObject::from_natnum((buffer_ref.z() - buffer_ref.beg()) as EmacsInt)
}

/// Return t if point is at the end of the buffer.
/// If the buffer is narrowed, this means the end of the narrowed part.
#[lisp_fn]
pub fn eobp() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_bool(buffer_ref.zv() == buffer_ref.pt)
}

/// Return t if point is at the beginning of the buffer.  If the
/// buffer is narrowed, this means the beginning of the narrowed part.
#[lisp_fn]
pub fn bobp() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_bool(buffer_ref.pt == buffer_ref.begv)
}

/// Return t if point is at the beginning of a line.
#[lisp_fn]
pub fn bolp() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_bool(
        buffer_ref.pt == buffer_ref.begv || buffer_ref.fetch_byte(buffer_ref.pt_byte - 1) == b'\n',
    )
}

/// Return t if point is at the end of a line.
///`End of a line' includes point being at the end of the buffer.
#[lisp_fn]
pub fn eolp() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_bool(
        buffer_ref.pt == buffer_ref.zv() || buffer_ref.fetch_byte(buffer_ref.pt_byte) == b'\n',
    )
}
