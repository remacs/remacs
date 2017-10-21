use lisp::LispObject;
use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Fline_beginning_position, set_point};
use threads::ThreadState;

/// Return buffer position N characters after (before if N negative) point.
#[lisp_fn]
pub fn forward_point(n: LispObject) -> LispObject {
    let pt = ThreadState::current_buffer().pt();
    LispObject::from_fixnum(n.as_fixnum_or_error() + pt as EmacsInt)
}

#[lisp_fn]
pub fn test_b(n: LispObject) -> LispObject {
    let pos = unsafe { Fline_beginning_position(n.as_fixnum_or_error()) };
    //unsafe { set_point(pos as isize) };
    LispObject::constant_nil()
}
