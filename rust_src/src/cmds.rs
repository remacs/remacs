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

#[lisp_fn(min = "0")]
pub fn beginning_of_line(n: LispObject) -> LispObject {

    let x = if n.is_nil() {
        LispObject::from_fixnum(1)
    } else {
        n
    };

    let pos = unsafe {
        LispObject::from_raw(Fline_beginning_position(x.to_raw()))
    };
    unsafe { set_point(pos.as_fixnum_or_error() as isize) };
    LispObject::constant_nil()
}
