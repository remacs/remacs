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

//Move point to beginning of current line (in the logical order).
//With argument N not nil or 1, move forward N - 1 lines first.
//If point reaches the beginning or end of buffer, it stops there.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn beginning_of_line(mut n: LispObject) -> LispObject {

    if n.is_nil()
        n = LispObject::from_fixnum(1);

    let pos = unsafe {
        LispObject::from_raw(Fline_beginning_position(n.to_raw()))
    };
    unsafe { set_point(pos.as_fixnum_or_error() as isize) };
    LispObject::constant_nil()
}
