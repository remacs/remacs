use lisp::LispObject;
use remacs_macros::lisp_fn;
use remacs_sys::{set_point, EmacsInt, Fline_beginning_position};
use threads::ThreadState;

/// Return buffer position N characters after (before if N negative) point.
#[lisp_fn]
pub fn forward_point(n: LispObject) -> LispObject {
    let pt = ThreadState::current_buffer().pt();
    LispObject::from_fixnum(n.as_fixnum_or_error() + pt as EmacsInt)
}

/// Move point to beginning of current line (in the logical order).
/// With argument N not nil or 1, move forward N - 1 lines first.
/// If point reaches the beginning or end of buffer, it stops there.
/// This function constrains point to the current field unless this moves
/// point to a different line than the original, unconstrained result.
/// If N is nil or 1, and a front-sticky field starts at point, the point
/// does not move.  To ignore field boundaries bind
/// `inhibit-field-text-motion' to t, or use the `forward-line' function
/// instead.  For instance, `(forward-line 0)' does the same thing as
/// `(beginning-of-line)', except that it ignores field boundaries.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn beginning_of_line(mut n: LispObject) -> LispObject {
    if n.is_nil() {
        n = LispObject::from_fixnum(1);
    }

    unsafe {
        let pos = LispObject::from(Fline_beginning_position(n.to_raw()));
        set_point(pos.as_fixnum_or_error() as isize);
    };

    LispObject::constant_nil()
}
