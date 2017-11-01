//! Commands

use remacs_macros::lisp_fn;
use remacs_sys::EmacsInt;

use lisp::LispObject;
use lisp::defsubr;

use threads::ThreadState;

/// Return buffer position N characters after (before if N negative) point.
#[lisp_fn]
pub fn forward_point(n: LispObject) -> LispObject {
    let pt = ThreadState::current_buffer().pt();
    LispObject::from_fixnum(n.as_fixnum_or_error() + pt as EmacsInt)
}

export_lisp_fns! {
    forward_point
}
