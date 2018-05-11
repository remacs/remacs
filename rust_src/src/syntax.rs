use remacs_macros::lisp_fn;
use threads::ThreadState;

use lisp::defsubr;
use lisp::LispObject;

/// Return the current syntax table. This is the one specified by the
/// current buffer.
#[lisp_fn]
pub fn syntax_table() -> LispObject {
    LispObject::from_raw(ThreadState::current_buffer().syntax_table)
}

include!(concat!(env!("OUT_DIR"), "/syntax_exports.rs"));