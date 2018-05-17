use remacs_macros::lisp_fn;
use remacs_sys::{scan_lists, EmacsInt};
use threads::ThreadState;

use lisp::LispObject;
use lisp::defsubr;

/// Return the current syntax table. This is the one specified by the
/// current buffer.
#[lisp_fn]
pub fn syntax_table() -> LispObject {
    LispObject::from_raw(ThreadState::current_buffer().syntax_table)
}

/// Scan from character number FROM by COUNT lists.
/// Scan forward if COUNT is positive, backward if COUNT is negative.
/// Return the character number of the position thus found.
///
/// A \"list", in this context, refers to a balanced parenthetical
/// grouping, as determined by the syntax table.
///
/// If DEPTH is nonzero, treat that as the nesting depth of the starting
/// point (i.e. the starting point is DEPTH parentheses deep).  This
/// function scans over parentheses until the depth goes to zero COUNT
/// times.  Hence, positive DEPTH moves out that number of levels of
/// parentheses, while negative DEPTH moves to a deeper level.
///
/// Comments are ignored if `parse-sexp-ignore-comments' is non-nil.
///
/// If we reach the beginning or end of the accessible part of the buffer
/// before we have scanned over COUNT lists, return nil if the depth at
/// that point is zero, and signal a error if the depth is nonzero.

// We don't name it scan_lists because there is an internal function
// with the same name
#[lisp_fn(name = "scan-lists")]
pub fn scan_lists_defun(from: EmacsInt, count: EmacsInt, depth: EmacsInt) -> LispObject {
    LispObject::from_raw(unsafe { scan_lists(from, count, depth, false) })
}

include!(concat!(env!("OUT_DIR"), "/syntax_exports.rs"));
