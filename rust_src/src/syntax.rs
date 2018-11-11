//! Functions related to syntax

use crate::remacs_sys::{buffer_defaults, scan_lists, set_char_table_defalt};
use crate::remacs_sys::{EmacsInt, Qnil, Qsyntax_table, Qsyntax_table_p};
use crate::remacs_sys::{Fcopy_sequence, Fset_char_table_parent};
use remacs_macros::lisp_fn;

use crate::chartable::LispCharTableRef;
use crate::lisp::defsubr;
use crate::lisp::LispObject;
use crate::threads::ThreadState;

/// Return the current syntax table. This is the one specified by the
/// current buffer.
#[lisp_fn]
pub fn syntax_table() -> LispObject {
    ThreadState::current_buffer().syntax_table_
}

/// Scan from character number FROM by COUNT lists.
/// Scan forward if COUNT is positive, backward if COUNT is negative.
/// Return the character number of the position thus found.
///
/// A "list", in this context, refers to a balanced parenthetical
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
    unsafe { scan_lists(from, count, depth, false) }
}

/// Select a new syntax table for the current buffer.
/// One argument, a syntax table.
#[lisp_fn]
pub fn set_syntax_table(table: LispCharTableRef) -> LispCharTableRef {
    check_syntax_table_p(table);
    let mut buf = ThreadState::current_buffer();
    buf.set_syntax_table(table);
    let idx = per_buffer_var_idx!(syntax_table_);
    buf.set_per_buffer_value_p(idx, 1);
    table
}

fn check_syntax_table_p(table: LispCharTableRef) {
    if table.purpose != Qsyntax_table {
        wrong_type!(Qsyntax_table_p, LispObject::from(table))
    }
}

/// Return the standard syntax table.  This is the one used for new buffers.
#[lisp_fn]
pub fn standard_syntax_table() -> LispCharTableRef {
    unsafe { buffer_defaults.syntax_table_ }.into()
}

#[no_mangle]
pub extern "C" fn check_syntax_table(obj: LispObject) {
    if obj
        .as_char_table()
        .map_or(true, |c| !c.purpose.eq(Qsyntax_table))
    {
        xsignal!(Qsyntax_table_p, obj);
    }
}

/// Construct a new syntax table and return it.
/// It is a copy of the TABLE, which defaults to the standard syntax table.
#[lisp_fn(min = "0")]
pub fn copy_syntax_table(mut table: LispObject) -> LispObject {
    let buffer_table = unsafe { buffer_defaults.syntax_table_ };
    if table.is_not_nil() {
        check_syntax_table(table);
    } else {
        table = buffer_table;
    }
    let copy = unsafe { Fcopy_sequence(table) };

    // Only the standard syntax table should have a default element.
    // Other syntax tables should inherit from parents instead.
    unsafe { set_char_table_defalt(copy, Qnil) };

    // Copied syntax tables should all have parents.
    // If we copied one with no parent, such as the standard syntax table,
    // use the standard syntax table as the copy's parent.
    if copy.as_char_table_or_error().parent.is_nil() {
        unsafe { Fset_char_table_parent(copy, buffer_table) };
    }
    copy
}

include!(concat!(env!("OUT_DIR"), "/syntax_exports.rs"));
