//! Functions related to syntax

use remacs_macros::lisp_fn;

use crate::{
    chartable::LispCharTableRef,
    editfns::constrain_to_field,
    lisp::LispObject,
    numbers::LispNumber,
    remacs_sys::{
        buffer_defaults, scan_lists, scan_words, set_char_table_defalt, set_point, skip_chars,
        skip_syntaxes,
    },
    remacs_sys::{EmacsInt, Qnil, Qsyntax_table, Qsyntax_table_p},
    remacs_sys::{Fcopy_sequence, Fset_char_table_parent},
    threads::ThreadState,
};

/// Return the current syntax table. This is the one specified by the
/// current buffer.
#[lisp_fn]
pub fn syntax_table() -> LispObject {
    ThreadState::current_buffer_unchecked().syntax_table_
}

/// Return t if OBJECT is a syntax table.
/// Currently, any char-table counts as a syntax table.
#[lisp_fn]
pub fn syntax_table_p(object: LispObject) -> bool {
    object
        .as_char_table()
        .map_or(false, |v| v.purpose == Qsyntax_table)
}

def_lisp_sym!(Qsyntax_table_p, "syntax-table-p");

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
#[lisp_fn(name = "scan-lists", c_name = "scan_lists")]
pub fn scan_lists_lisp(from: EmacsInt, count: EmacsInt, depth: EmacsInt) -> LispObject {
    unsafe { scan_lists(from, count, depth, false) }
}

/// Select a new syntax table for the current buffer.
/// One argument, a syntax table.
#[lisp_fn]
pub fn set_syntax_table(table: LispCharTableRef) -> LispCharTableRef {
    check_syntax_table_p(table);
    let mut buf = ThreadState::current_buffer_unchecked();
    buf.set_syntax_table(table);
    let idx = per_buffer_var_idx!(syntax_table_);
    buf.set_per_buffer_value_p(idx, 1);
    table
}

fn check_syntax_table_p(table: LispCharTableRef) {
    if table.purpose != Qsyntax_table {
        wrong_type!(Qsyntax_table_p, table)
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
        wrong_type!(Qsyntax_table_p, obj);
    }
}

/// Construct a new syntax table and return it.
/// It is a copy of the TABLE, which defaults to the standard syntax table.
#[lisp_fn(min = "0")]
pub fn copy_syntax_table(mut table: LispObject) -> LispCharTableRef {
    let buffer_table = unsafe { buffer_defaults.syntax_table_ };
    if table.is_not_nil() {
        check_syntax_table(table);
    } else {
        table = buffer_table;
    }
    let copy: LispCharTableRef = unsafe { Fcopy_sequence(table) }.into();

    // Only the standard syntax table should have a default element.
    // Other syntax tables should inherit from parents instead.
    unsafe { set_char_table_defalt(copy.into(), Qnil) };

    // Copied syntax tables should all have parents.
    // If we copied one with no parent, such as the standard syntax table,
    // use the standard syntax table as the copy's parent.
    if copy.parent.is_nil() {
        unsafe { Fset_char_table_parent(copy.into(), buffer_table) };
    }
    copy
}

/// Move point forward ARG words (backward if ARG is negative).
/// If ARG is omitted or nil, move point forward one word.
/// Normally returns t.
/// If an edge of the buffer or a field boundary is reached, point is
/// left there and the function returns nil.  Field boundaries are not
/// noticed if `inhibit-field-text-motion' is non-nil.
///
/// The word boundaries are normally determined by the buffer's syntax
/// table, but `find-word-boundary-function-table', such as set up
/// by `subword-mode', can change that.  If a Lisp program needs to
/// move by words determined strictly by the syntax table, it should
/// use `forward-word-strictly' instead.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn forward_word(arg: Option<EmacsInt>) -> bool {
    let arg = arg.unwrap_or(1);
    let cur_buf = ThreadState::current_buffer_unchecked();
    let point = cur_buf.pt;

    let (mut val, orig_val) = match unsafe { scan_words(point, arg) } {
        0 => {
            let val = if arg > 0 { cur_buf.zv } else { cur_buf.begv };
            (val, 0)
        }
        n => (n, n),
    };
    // Avoid jumping out of an input field.
    val = constrain_to_field(
        Some(LispNumber::Fixnum(val as EmacsInt)),
        LispNumber::Fixnum(point as EmacsInt),
        false,
        false,
        Qnil,
    ) as isize;
    unsafe { set_point(val) };
    val == orig_val
}

/// Move point forward, stopping before a char not in STRING, or at pos LIM.
/// STRING is like the inside of a `[...]' in a regular expression
/// except that `]' is never special and `\\' quotes `^', `-' or `\\'
///  (but not at the end of a range; quoting is never needed there).
/// Thus, with arg "a-zA-Z", this skips letters stopping before first nonletter.
/// With arg "^a-zA-Z", skips nonletters stopping before first letter.
/// Char classes, e.g. `[:alpha:]', are supported.
///
/// Returns the distance traveled, either zero or positive.
#[lisp_fn(min = "1")]
pub fn skip_chars_forward(string: LispObject, lim: LispObject) -> LispObject {
    unsafe { skip_chars(true, string, lim, true) }
}

/// Move point backward, stopping after a char not in STRING, or at pos LIM.
/// See `skip-chars-forward' for details.
/// Returns the distance traveled, either zero or negative.
#[lisp_fn(min = "1")]
pub fn skip_chars_backward(string: LispObject, lim: LispObject) -> LispObject {
    unsafe { skip_chars(false, string, lim, true) }
}

/// Move point forward across chars in specified syntax classes.
/// SYNTAX is a string of syntax code characters.
/// Stop before a char whose syntax is not in SYNTAX, or at position LIM.
/// If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
/// This function returns the distance traveled, either zero or positive.
#[lisp_fn(min = "1")]
pub fn skip_syntax_forward(syntax: LispObject, lim: LispObject) -> LispObject {
    unsafe { skip_syntaxes(true, syntax, lim) }
}

/// Move point backward across chars in specified syntax classes.
/// SYNTAX is a string of syntax code characters.
/// Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
/// If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
/// This function returns either zero or a negative number, and the absolute value
/// of this is the distance traveled.
#[lisp_fn(min = "1")]
pub fn skip_syntax_backward(syntax: LispObject, lim: LispObject) -> LispObject {
    unsafe { skip_syntaxes(false, syntax, lim) }
}

include!(concat!(env!("OUT_DIR"), "/syntax_exports.rs"));
