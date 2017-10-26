//! Routines to deal with category tables.

use remacs_macros::lisp_fn;
use remacs_sys::Qcategory_table;
use lisp::LispObject;
use threads::ThreadState;

/// Return t if ARG is a category table.
#[lisp_fn]
fn category_table_p(arg: LispObject) -> LispObject {
    LispObject::from_bool(arg.as_char_table().map_or(false, |table| {
        LispObject::from(table.purpose).eq(unsafe { LispObject::from(Qcategory_table) })
    }))
}

/// Return the current category table.
/// This is the one specified by the current buffer.
#[lisp_fn]
fn category_table() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from(buffer_ref.category_table)
}
