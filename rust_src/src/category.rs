//! Routines to deal with category tables.

use remacs_macros::lisp_fn;
use remacs_sys::Qcategory_table;

use lisp::LispObject;
use lisp::defsubr;
use threads::ThreadState;

/// Return t if ARG is a category table.
#[lisp_fn]
pub fn category_table_p(arg: LispObject) -> bool {
    arg.as_char_table().map_or(false, |table| {
        LispObject::from_raw(table.purpose).eq(LispObject::from_raw(Qcategory_table))
    })
}

/// Return the current category table.
/// This is the one specified by the current buffer.
#[lisp_fn]
pub fn category_table() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    LispObject::from_raw(buffer_ref.category_table)
}

include!("category_exports.rs");
