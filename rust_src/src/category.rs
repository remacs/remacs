//! Routines to deal with category tables.

use crate::{
    chartable::LispCharTableRef, lisp::LispObject, remacs_sys::Qcategory_table,
    threads::ThreadState,
};

use remacs_macros::lisp_fn;

/// Return t if ARG is a category table.
#[lisp_fn]
pub fn category_table_p(arg: Option<LispCharTableRef>) -> bool {
    arg.map_or(false, |table| table.purpose.eq(Qcategory_table))
}

/// Return the current category table.
/// This is the one specified by the current buffer.
#[lisp_fn]
pub fn category_table() -> LispObject {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.category_table_
}

include!(concat!(env!("OUT_DIR"), "/category_exports.rs"));
