//! char table related functions

use remacs_macros::lisp_fn;
use remacs_sys::Lisp_Char_Table;

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

pub type LispCharTableRef = ExternalPtr<Lisp_Char_Table>;

/// Return the subtype of char-table CHARTABLE.  The value is a symbol.
#[lisp_fn]
pub fn char_table_subtype(chartable: LispObject) -> LispObject {
    let table = chartable.as_char_table_or_error();
    LispObject::from_raw(table.purpose)
}

/// Return the parent char-table of CHARTABLE.
/// The value is either nil or another char-table.
/// If CHAR-TABLE holds nil for a given character,
/// then the actual applicable value is inherited from the parent char-table
/// (or from its parents, if necessary).
#[lisp_fn]
pub fn char_table_parent(chartable: LispObject) -> LispObject {
    let table = chartable.as_char_table_or_error();
    LispObject::from_raw(table.parent)
}

/// Set the parent char-table of CHARTABLE to PARENT.
/// Return PARENT.  PARENT must be either nil or another char-table.
#[lisp_fn]
pub fn set_char_table_parent(chartable: LispObject, parent: LispObject) -> LispObject {
    let mut curr_table = chartable.as_char_table_or_error();

    if parent.is_not_nil() {
        let mut temp = parent;
        while temp.is_not_nil() {
            if chartable.eq(temp) {
                error!("Attempt to make a chartable to be its own parent");
            }
            temp = char_table_parent(temp);
        }
    }

    curr_table.parent = parent.to_raw();
    parent
}

include!(concat!(env!("OUT_DIR"), "/chartable_exports.rs"));
