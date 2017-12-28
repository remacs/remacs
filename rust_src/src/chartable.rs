//! char table related functions

use remacs_macros::lisp_fn;
use remacs_sys::{Lisp_Char_Table, Lisp_Type};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use std::mem;

pub type LispCharTableRef = ExternalPtr<Lisp_Char_Table>;

impl LispCharTableRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)) }
    }
}

/// Return the subtype of char-table CHARTABLE.  The value is a symbol.
#[lisp_fn]
pub fn char_table_subtype(chartable: LispCharTableRef) -> LispObject {
    LispObject::from_raw(chartable.purpose)
}

/// Return the parent char-table of CHARTABLE.
/// The value is either nil or another char-table.
/// If CHAR-TABLE holds nil for a given character,
/// then the actual applicable value is inherited from the parent char-table
/// (or from its parents, if necessary).
#[lisp_fn]
pub fn char_table_parent(chartable: LispCharTableRef) -> Option<LispCharTableRef> {
    LispObject::from_raw(chartable.parent).as_char_table()
}

/// Set the parent char-table of CHARTABLE to PARENT.
/// Return PARENT.  PARENT must be either nil or another char-table.
#[lisp_fn]
pub fn set_char_table_parent(
    mut chartable: LispCharTableRef,
    parent: Option<LispCharTableRef>,
) -> () {
    let mut temp = parent;
    while temp.is_some() {
        if let Some(p) = temp {
            if chartable.eq(&p) {
                error!("Attempt to make a chartable to be its own parent");
            }
            temp = char_table_parent(p);
        }
    }

    chartable.parent = if let Some(p) = parent {
        p.as_lisp_obj().to_raw()
    } else {
        LispObject::constant_nil().to_raw()
    };
    //parent
}

include!(concat!(env!("OUT_DIR"), "/chartable_exports.rs"));
