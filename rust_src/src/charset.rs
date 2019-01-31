//! Basic character set support.

use remacs_macros::lisp_fn;

use crate::{
    hashtable::{HashLookupResult, LispHashTableRef},
    lisp::LispObject,
    remacs_sys::Vcharset_hash_table,
};

impl LispObject {
    pub fn is_charset(self) -> bool {
        let h_ref: LispHashTableRef = unsafe { Vcharset_hash_table }.into();
        match h_ref.lookup(self) {
            HashLookupResult::Found(_) => true,
            HashLookupResult::Missing(_) => false,
        }
    }
}

/// Return non-nil if and only if OBJECT is a charset.
#[lisp_fn]
pub fn charsetp(object: LispObject) -> bool {
    object.is_charset()
}

include!(concat!(env!("OUT_DIR"), "/charset_exports.rs"));
