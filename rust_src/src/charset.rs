//! Basic character set support.

use std::ptr;

use remacs_macros::lisp_fn;

use remacs_sys::hash_lookup;
use remacs_sys::Vcharset_hash_table;

use lisp::defsubr;
use lisp::LispObject;

impl LispObject {
    pub fn is_charset(self) -> bool {
        unsafe {
            let h_ref = Vcharset_hash_table.as_hash_table_or_error();
            h_ref.lookup(self, ptr::null_mut()) >= 0
        }
    }
}

/// Return non-nil if and only if OBJECT is a charset.
#[lisp_fn]
pub fn charsetp(object: LispObject) -> bool {
    object.is_charset()
}

include!(concat!(env!("OUT_DIR"), "/charset_exports.rs"));
