//! Basic character set support.

use remacs_macros::lisp_fn;
use std::ptr;

use crate::{
    hashtable::{HashLookupResult, LispHashTableRef},
    lisp::LispObject,
    remacs_sys::temp_charset_work,
    remacs_sys::Qnil,
    remacs_sys::{xfree, Foptimize_char_table},
    remacs_sys::{Vchar_unify_table, Vcharset_hash_table},
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

/// Internal use only.
/// Clear temporary charset mapping tables.
/// It should be called only from temacs invoked for dumping.
#[lisp_fn]
pub fn clear_charset_maps() {
    unsafe {
        xfree(temp_charset_work as *mut libc::c_void);
        temp_charset_work = ptr::null_mut();

        if Vchar_unify_table.is_char_table() {
            Foptimize_char_table(Vchar_unify_table, Qnil);
        }
    }
}

/// Return non-nil if and only if OBJECT is a charset.
#[lisp_fn]
pub fn charsetp(object: LispObject) -> bool {
    object.is_charset()
}

include!(concat!(env!("OUT_DIR"), "/charset_exports.rs"));
