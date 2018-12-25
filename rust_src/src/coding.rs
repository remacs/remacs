//! Coding system handler.

use remacs_macros::lisp_fn;

use crate::{
    hashtable::HashLookupResult::{Found, Missing},
    lisp::defsubr,
    lisp::LispObject,
    remacs_sys::{Fget, Qcoding_system_define_form, Vcoding_system_hash_table},
};

/// Return the ID of OBJECT.
/// Same as the CODING_SYSTEM_ID C macro.
pub fn coding_system_id(object: LispObject) -> isize {
    let h_ref = unsafe { Vcoding_system_hash_table }.as_hash_table_or_error();
    match h_ref.lookup(object) {
        Found(idx) => idx as isize,
        Missing(_) => -1,
    }
}

/// Return t if OBJECT is nil or a coding-system.
/// See the documentation of `define-coding-system' for information
/// about coding-system objects.
#[lisp_fn]
pub fn coding_system_p(object: LispObject) -> bool {
    object.is_nil()
        || coding_system_id(object) >= 0
        || (object.is_symbol() && unsafe { Fget(object, Qcoding_system_define_form) }.is_not_nil())
}

include!(concat!(env!("OUT_DIR"), "/coding_exports.rs"));
