//! Basic character set support.

use remacs_macros::lisp_fn;

use crate::{
    hashtable::{HashLookupResult, LispHashTableRef},
    libc::c_int,
    lisp::LispObject,
    remacs_sys::temp_charset_work,
    remacs_sys::Foptimize_char_table,
    remacs_sys::Qnil,
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

unsafe fn set_temp_charset_work_encoder(c: c_int, code: u16) {
    if code == 0 {
        (*temp_charset_work).zero_index_char = c;
    } else if c < 0x20000 {
        (*temp_charset_work).table.encoder[c as usize] = code;
    } else {
        (*temp_charset_work).table.encoder[c as usize - 0x10000] = code
    }
}

unsafe fn get_temp_charset_work_encoder(c: c_int) -> c_int {
    if c == (*temp_charset_work).zero_index_char {
        0
    } else if c < 0x20000 {
        if (*temp_charset_work).table.encoder[c as usize] != 0 {
            i32::from((*temp_charset_work).table.encoder[c as usize])
        } else {
            -1
        }
    } else if (*temp_charset_work).table.encoder[c as usize - 0x10000] != 0 {
        i32::from((*temp_charset_work).table.encoder[c as usize - 0x10000])
    } else {
        -1
    }
}

/// Internal use only.
/// Clear temporary charset mapping tables.
/// It should be called only from temacs invoked for dumping.
#[lisp_fn]
pub fn clear_charset_maps() {
    unsafe {
        *temp_charset_work = Default::default();

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
