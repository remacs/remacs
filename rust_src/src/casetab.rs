//! Routines to deal with case tables.

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    objects::eq,
    remacs_sys::{set_case_table, Qcase_table, Vascii_downcase_table},
    threads::ThreadState,
};

/// Return t if OBJECT is a case table.
/// See `set-case-table' for more information on these data structures.
#[lisp_fn]
pub fn case_table_p(object: LispObject) -> bool {
    let char_table = match object.as_char_table() {
        Some(ct) => ct,
        None => {
            return false;
        }
    };

    if !eq(char_table.purpose, Qcase_table) {
        return false;
    }

    let extras = unsafe { char_table.extras.as_slice(3) };
    let up = extras[0];
    let canon = extras[1];
    let eqv = extras[2];

    (up.is_nil() || up.is_char_table())
        && ((canon.is_nil() && eqv.is_nil())
            || (canon.is_char_table() && (eqv.is_nil() || eqv.is_char_table())))
}

/// Return the case table of the current buffer.
#[lisp_fn]
pub fn current_case_table() -> LispObject {
    ThreadState::current_buffer_unchecked().downcase_table_
}

/// Return the standard case table.
/// This is the one used for new buffers.
#[lisp_fn]
pub fn standard_case_table() -> LispObject {
    unsafe { Vascii_downcase_table }
}

/// Select a new case table for the current buffer.
/// A case table is a char-table which maps characters
/// to their lower-case equivalents.  It also has three \"extra\" slots
/// which may be additional char-tables or nil.
/// These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
/// UPCASE maps each non-upper-case character to its upper-case equivalent.
///  (The value in UPCASE for an upper-case character is never used.)
///  If lower and upper case characters are in 1-1 correspondence,
///  you may use nil and the upcase table will be deduced from DOWNCASE.
/// CANONICALIZE maps each character to a canonical equivalent;
///  any two characters that are related by case-conversion have the same
///  canonical equivalent character; it may be nil, in which case it is
///  deduced from DOWNCASE and UPCASE.
/// EQUIVALENCES is a map that cyclically permutes each equivalence class
///  (of characters with the same canonical equivalent); it may be nil,
///  in which case it is deduced from CANONICALIZE.
#[lisp_fn(name = "set-case-table", c_name = "set_case_table")]
pub fn set_case_table_lisp(table: LispObject) -> LispObject {
    unsafe { set_case_table(table, false) }
}

/// Select a new standard case table for new buffers.
/// See `set-case-table' for more info on case tables.
#[lisp_fn]
pub fn set_standard_case_table(table: LispObject) -> LispObject {
    unsafe { set_case_table(table, true) }
}

#[no_mangle]
pub extern "C" fn rust_syms_of_casetab() {
    def_lisp_sym!(Qcase_table_p, "case-table-p");
}

include!(concat!(env!("OUT_DIR"), "/casetab_exports.rs"));
