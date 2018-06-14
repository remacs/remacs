use remacs_macros::lisp_fn;

use lisp::{defsubr, LispObject};
use objects::eq;
use remacs_sys::Qcase_table;

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

#[no_mangle]
pub extern "C" fn rust_syms_of_casetab() {
    def_lisp_sym!(Qcase_table_p, "case-table-p");
}

include!(concat!(env!("OUT_DIR"), "/casetab_exports.rs"));
