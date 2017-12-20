//! Call a Lisp function interactively.

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Qminus};

use lisp::LispObject;
use lisp::defsubr;

/// Return numeric meaning of raw prefix argument RAW.
/// A raw prefix argument is what you get from `(interactive "P")'.
/// Its numeric meaning is what you would get from `(interactive "p")'.
#[lisp_fn]
pub fn prefix_numeric_value(raw: LispObject) -> EmacsInt {
    if raw.is_nil() {
        1
    } else if raw.eq(LispObject::from_raw(Qminus)) {
        -1
    } else if raw.is_integer() {
        raw.as_fixnum_or_error()
    } else if let Some(number) = raw.as_cons().map_or(None, |v| v.car().as_fixnum()) {
        number
    } else {
        1
    }
}

include!(concat!(env!("OUT_DIR"), "/interactive_exports.rs"));
