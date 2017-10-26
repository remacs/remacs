//! Call a Lisp function interactively.

use remacs_macros::lisp_fn;
use remacs_sys::Qminus;
use lisp::LispObject;

/// Return numeric meaning of raw prefix argument RAW.
/// A raw prefix argument is what you get from `(interactive "P")'.
/// Its numeric meaning is what you would get from `(interactive "p")'.
#[lisp_fn]
fn prefix_numeric_value(raw: LispObject) -> LispObject {
    if raw.is_nil() {
        LispObject::from_fixnum(1)
    } else if raw.eq(LispObject::from_raw(Qminus)) {
        LispObject::from_fixnum(-1)
    } else if raw.is_integer() {
        raw
    } else if let Some(number) = raw.as_cons().map_or(None, |v| v.car().as_fixnum()) {
        LispObject::from_fixnum(number)
    } else {
        LispObject::from_fixnum(1)
    }
}
