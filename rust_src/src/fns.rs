//! Random utility Lisp functions.

use remacs_macros::lisp_fn;
use remacs_sys::{Faref, Qsequencep};
use lisp::LispObject;
use lists::{car, nthcdr};

/// Return element of SEQUENCE at index N.
#[lisp_fn]
pub fn elt(sequence: LispObject, n: LispObject) -> LispObject {
    n.as_natnum_or_error();
    if sequence.is_cons() || sequence.is_nil() {
        car(nthcdr(n, sequence))
    } else if sequence.is_array() {
        LispObject::from_raw(unsafe { Faref(sequence.to_raw(), n.to_raw()) })
    } else {
        wrong_type!(Qsequencep, sequence);
    }
}
