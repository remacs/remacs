//! Functions operating on numbers.

use lisp::LispObject;
use remacs_macros::lisp_fn;


/// Return t if OBJECT is a floating point number.
#[lisp_fn]
fn floatp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_float())
}

/// Return t if OBJECT is an integer.
#[lisp_fn]
fn integerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_integer())
}

/// Return t if OBJECT is an integer or a marker (editor pointer).
#[lisp_fn]
fn integer_or_marker_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker() || object.is_integer())
}

/// Return t if OBJECT is a non-negative integer.
#[lisp_fn]
fn natnump(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_natnum())
}

/// Return t if OBJECT is a number (floating point or integer).
#[lisp_fn]
fn numberp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_number())
}

/// Return t if OBJECT is a number or a marker (editor pointer).
#[lisp_fn]
fn number_or_marker_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_number() || object.is_marker())
}
