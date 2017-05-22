use lisp::{LispObject, Qnil, INTEGERP, FLOATP, MARKERP, NATNUMP, NUMBERP};
use remacs_sys::Qt;
use remacs_macros::lisp_fn;


/// Return t if OBJECT is a floating point number.
/// (fn OBJECT)
#[lisp_fn(name = "floatp", min = "1")]
fn floatp(object: LispObject) -> LispObject {
    if FLOATP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}


/// Return t if OBJECT is an integer.
/// (fn OBJECT)
#[lisp_fn(name = "integerp", min = "1")]
fn integerp(object: LispObject) -> LispObject {
    if INTEGERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

/// Return t if OBJECT is an integer or a marker (editor pointer).
/// (fn OBJECT)
#[lisp_fn(name = "integer-or-marker-p", min = "1")]
fn integer_or_marker_p(object: LispObject) -> LispObject {
    if MARKERP(object) || INTEGERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

/// Return t if OBJECT is a non-negative integer.
/// (fn OBJECT)
#[lisp_fn(name = "natnump", min = "1")]
fn natnump(object: LispObject) -> LispObject {
    if NATNUMP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}


/// Return t if OBJECT is a number (floating point or integer).
/// (fn OBJECT)
#[lisp_fn(name = "numberp", min = "1")]
fn numberp(object: LispObject) -> LispObject {
    if NUMBERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

/// Return t if OBJECT is a number or a marker (editor pointer).
/// (fn OBJECT)
#[lisp_fn(name = "number-or-marker-p", min = "1")]
fn number_or_marker_p(object: LispObject) -> LispObject {
    if NUMBERP(object) || MARKERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}
