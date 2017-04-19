use std::ptr;

use lisp::{LispObject, Qnil, INTEGERP, FLOATP, MARKERP, NATNUMP, NUMBERP};
use remacs_sys::Qt;

fn floatp(object: LispObject) -> LispObject {
    if FLOATP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

defun!("floatp",
       Ffloatp(object),
       Sfloatp,
       floatp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a floating point number.

(fn OBJECT)");

fn integerp(object: LispObject) -> LispObject {
    if INTEGERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

defun!("integerp",
       Fintegerp(object),
       Sintegerp,
       integerp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is an integer.

(fn OBJECT)");

fn integer_or_marker_p(object: LispObject) -> LispObject {
    if MARKERP(object) || INTEGERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

defun!("integer-or-marker-p",
       Finteger_or_marker_p(object),
       Sinteger_or_marker_p,
       integer_or_marker_p,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is an integer or a marker (editor pointer).

(fn OBJECT)");

fn natnump(object: LispObject) -> LispObject {
    if NATNUMP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

defun!("natnump",
       Fnatnump(object),
       Snatnump,
       natnump,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a non-negative integer.

(fn OBJECT)");

fn numberp(object: LispObject) -> LispObject {
    if NUMBERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

defun!("numberp",
       Fnumberp(object),
       Snumberp,
       numberp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a number (floating point or integer).

(fn OBJECT)");

fn number_or_marker_p(object: LispObject) -> LispObject {
    if NUMBERP(object) || MARKERP(object) {
        LispObject::from_raw(unsafe { Qt })
    } else {
        Qnil
    }
}

defun!("number-or-marker-p",
       Fnumber_or_marker_p(object),
       Snumber_or_marker_p,
       number_or_marker_p,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a number or a marker (editor pointer).

(fn OBJECT)");
