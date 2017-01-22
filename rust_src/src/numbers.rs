extern crate libc;

use std::os::raw::c_char;
use std::ptr;

use lisp::{LispObject, LispSubr, Qnil, Qt};
use lisp::deprecated::MARKERP;

fn Ffloatp(object: LispObject) -> LispObject {
    if object.is_float() {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("floatp",
       Ffloatp,
       Sfloatp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a floating point number.

(fn OBJECT)");

fn Fintegerp(object: LispObject) -> LispObject {
    if object.is_integer() {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("integerp",
       Fintegerp,
       Sintegerp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is an integer.

(fn OBJECT)");

fn Finteger_or_marker_p(object: LispObject) -> LispObject {
    if MARKERP(object) || object.is_integer() {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("integer-or-marker-p",
       Finteger_or_marker_p,
       Sinteger_or_marker_p,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is an integer or a marker (editor pointer).

(fn OBJECT)");

fn Fnatnump(object: LispObject) -> LispObject {
    if object.is_integer() && 0 <= object.to_fixnum().unwrap() {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("natnump",
       Fnatnump,
       Snatnump,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a non-negative integer.

(fn OBJECT)");

fn Fnumberp(object: LispObject) -> LispObject {
    if object.is_number() {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("numberp",
       Fnumberp,
       Snumberp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a number (floating point or integer).

(fn OBJECT)");

fn Fnumber_or_marker_p(object: LispObject) -> LispObject {
    if object.is_number() || MARKERP(object) {
        unsafe { Qt }
    } else {
        Qnil
    }
}

defun!("number-or-marker-p",
       Fnumber_or_marker_p,
       Snumber_or_marker_p,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a number or a marker (editor pointer).

(fn OBJECT)");
