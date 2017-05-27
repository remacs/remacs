use std::ptr;

use lisp::LispObject;


fn floatp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_float())
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
    LispObject::from_bool(object.is_integer())
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
    LispObject::from_bool(object.is_marker() || object.is_integer())
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
    LispObject::from_bool(object.is_natnum())
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
    LispObject::from_bool(object.is_number())
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
    LispObject::from_bool(object.is_number() || object.is_marker())
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
