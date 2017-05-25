use lisp::LispObject;
use remacs_macros::lisp_fn;


/// Return t if OBJECT is a floating point number.
/// (fn OBJECT)
#[lisp_fn(name = "floatp", min = "1")]
fn floatp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_float())
}


/// Return t if OBJECT is an integer.
/// (fn OBJECT)
#[lisp_fn(name = "integerp", min = "1")]
fn integerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_integer())
}

/// Return t if OBJECT is an integer or a marker (editor pointer).
/// (fn OBJECT)
#[lisp_fn(name = "integer-or-marker-p", min = "1")]
fn integer_or_marker_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker() || object.is_integer())
}

/// Return t if OBJECT is a non-negative integer.
/// (fn OBJECT)
#[lisp_fn(name = "natnump", min = "1")]
fn natnump(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_natnum())
}


/// Return t if OBJECT is a number (floating point or integer).
/// (fn OBJECT)
#[lisp_fn(name = "numberp", min = "1")]
fn numberp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_number())
}

/// Return t if OBJECT is a number or a marker (editor pointer).
/// (fn OBJECT)
#[lisp_fn(name = "number-or-marker-p", min = "1")]
fn number_or_marker_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_number() || object.is_marker())
}
