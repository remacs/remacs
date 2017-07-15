//! Functions operating on float numbers.

use lisp::LispObject;
use remacs_sys::{EmacsDouble, Lisp_Object, Qnumberp, wrong_type_argument};
use remacs_macros::lisp_fn;

pub fn init_float_syms() {
    unsafe {
        ::defsubr(&*Sisnan);
        ::defsubr(&*Sacos);
        ::defsubr(&*Sasin);
        ::defsubr(&*Satan);
        ::defsubr(&*Scos);
        ::defsubr(&*Ssin);
        ::defsubr(&*Stan);
        ::defsubr(&*Slog);

        ::defsubr(&*Ssqrt);
        ::defsubr(&*Sexp);
        ::defsubr(&*Sffloor);
        ::defsubr(&*Sfceiling);
        ::defsubr(&*Sftruncate);
        ::defsubr(&*Sfloat);
    }
}

/// Either extracts a floating point number from a lisp number (of any kind) or throws an error
/// TODO eventually, this can hopefully go away when we have a better approach for error handling
#[no_mangle]
pub extern "C" fn extract_float(f: Lisp_Object) -> EmacsDouble {
    let f = LispObject::from_raw(f);
    f.any_to_float_or_error()
}

/// Calculate the modulus of two elisp floats.
#[no_mangle]
pub extern "C" fn fmod_float(x: Lisp_Object, y: Lisp_Object) -> Lisp_Object {
    let mut f1 = extract_float(x);
    let f2 = extract_float(y);

    f1 %= f2;

    // Ensure that the remainder has the correct sign.
    if f2 < 0.0 && f1 > 0.0 || f2 > 0.0 && f1 < 0.0 {
        f1 += f2
    }

    LispObject::from_float(f1).to_raw()
}

macro_rules! simple_float_op {
    ($lisp_name:expr, $float_func:ident, $lisp_docs:expr) => {
        #[doc = $lisp_docs]
        #[lisp_fn(name = $lisp_name, c_name = $lisp_name)]
        fn $float_func(arg: LispObject) -> LispObject {
            let d = arg.any_to_float_or_error();
            let val = d.$float_func();
            LispObject::from_float(val)
        }
    }
}

simple_float_op!("acos", acos, "Return the inverse cosine of ARG.");
simple_float_op!("asin", asin, "Return the inverse sine of ARG.");
// atan is special, defined later
simple_float_op!("cos", cos, "Return the cosine of ARG.");
simple_float_op!("sin", sin, "Return the sine of ARG.");
simple_float_op!("tan", tan, "Return the tangent of ARG.");

simple_float_op!("exp", exp, "Return the exponential base e of ARG.");
simple_float_op!("sqrt", sqrt, "Return the square root of ARG.");

/// Return non nil if argument X is a NaN.
#[lisp_fn]
fn isnan(x: LispObject) -> LispObject {
    let f = x.as_float_or_error();
    LispObject::from_bool(f.is_nan())
}

/// Return the inverse tangent of the arguments.
/// If only one argument Y is given, return the inverse tangent of Y.
/// If two arguments Y and X are given, return the inverse tangent of Y
/// divided by X, i.e. the angle in radians between the vector (X, Y)
/// and the x-axis
#[lisp_fn(min = "1")]
fn atan(y: LispObject, x: LispObject) -> LispObject {
    let y = y.any_to_float_or_error();

    if x.is_nil() {
        LispObject::from_float(y.atan())
    } else {
        let x = x.any_to_float_or_error();
        LispObject::from_float(y.atan2(x))
    }
}

/// Return the natural logarithm of ARG.
/// If the optional argument BASE is given, return log ARG using that base.
#[lisp_fn(min = "1")]
fn log(arg: LispObject, base: LispObject) -> LispObject {
    let d = arg.any_to_float_or_error();
    let res = if base.is_nil() {
        d.ln()
    } else {
        let base = base.any_to_float_or_error();
        if base == 10.0 {
            d.log10()
        } else if base == 2.0 {
            d.log2()
        } else {
            d.log(base)
        }
    };
    LispObject::from_float(res)
}

/* These functions take only floats now. */

/// Return the smallest integer no less than ARG, as a float.
/// (Round toward +inf.)
#[lisp_fn]
fn fceiling(arg: LispObject) -> LispObject {
    let d = arg.as_float_or_error();
    LispObject::from_float(d.ceil())
}

/// Return the largest integer no greater than ARG, as a float.
/// (Round toward -inf.)
#[lisp_fn]
fn ffloor(arg: LispObject) -> LispObject {
    let d = arg.as_float_or_error();
    LispObject::from_float(d.floor())
}

/// Truncate a floating point number to an integral float value.
/// (Round toward zero.)
#[lisp_fn]
fn ftruncate(arg: LispObject) -> LispObject {
    let d = arg.as_float_or_error();
    if d > 0.0 {
        LispObject::from_float(d.floor())
    } else {
        LispObject::from_float(d.ceil())
    }
}

/// Return the floating point number equal to ARG.
#[lisp_fn]
fn float(arg: LispObject) -> LispObject {
    if !arg.is_number() {
        unsafe {
            wrong_type_argument(Qnumberp, arg.to_raw());
        }
    }

    if arg.is_float() {
        arg
    } else {
        LispObject::from_float(arg.as_fixnum().unwrap() as EmacsDouble)
    }
}
