//! Functions operating on float numbers.

use libc;
use std::mem;

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsDouble, EmacsInt, EmacsUint, Lisp_Object, MOST_NEGATIVE_FIXNUM,
                 MOST_POSITIVE_FIXNUM};
use remacs_sys::{Qarith_error, Qinteger_or_marker_p, Qnumberp, Qrange_error};
use remacs_sys::libm;

use lisp::{LispNumber, LispObject};
use lisp::defsubr;
use math::ArithOp;

/// Either extracts a floating point number from a lisp number (of any kind) or throws an error
/// TODO this is used from C in a few places; remove afterwards.
#[no_mangle]
pub extern "C" fn extract_float(f: Lisp_Object) -> EmacsDouble {
    let f = LispObject::from_raw(f);
    f.any_to_float_or_error()
}

/// Calculate the modulus of two elisp floats.
pub fn fmod_float(mut f1: f64, f2: f64) -> EmacsDouble {
    f1 %= f2;

    // Ensure that the remainder has the correct sign.
    if f2 < 0.0 && f1 > 0.0 || f2 > 0.0 && f1 < 0.0 {
        f1 += f2;
    }

    f1
}

macro_rules! simple_float_op {
    ($lisp_name:expr, $float_func:ident, $lisp_docs:expr) => {
        #[doc = $lisp_docs]
        #[lisp_fn(name = $lisp_name, c_name = $lisp_name)]
        fn $float_func(arg: EmacsDouble) -> EmacsDouble {
            arg.$float_func()
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

/// Driver for standard arithmetic operations on floats.
pub fn float_arith_driver(
    mut accum: f64,
    argstart: usize,
    code: ArithOp,
    args: &[LispObject],
) -> EmacsDouble {
    for (i, &val) in args[argstart..].iter().enumerate() {
        let argnum = argstart + i;
        let next = match val.as_number_coerce_marker_or_error() {
            LispNumber::Float(f) => f,
            LispNumber::Fixnum(d) => d as f64,
        };
        match code {
            ArithOp::Add => accum += next,
            ArithOp::Sub => {
                accum = {
                    if argnum > 0 {
                        accum - next
                    } else if args.len() == 1 {
                        -next
                    } else {
                        next
                    }
                }
            }
            ArithOp::Mult => accum *= next,
            ArithOp::Div => if args.len() > 1 && argnum == 0 {
                accum = next;
            } else {
                if next == 0. {
                    xsignal!(Qarith_error);
                }
                accum /= next;
            },
            ArithOp::Logand | ArithOp::Logior | ArithOp::Logxor => {
                wrong_type!(Qinteger_or_marker_p, val)
            }
        }
    }
    accum
}

/// Return non nil if argument X is a NaN.
#[lisp_fn]
pub fn isnan(f: EmacsDouble) -> bool {
    f.is_nan()
}

/// Return the inverse tangent of the arguments.
/// If only one argument Y is given, return the inverse tangent of Y.
/// If two arguments Y and X are given, return the inverse tangent of Y
/// divided by X, i.e. the angle in radians between the vector (X, Y)
/// and the x-axis
#[lisp_fn(min = "1")]
pub fn atan(y: EmacsDouble, x: Option<EmacsDouble>) -> EmacsDouble {
    match x {
        None => y.atan(),
        Some(x) => y.atan2(x),
    }
}

/// Return the natural logarithm of ARG.
/// If the optional argument BASE is given, return log ARG using that base.
#[lisp_fn(min = "1")]
pub fn log(arg: EmacsDouble, base: Option<EmacsDouble>) -> EmacsDouble {
    match base {
        None => arg.ln(),
        Some(base) => if base == 10.0 {
            arg.log10()
        } else if base == 2.0 {
            arg.log2()
        } else {
            arg.log(base)
        },
    }
}

/* These functions take only floats now. */

/// Return the smallest integer no less than ARG, as a float.
/// (Round toward +inf.)
#[lisp_fn]
pub fn fceiling(arg: LispObject) -> EmacsDouble {
    let d = arg.as_float_or_error();
    d.ceil()
}

/// Return the largest integer no greater than ARG, as a float.
/// (Round toward -inf.)
#[lisp_fn]
pub fn ffloor(arg: LispObject) -> EmacsDouble {
    let d = arg.as_float_or_error();
    d.floor()
}

/// Truncate a floating point number to an integral float value.
/// (Round toward zero.)
#[lisp_fn]
pub fn ftruncate(arg: LispObject) -> EmacsDouble {
    let d = arg.as_float_or_error();
    if d > 0.0 {
        d.floor()
    } else {
        d.ceil()
    }
}

/// Return the floating point number equal to ARG.
#[lisp_fn]
pub fn float(arg: LispObject) -> LispObject {
    if arg.is_float() {
        arg
    } else if let Some(n) = arg.as_fixnum() {
        LispObject::from_float(n as EmacsDouble)
    } else {
        wrong_type!(Qnumberp, arg);
    }
}

/// Copy sign of X2 to value of X1, and return the result.
/// Cause an error if X1 or X2 is not a float.
#[lisp_fn]
pub fn copysign(x1: EmacsDouble, x2: EmacsDouble) -> EmacsDouble {
    if libm::signbit(x1) != libm::signbit(x2) {
        -x1
    } else {
        x1
    }
}

/// Get significand and exponent of a floating point number.
/// Breaks the floating point number X into its binary significand SGNFCAND
/// (a floating point value between 0.5 (included) and 1.0 (excluded))
/// and an integral exponent EXP for 2, such that:
///
///   X = SGNFCAND * 2^EXP
///
/// The function returns the cons cell (SGNFCAND . EXP).
/// If X is zero, both parts (SGNFCAND and EXP) are zero.
#[lisp_fn]
pub fn frexp(x: EmacsDouble) -> LispObject {
    let (significand, exponent) = libm::frexp(x);
    LispObject::cons(
        LispObject::from_float(significand),
        LispObject::from_fixnum(EmacsInt::from(exponent)),
    )
}

/// Return SGNFCAND * 2**EXPONENT, as a floating point number.
/// EXPONENT must be an integer.
#[lisp_fn]
pub fn ldexp(significand: EmacsDouble, exponent: EmacsInt) -> EmacsDouble {
    libm::ldexp(significand, exponent as libc::c_int)
}

/// Return the exponential ARG1 ** ARG2.
#[lisp_fn]
pub fn expt(arg1: LispObject, arg2: LispObject) -> LispObject {
    if let (Some(x), Some(y)) = (arg1.as_fixnum(), arg2.as_fixnum()) {
        if y >= 0 && y <= EmacsInt::from(u32::max_value()) {
            return LispObject::from_fixnum(x.pow(y as u32));
        }
    }
    let b = arg1.any_to_float_or_error();
    let e = arg2.any_to_float_or_error();
    LispObject::from_float(b.powf(e))
}

/// Returns largest integer <= the base 2 log of the magnitude of ARG.
/// This is the same as the exponent of a float.
#[lisp_fn]
pub fn logb(arg: LispObject) -> EmacsInt {
    if let Some(n) = arg.as_fixnum() {
        let i = n.abs();
        if i == 0 {
            MOST_NEGATIVE_FIXNUM
        } else {
            (mem::size_of::<EmacsUint>() * 8) as EmacsInt - 1 - EmacsInt::from(i.leading_zeros())
        }
    } else if let Some(f) = arg.as_float() {
        if f == 0.0 {
            MOST_NEGATIVE_FIXNUM
        } else if f.is_finite() {
            let (_, exp) = libm::frexp(f);
            EmacsInt::from(exp) - 1
        } else {
            MOST_POSITIVE_FIXNUM
        }
    } else {
        wrong_type!(Qnumberp, arg)
    }
}

/// Return the nearest integer to ARG, as a float.
#[lisp_fn]
pub fn fround(arg: LispObject) -> EmacsDouble {
    let d = arg.as_float_or_error();
    libm::rint(d)
}

/// Return the smallest integer no less than ARG.
/// This rounds the value towards +inf.
/// With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR.
#[lisp_fn(min = "1")]
pub fn ceiling(arg: LispObject, divisor: LispObject) -> EmacsInt {
    rounding_driver(arg, divisor, |x| x.ceil(), ceiling2, "ceiling")
}

/// Return the largest integer no greater than ARG.
/// This rounds the value towards -inf.
/// With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.
#[lisp_fn(min = "1")]
pub fn floor(arg: LispObject, divisor: LispObject) -> EmacsInt {
    rounding_driver(arg, divisor, |x| x.floor(), floor2, "floor")
}

/// Return the nearest integer to ARG.
/// With optional DIVISOR, return the nearest integer to ARG/DIVISOR.
///
/// Rounding a value equidistant between two integers may choose the
/// integer closer to zero, or it may prefer an even integer, depending on
/// your machine.  For example, (round 2.5) can return 3 on some
/// systems, but 2 on others.
#[lisp_fn(min = "1")]
pub fn round(arg: LispObject, divisor: LispObject) -> EmacsInt {
    rounding_driver(arg, divisor, libm::rint, round2, "round")
}

/// Truncate a floating point number to an int.
/// Rounds ARG toward zero.
/// With optional DIVISOR, truncate ARG/DIVISOR.
#[lisp_fn(min = "1")]
pub fn truncate(arg: LispObject, divisor: LispObject) -> EmacsInt {
    rounding_driver(arg, divisor, |x| x.trunc(), truncate2, "truncate")
}

fn rounding_driver<F>(
    arg: LispObject,
    divisor: LispObject,
    double_round: F,
    int_round2: fn(EmacsInt, EmacsInt) -> EmacsInt,
    name: &str,
) -> EmacsInt
where
    F: Fn(f64) -> f64,
{
    let d;
    if divisor.is_nil() {
        if arg.is_fixnum() {
            return arg.as_fixnum().unwrap();
        } else if let Some(f) = arg.as_float() {
            d = f;
        } else {
            wrong_type!(Qnumberp, arg)
        }
    } else {
        if let (Some(arg), Some(div)) = (arg.as_fixnum(), divisor.as_fixnum()) {
            if div == 0 {
                xsignal!(Qarith_error);
            }
            return int_round2(arg, div);
        }
        let arg = arg.any_to_float_or_error();
        let div = divisor.any_to_float_or_error();
        d = arg / div;
    }

    // Round, coarsely test for fixnum overflow before converting to
    // EmacsInt (to avoid undefined behavior), and then exactly test
    // for overflow after converting (as FIXNUM_OVERFLOW_P is inaccurate
    // on floats).
    let dr = double_round(d);
    if dr.abs() < (2 * (MOST_POSITIVE_FIXNUM + 1)) as f64 {
        let ir = dr as EmacsInt;
        if !LispObject::fixnum_overflow(ir) {
            return ir;
        }
    }

    xsignal!(Qrange_error, LispObject::from(name), arg)
}

fn ceiling2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    i1 / i2 + ((i1 % i2 != 0) & ((i1 < 0) == (i2 < 0))) as EmacsInt
}

fn floor2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    i1 / i2 - ((i1 % i2 != 0) & ((i1 < 0) != (i2 < 0))) as EmacsInt
}

fn truncate2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    i1 / i2
}

fn round2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    // The division operator gives us one remainder R, but we want the
    // remainder R1 on the other side of 0 if R1 is closer to 0 than R
    // is; because we want to round to even, we also want R1 if R and R1
    // are the same distance from 0 and if C's quotient is odd.
    let q = i1 / i2;
    let r = i1 % i2;
    let abs_r = r.abs();
    let abs_r1 = i2.abs() - abs_r;
    q + if abs_r + (q & 1) <= abs_r1 {
        0
    } else if (i2 ^ r) < 0 {
        -1
    } else {
        1
    }
}

// Since these are generated via a macro the build cannot hook them into the
// system automatically. Do not add more items here unless they are also generated
// with something like simple_float_op.
pub fn rust_init_extra_syms() {
    unsafe {
        defsubr(Sacos.as_ptr());
        defsubr(Sasin.as_ptr());
        defsubr(Scos.as_ptr());
        defsubr(Ssin.as_ptr());
        defsubr(Stan.as_ptr());
        defsubr(Sexp.as_ptr());
        defsubr(Ssqrt.as_ptr());
    }
}

include!(concat!(env!("OUT_DIR"), "/floatfns_exports.rs"));
