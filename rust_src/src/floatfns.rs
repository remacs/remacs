//! Functions operating on float numbers.
#![allow(clippy::float_cmp)]

use std::mem;

use libc;

use remacs_macros::lisp_fn;

use crate::{
    hashtable::LispHashTableRef,
    libm,
    lisp::defsubr,
    lisp::{ExternalPtr, LispObject, LispStructuralEqual},
    math::ArithOp,
    numbers::{LispNumber, LispNumberOrFloatOrMarker, MOST_NEGATIVE_FIXNUM, MOST_POSITIVE_FIXNUM},
    remacs_sys::{equal_kind, EmacsDouble, EmacsInt, EmacsUint, Lisp_Float, Lisp_Type},
    remacs_sys::{Qfloatp, Qinteger_or_marker_p, Qrange_error},
};

// Float support (LispType == Lisp_Float == 7 )

pub type LispFloatRef = ExternalPtr<Lisp_Float>;

impl LispFloatRef {
    pub fn as_data(&self) -> &EmacsDouble {
        unsafe { &*(&self.u.data as *const EmacsDouble) }
    }

    fn to_float(self) -> EmacsDouble {
        *self.as_data()
    }
}

impl LispStructuralEqual for LispFloatRef {
    fn equal(
        &self,
        other: Self,
        _kind: equal_kind::Type,
        _depth: i32,
        _ht: &mut LispHashTableRef,
    ) -> bool {
        let d1 = self.to_float();
        let d2 = other.to_float();

        // Two NaNs should be `equal' even though they are not =.
        d1 == d2 || (d1.is_nan() && d2.is_nan())
    }
}

impl LispObject {
    pub fn is_float(self) -> bool {
        self.get_type() == Lisp_Type::Lisp_Float
    }

    unsafe fn to_float_unchecked(self) -> LispFloatRef {
        debug_assert!(self.is_float());
        LispFloatRef::new(self.get_untaggedptr() as *mut Lisp_Float)
    }

    unsafe fn get_float_data_unchecked(self) -> EmacsDouble {
        *self.to_float_unchecked().as_data()
    }

    pub fn force_float(self) -> EmacsDouble {
        unsafe { self.get_float_data_unchecked() }
    }

    pub fn as_floatref(self) -> Option<LispFloatRef> {
        if self.is_float() {
            Some(unsafe { self.to_float_unchecked() })
        } else {
            None
        }
    }

    pub fn force_floatref(self) -> LispFloatRef {
        unsafe { self.to_float_unchecked() }
    }

    pub fn as_float(self) -> Option<EmacsDouble> {
        if self.is_float() {
            Some(unsafe { self.get_float_data_unchecked() })
        } else {
            None
        }
    }
}

impl From<LispObject> for EmacsDouble {
    fn from(o: LispObject) -> Self {
        o.as_float().unwrap_or_else(|| wrong_type!(Qfloatp, o))
    }
}

impl From<LispObject> for Option<EmacsDouble> {
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.into())
        }
    }
}

/// Either extracts a floating point number from a lisp number (of any kind) or throws an error
/// TODO this is used from C in a few places; remove afterwards.
#[no_mangle]
pub extern "C" fn extract_float(f: LispObject) -> EmacsDouble {
    let value: LispNumber = f.into();
    value.to_float()
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
        fn $float_func(arg: LispNumber) -> EmacsDouble {
            arg.to_float().$float_func()
        }
    };
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
        let v = LispNumberOrFloatOrMarker::from(val);
        let next = v.to_float();
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
            ArithOp::Div => {
                if args.len() > 1 && argnum == 0 {
                    accum = next;
                } else {
                    accum /= next;
                }
            }
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
pub fn atan(y: LispNumber, x: Option<LispNumber>) -> EmacsDouble {
    match x {
        None => y.to_float().atan(),
        Some(x) => y.to_float().atan2(x.to_float()),
    }
}

/// Return the natural logarithm of ARG.
/// If the optional argument BASE is given, return log ARG using that base.
#[lisp_fn(min = "1")]
pub fn log(arg: LispNumber, base: Option<LispNumber>) -> EmacsDouble {
    let d = arg.to_float();
    match base {
        None => d.ln(),
        Some(base) => {
            let b = base.to_float();
            if b == 10.0 {
                d.log10()
            } else if b == 2.0 {
                d.log2()
            } else {
                d.log(b)
            }
        }
    }
}

/* These functions take only floats now. */

/// Return the smallest integer no less than ARG, as a float.
/// (Round toward +inf.)
#[lisp_fn]
pub fn fceiling(arg: EmacsDouble) -> EmacsDouble {
    arg.ceil()
}

/// Return the largest integer no greater than ARG, as a float.
/// (Round toward -inf.)
#[lisp_fn]
pub fn ffloor(arg: EmacsDouble) -> EmacsDouble {
    arg.floor()
}

/// Truncate a floating point number to an integral float value.
/// (Round toward zero.)
#[lisp_fn]
pub fn ftruncate(arg: EmacsDouble) -> EmacsDouble {
    if arg > 0.0 {
        arg.floor()
    } else {
        arg.ceil()
    }
}

/// Return the floating point number equal to ARG.
#[lisp_fn]
pub fn float(arg: LispNumber) -> EmacsDouble {
    arg.to_float()
}

/// Copy sign of X2 to value of X1, and return the result.
/// Cause an error if X1 or X2 is not a float.
#[lisp_fn]
pub fn copysign(x1: EmacsDouble, x2: EmacsDouble) -> EmacsDouble {
    if libm::signbit(x1) == libm::signbit(x2) {
        x1
    } else {
        -x1
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
pub fn frexp(x: LispNumber) -> (EmacsDouble, libc::c_int) {
    libm::frexp(x.to_float())
    (significand, exponent)
}

/// Return SGNFCAND * 2**EXPONENT, as a floating point number.
/// EXPONENT must be an integer.
#[lisp_fn]
pub fn ldexp(significand: LispNumber, exponent: EmacsInt) -> EmacsDouble {
    libm::ldexp(significand.to_float(), exponent as libc::c_int)
}

/// Return the exponential ARG1 ** ARG2.
#[lisp_fn]
pub fn expt(arg1: LispNumber, arg2: LispNumber) -> LispNumber {
    match (arg1, arg2) {
        (LispNumber::Fixnum(x), LispNumber::Fixnum(y)) if y >= 0 => {
            LispNumber::Fixnum(x.pow(y as u32))
        }
        (x, y) => {
            let b = x.to_float();
            let e = y.to_float();
            LispNumber::Float(b.powf(e))
        }
    }
}

/// Returns largest integer <= the base 2 log of the magnitude of ARG.
/// This is the same as the exponent of a float.
#[lisp_fn]
pub fn logb(arg: LispNumber) -> EmacsInt {
    match arg {
        LispNumber::Float(f) if f == 0.0 => MOST_NEGATIVE_FIXNUM,
        LispNumber::Float(f) if f.is_finite() => {
            let (_, exp) = libm::frexp(f);
            EmacsInt::from(exp) - 1
        }
        LispNumber::Float(_) => MOST_POSITIVE_FIXNUM,
        LispNumber::Fixnum(n) if n == 0 => MOST_NEGATIVE_FIXNUM,
        LispNumber::Fixnum(n) => {
            let i = n.abs();
            (mem::size_of::<EmacsUint>() * 8) as EmacsInt - 1 - EmacsInt::from(i.leading_zeros())
        }
    }
}

/// Return the nearest integer to ARG, as a float.
#[lisp_fn]
pub fn fround(arg: EmacsDouble) -> EmacsDouble {
    libm::rint(arg)
}

/// Return the smallest integer no less than ARG.
/// This rounds the value towards +inf.
/// With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR.
#[lisp_fn(min = "1")]
pub fn ceiling(arg: LispNumber, divisor: Option<LispNumber>) -> EmacsInt {
    rounding_driver(arg, divisor, |x| x.ceil(), ceiling2, "ceiling")
}

/// Return the largest integer no greater than ARG.
/// This rounds the value towards -inf.
/// With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.
#[lisp_fn(min = "1")]
pub fn floor(arg: LispNumber, divisor: Option<LispNumber>) -> EmacsInt {
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
pub fn round(arg: LispNumber, divisor: Option<LispNumber>) -> EmacsInt {
    rounding_driver(arg, divisor, |x| x.round(), round2, "round")
}

/// Truncate a floating point number to an int.
/// Rounds ARG toward zero.
/// With optional DIVISOR, truncate ARG/DIVISOR.
#[lisp_fn(min = "1")]
pub fn truncate(arg: LispNumber, divisor: Option<LispNumber>) -> EmacsInt {
    rounding_driver(arg, divisor, |x| x.trunc(), truncate2, "truncate")
}

fn rounding_driver(
    arg: LispNumber,
    divisor: Option<LispNumber>,
    round: impl Fn(EmacsDouble) -> EmacsDouble,
    int_round: impl Fn(EmacsInt, EmacsInt) -> EmacsInt,
    name: &str,
) -> EmacsInt {
    let value = match divisor {
        None => match arg {
            LispNumber::Fixnum(v) => {
                return v;
            }
            LispNumber::Float(f) => f,
        },
        Some(div) => match (arg, div) {
            (LispNumber::Fixnum(v), LispNumber::Fixnum(d)) => {
                return int_round(v, d);
            }
            _ => {
                let div: EmacsDouble = div.into();
                if div == 0.0 {
                    arith_error!();
                }
                EmacsDouble::from(arg) / div
            }
        },
    };
    let result = round(value) as EmacsInt;
    if LispObject::fixnum_overflow(result) {
        xsignal!(Qrange_error, name, arg);
    }
    result
}

const fn ceiling2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    i1 / i2 + ((i1 % i2 != 0) & ((i1 < 0) == (i2 < 0))) as EmacsInt
}

const fn floor2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    i1 / i2 - ((i1 % i2 != 0) & ((i1 < 0) != (i2 < 0))) as EmacsInt
}

const fn truncate2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    i1 / i2
}

fn round2(i1: EmacsInt, i2: EmacsInt) -> EmacsInt {
    // We also want the remainder R1 on the other side of 0 if R1 is
    // closer to 0 than R is; because we want to round to even, we also
    // want R1 if R and R1 are the same distance from 0 and if the
    // quotient is odd.
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

#[cfg(test)]
use std::cmp::max;

#[test]
fn test_basic_float() {
    let val = 8.0;
    let result = mock_float!(val);
    assert!(result.is_float() && result.as_float() == Some(val));
}

#[test]
fn test_lisp_float_size() {
    let double_size = mem::size_of::<EmacsDouble>();
    let ptr_size = mem::size_of::<*const Lisp_Float>();

    assert!(mem::size_of::<Lisp_Float>() == max(double_size, ptr_size));
}
