//! Functions doing math on numbers.

use floatfns;
use lisp::{LispObject, LispNumber};
use remacs_sys::{EmacsInt, Qarith_error, Qnumberp};
use remacs_macros::lisp_fn;

/// Return X modulo Y.
/// The result falls between zero (inclusive) and Y (exclusive).
/// Both X and Y must be numbers or markers.
/// (fn X Y)
#[lisp_fn(name = "mod", c_name = "mod")]
fn lisp_mod(x: LispObject, y: LispObject) -> LispObject {
    match (
        x.as_number_coerce_marker_or_error(),
        y.as_number_coerce_marker_or_error(),
    ) {
        (LispNumber::Fixnum(mut i1), LispNumber::Fixnum(i2)) => {
            if i2 == 0 {
                xsignal!(Qarith_error);
            }

            i1 %= i2;

            // Ensure that the remainder has the correct sign.
            if i2 < 0 && i1 > 0 || i2 > 0 && i1 < 0 {
                i1 += i2;
            }

            LispObject::from_fixnum(i1)
        }
        (LispNumber::Fixnum(i1), LispNumber::Float(f2)) => floatfns::fmod_float(i1 as f64, f2),
        (LispNumber::Float(f1), LispNumber::Fixnum(i2)) => floatfns::fmod_float(f1, i2 as f64),
        (LispNumber::Float(f1), LispNumber::Float(f2)) => floatfns::fmod_float(f1, f2),
    }
}

#[repr(C)]
pub enum ArithOp {
    Add,
    Sub,
    Mult,
    Div,
    // Logical AND.
    Logand,
    // Logical inclusive OR.
    Logior,
    // Logical exclusive OR.
    Logxor,
}

/// Given an array of LispObject, reduce over them according to the
/// arithmetic operation specified.
///
/// Modifies the array in place.
fn arith_driver(code: ArithOp, args: &[LispObject]) -> LispObject {
    let mut accum: EmacsInt = match code {
        ArithOp::Add | ArithOp::Sub | ArithOp::Logior | ArithOp::Logxor => 0,
        ArithOp::Logand => -1,
        _ => 1,
    };

    // TODO: use better variable names rather than just copying the C.
    let mut overflow = false;
    let mut ok_accum = accum;
    let mut ok_args = 0;

    for (argnum, &val) in args.iter().enumerate() {
        if !overflow {
            ok_args = argnum;
            ok_accum = accum;
        }

        match val.as_number_coerce_marker_or_error() {
            LispNumber::Float(_) => {
                return floatfns::float_arith_driver(ok_accum as f64, ok_args, code, args);
            }
            LispNumber::Fixnum(next) => {
                match code {
                    ArithOp::Add => {
                        if accum.checked_add(next).is_none() {
                            overflow = true;
                        }
                        accum = accum.wrapping_add(next);
                    }
                    ArithOp::Sub => {
                        if argnum == 0 {
                            if args.len() == 1 {
                                // Calling - with one argument negates it.
                                accum = -next;
                            } else {
                                accum = next;
                            }
                        } else {
                            if accum.checked_sub(next).is_none() {
                                overflow = true;
                            }
                            accum = accum.wrapping_sub(next);
                        }
                    }
                    ArithOp::Mult => {
                        if accum.checked_mul(next).is_none() {
                            overflow = true;
                        }
                        accum = accum.wrapping_mul(next);
                    }
                    ArithOp::Div => {
                        // If we have multiple arguments, we divide the first
                        // argument by all the others.
                        if args.len() > 1 && argnum == 0 {
                            accum = next;
                        } else {
                            if next == 0 {
                                xsignal!(Qarith_error);
                            }
                            if accum.checked_div(next).is_none() {
                                overflow = true;
                            } else {
                                accum = accum.wrapping_div(next);
                            }
                        }
                    }
                    ArithOp::Logand => {
                        accum &= next;
                    }
                    ArithOp::Logior => {
                        accum |= next;
                    }
                    ArithOp::Logxor => {
                        accum ^= next;
                    }
                }
            }
        }
    }

    LispObject::from_fixnum_truncated(accum)
}

/// Return sum of any number of arguments, which are numbers or markers.
/// usage: (fn &rest NUMBERS-OR-MARKERS)
#[lisp_fn(name = "+")]
fn plus(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Add, args)
}

/// Negate number or subtract numbers or markers and return the result.
/// With one arg, negates it.  With more than one arg,
/// subtracts all but the first from the first.
/// usage: (fn &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)
#[lisp_fn(name = "-")]
fn minus(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Sub, args)
}

/// Return product of any number of arguments, which are numbers or markers.
/// usage: (fn &rest NUMBER-OR-MARKERS)
#[lisp_fn(name = "*")]
fn times(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Mult, args)
}

/// Divide number by divisors and return the result.
/// With two or more arguments, return first argument divided by the rest.
/// With one argument, return 1 divided by te argument.
/// The arguments must be numbers or markers.
/// usage: (fn NUMBER &rest DIVISORS)
#[lisp_fn(name = "/", min = "1")]
fn quo(args: &mut [LispObject]) -> LispObject {
    for argnum in 2..args.len() {
        let arg = args[argnum];
        if arg.is_float() {
            return floatfns::float_arith_driver(0.0, 0, ArithOp::Div, args);
        }
    }
    arith_driver(ArithOp::Div, args)
}

/// Return bitwise-and of all the arguments.
/// Arguments may be integers, or markers, converted to integers.
/// usage: (fn &rest INTS-OR-MARKERS)
#[lisp_fn]
fn logand(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logand, args)
}

/// Return bitwise-or of all the arguments.
/// Arguments may be integers, or markers converted to integers.
/// usage: (fn &rest INTS-OR-MARKERS)
#[lisp_fn]
fn logior(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logior, args)
}

/// Return bitwise-exclusive-or of all the arguments.
/// Arguments may be integers, or markers converted to integers.
/// usage: (fn &rest INTS-OR-MARKERS)
#[lisp_fn]
fn logxor(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logxor, args)
}

fn minmax_driver(args: &[LispObject], comparison: ArithComparison) -> LispObject {
    assert!(args.len() > 0);
    let mut accum = args[0];
    for &arg in &args[1..] {
        if arithcompare(arg, accum, comparison).is_not_nil() {
            accum = arg;
        }
        if accum.as_float().map_or(false, |f| f.is_nan()) {
            return accum;
        }
    }
    // we should return the same object if it's not a marker
    if let Some(m) = accum.as_marker() {
        LispObject::from_fixnum(m.charpos_or_error() as EmacsInt)
    } else {
        accum
    }
}

/// Return largest of all the arguments (which must be numbers or markers).
/// The value is always a number; markers are converted to numbers.
/// usage: (fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(min = "1")]
fn max(args: &mut [LispObject]) -> LispObject {
    minmax_driver(args, ArithComparison::Grtr)
}

/// Return smallest of all the arguments (which must be numbers or markers).
/// The value is always a number; markers are converted to numbers.
/// usage: (fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(min = "1")]
fn min(args: &mut [LispObject]) -> LispObject {
    minmax_driver(args, ArithComparison::Less)
}

/// Return the absolute value of ARG.
#[lisp_fn]
fn abs(arg: LispObject) -> LispObject {
    if let Some(f) = arg.as_float() {
        LispObject::from_float(f.abs())
    } else if let Some(n) = arg.as_fixnum() {
        LispObject::from_fixnum(n.abs())
    } else {
        wrong_type!(Qnumberp, arg);
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum ArithComparison {
    Equal,
    Notequal,
    Less,
    Grtr,
    LessOrEqual,
    GrtrOrEqual,
}

#[no_mangle]
pub extern "C" fn arithcompare(
    obj1: LispObject,
    obj2: LispObject,
    comparison: ArithComparison,
) -> LispObject {
    // If either arg is floating point, set F1 and F2 to the 'double'
    // approximations of the two arguments, and set FNEQ if floating-point
    // comparison reports that F1 is not equal to F2, possibly because F1
    // or F2 is a NaN.  Regardless, set I1 and I2 to integers that break
    // ties if the floating-point comparison is either not done or reports
    // equality.

    let (i1, i2, f1, f2) = match (
        obj1.as_number_coerce_marker_or_error(),
        obj2.as_number_coerce_marker_or_error(),
    ) {
        (LispNumber::Fixnum(n1), LispNumber::Fixnum(n2)) => (n1, n2, 0., 0.),
        (LispNumber::Fixnum(n1), LispNumber::Float(n2)) => {
            // Compare an integer NUM1 to a float NUM2.  This is the
            // converse of comparing float to integer (see below).
            (n1, n1 as f64 as EmacsInt, n1 as f64, n2)
        }
        (LispNumber::Float(n1), LispNumber::Fixnum(n2)) => {
            // Compare a float NUM1 to an integer NUM2 by converting the
            // integer I2 (i.e., NUM2) to the double F2 (a conversion that
            // can round on some platforms, if I2 is large enough), and then
            // converting F2 back to the integer I1 (a conversion that is
            // always exact), so that I1 exactly equals ((double) NUM2).  If
            // floating-point comparison reports a tie, NUM1 = F1 = F2 = I1
            // (exactly) so I1 - I2 = NUM1 - NUM2 (exactly), so comparing I1
            // to I2 will break the tie correctly.
            (n2 as f64 as EmacsInt, n2, n1, n2 as f64)
        }
        (LispNumber::Float(n1), LispNumber::Float(n2)) => (0, 0, n1, n2),
    };
    let fneq = f1 != f2;

    let result = match comparison {
        ArithComparison::Equal => !fneq && i1 == i2,
        ArithComparison::Notequal => fneq || i1 != i2,
        ArithComparison::Less => if fneq { f1 < f2 } else { i1 < i2 },
        ArithComparison::LessOrEqual => if fneq { f1 <= f2 } else { i1 <= i2 },
        ArithComparison::Grtr => if fneq { f1 > f2 } else { i1 > i2 },
        ArithComparison::GrtrOrEqual => if fneq { f1 >= f2 } else { i1 >= i2 },
    };

    LispObject::from_bool(result)
}

fn arithcompare_driver(args: &[LispObject], comparison: ArithComparison) -> LispObject {
    LispObject::from_bool(args.windows(2).all(|i| {
        arithcompare(i[0], i[1], comparison).is_not_nil()
    }))
}

/// Return t if args, all numbers or markers, are equal.
/// usage: (= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(name = "=", min = "1")]
fn eqlsign(args: &mut [LispObject]) -> LispObject {
    arithcompare_driver(args, ArithComparison::Equal)
}

/// Return t if each arg (a number or marker), is less than the next arg.
/// usage: (< NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(name = "<", min = "1")]
fn lss(args: &mut [LispObject]) -> LispObject {
    arithcompare_driver(args, ArithComparison::Less)
}

/// Return t if each arg (a number or marker) is greater than the next arg.
/// usage: (> NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(name = ">", min = "1")]
fn gtr(args: &mut [LispObject]) -> LispObject {
    arithcompare_driver(args, ArithComparison::Grtr)
}

/// Return t if each arg (a number or marker) is less than or equal to the next.
/// usage: (<= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(name = "<=", min = "1")]
fn leq(args: &mut [LispObject]) -> LispObject {
    arithcompare_driver(args, ArithComparison::LessOrEqual)
}

/// Return t if each arg (a number or marker) is greater than or equal to the next.
/// usage: (>= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(name = ">=", min = "1")]
fn geq(args: &mut [LispObject]) -> LispObject {
    arithcompare_driver(args, ArithComparison::GrtrOrEqual)
}

/// Return t if first arg is not equal to second arg.  Both must be numbers or markers.
#[lisp_fn(name = "/=")]
fn neq(num1: LispObject, num2: LispObject) -> LispObject {
    arithcompare(num1, num2, ArithComparison::Notequal)
}
