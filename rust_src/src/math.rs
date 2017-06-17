//! Functions doing math on numbers.

use libc::ptrdiff_t;

use floatfns;
use lisp::{LispObject, check_number_coerce_marker};
use eval::xsignal0;
use remacs_sys::{EmacsInt, Lisp_Object, Qarith_error, Qnumberp, wrong_type_argument};
use remacs_macros::lisp_fn;

/// Return X modulo Y.
/// The result falls between zero (inclusive) and Y (exclusive).
/// Both X and Y must be numbers or markers.
/// (fn X Y)
#[lisp_fn(name = "mod", c_name = "mod")]
fn lisp_mod(x: LispObject, y: LispObject) -> LispObject {
    let x = check_number_coerce_marker(x);
    let y = check_number_coerce_marker(y);

    if x.is_float() || y.is_float() {
        let ret = floatfns::fmod_float(x.to_raw(), y.to_raw());
        return LispObject::from_raw(ret);
    }

    // TODO: too much checking here
    let mut i1 = x.as_fixnum().unwrap();
    let i2 = y.as_fixnum().unwrap();

    if i2 == 0 {
        unsafe {
            xsignal0(LispObject::from_raw(Qarith_error));
        }
    }

    i1 %= i2;

    // Ensure that the remainder has the correct sign.
    if i2 < 0 && i1 > 0 || i2 > 0 && i1 < 0 {
        i1 += i2
    }

    LispObject::from_fixnum(i1)
}

#[repr(C)]
enum ArithOp {
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
    Max,
    Min,
}

extern "C" {
    fn float_arith_driver(
        accum: f64,
        argnum: ptrdiff_t,
        code: ArithOp,
        nargs: ptrdiff_t,
        args: *const Lisp_Object,
    ) -> Lisp_Object;
}

/// Given an array of LispObject, reduce over them according to the
/// arithmetic operation specified.
///
/// Modifies the array in place.
fn arith_driver(code: ArithOp, args: &mut [LispObject]) -> LispObject {
    let mut accum: EmacsInt = match code {
        ArithOp::Add | ArithOp::Sub | ArithOp::Logior | ArithOp::Logxor => 0,
        ArithOp::Logand => -1,
        _ => 1,
    };

    // TODO: use better variable names rather than just copying the C.
    let mut overflow = false;
    let mut ok_accum = accum;
    let mut ok_args: ptrdiff_t = 0;

    let args_clone = args.to_vec();

    for (argnum, val) in args.iter_mut().enumerate() {
        if !overflow {
            ok_args = argnum as ptrdiff_t;
            ok_accum = accum;
        }

        let coerced_val = check_number_coerce_marker(*val);

        if coerced_val.is_float() {
            let mut args: Vec<Lisp_Object> = args_clone.iter().map(|v| v.to_raw()).collect();
            let ret = unsafe {
                float_arith_driver(
                    ok_accum as f64,
                    ok_args,
                    code,
                    args.len() as ptrdiff_t,
                    args.as_mut_ptr(),
                )
            };

            return LispObject::from_raw(ret);
        }

        *val = coerced_val;
        // TODO: too much checking here
        let next = (*val).as_fixnum().unwrap();

        match code {
            ArithOp::Add => {
                if accum.checked_add(next).is_none() {
                    overflow = true;
                }
                accum = accum.wrapping_add(next);
            }
            ArithOp::Sub => {
                if argnum == 0 {
                    if args_clone.len() == 1 {
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
                if args_clone.len() > 1 && argnum == 0 {
                    accum = next;
                } else {
                    if next == 0 {
                        unsafe {
                            xsignal0(LispObject::from_raw(Qarith_error));
                        }
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
            ArithOp::Max => {
                if argnum == 0 || next > accum {
                    accum = next;
                }
            }
            ArithOp::Min => {
                if argnum == 0 || next < accum {
                    accum = next;
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
            let mut args: Vec<::remacs_sys::Lisp_Object> =
                args.iter().map(|arg| arg.to_raw()).collect();
            let ret = unsafe {
                float_arith_driver(
                    0.0,
                    0,
                    ArithOp::Div,
                    args.len() as ptrdiff_t,
                    args.as_mut_ptr(),
                )
            };
            return LispObject::from_raw(ret);
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

/// Return largest of all the arguments (which must be numbers or markers).
/// The value is always a number; markers are converted to numbers.
/// usage: (fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(min = "1")]
fn max(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Max, args)
}

/// Return smallest of all the arguments (which must be numbers or markers).
/// The value is always a number; markers are converted to numbers.
/// usage: (fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)
#[lisp_fn(min = "1")]
fn min(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Min, args)
}

/// Return the absolute value of ARG.
#[lisp_fn]
fn abs(arg: LispObject) -> LispObject {
    if !arg.is_number() {
        unsafe {
            wrong_type_argument(Qnumberp, arg.to_raw());
        }
    }

    match arg.as_float() {
        Some(f) => LispObject::from_float(f.abs()),
        _ => {
            let n = arg.as_fixnum().unwrap();
            LispObject::from_fixnum(n.abs())
        }
    }
}
