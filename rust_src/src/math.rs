use std::ptr;
use libc::ptrdiff_t;

use floatfns;
use lisp;
use lisp::{LispObject, Qarith_error, XINT, make_number, CHECK_TYPE, Qnumberp, LispType};
use eval::xsignal0;
use remacs_sys::{EmacsInt, Lisp_Object};
use remacs_macros::{lisp_fn, lisp_doc};

fn lisp_mod(x: LispObject, y: LispObject) -> LispObject {
    let x = lisp::check_number_coerce_marker(x);
    let y = lisp::check_number_coerce_marker(y);

    if lisp::FLOATP(x) || lisp::FLOATP(y) {
        let ret = floatfns::fmod_float(x.to_raw(), y.to_raw());
        return LispObject::from_raw(ret);
    }

    let mut i1 = XINT(x);
    let i2 = XINT(y);

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

    make_number(i1)
}

// TODO: There's some magic somewhere in core Emacs that means
// `(fn X Y)` is added to the docstring automatically. We
// should do something similar.
defun!("mod",
       Fmod(x, y),
       Smod,
       lisp_mod,
       2,
       2,
       ptr::null(),
       "Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers or markers.

(fn X Y)");

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
    fn float_arith_driver(accum: f64,
                          argnum: ptrdiff_t,
                          code: ArithOp,
                          nargs: ptrdiff_t,
                          args: *const Lisp_Object)
                          -> Lisp_Object;
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

        let coerced_val = lisp::check_number_coerce_marker(*val);

        if lisp::FLOATP(coerced_val) {
            let mut args: Vec<Lisp_Object> = args_clone.iter().map(|v| v.to_raw()).collect();
            let ret = unsafe {
                float_arith_driver(ok_accum as f64,
                                   ok_args,
                                   code,
                                   args.len() as ptrdiff_t,
                                   args.as_mut_ptr())
            };

            return LispObject::from_raw(ret);
        }

        *val = coerced_val;
        let next = lisp::XINT(*val);

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

    make_number(accum)
}

#[lisp_fn(name = "+")]
#[lisp_doc("Return sum of any number of arguments, which are numbers or markers")]
#[lisp_doc("(fn &rest NUMBERS-OR-MARKERS)")]
fn plus(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Add, args)
}

#[lisp_fn(name = "-")]
#[lisp_doc("Negate number or subtract numbers or markers and return the result.")]
#[lisp_doc("With one arg, negates it. With more than one arg, subtracts all but")]
#[lisp_doc("the first from the first")]
#[lisp_doc("(fn &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)")]
fn minus(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Sub, args)
}

#[lisp_fn(name = "*")]
#[lisp_doc("Return product of any number of arguments, which are numbers or markers.")]
#[lisp_doc("(fn &optional NUMBER-OR-MARKERS)")]
fn times(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Mult, args)
}

#[lisp_fn(name = "/", min = "1")]
#[lisp_doc("Divide number by divisors and return the result.")]
#[lisp_doc("With two or more arguments, return first argument divided by the rest.")]
#[lisp_doc("With one argument, return 1 divided by te argument.")]
#[lisp_doc("The arguments must be numbers or markers")]
#[lisp_doc("(fn NUMBER &rest DIVISORS)")]
fn quo(args: &mut [LispObject]) -> LispObject {
    for argnum in 2..args.len() {
        let arg = args[argnum];
        if lisp::FLOATP(arg) {
            let mut args: Vec<::remacs_sys::Lisp_Object> =
                args.iter().map(|arg| arg.to_raw()).collect();
            let ret = unsafe {
                float_arith_driver(0.0,
                                   0,
                                   ArithOp::Div,
                                   args.len() as ptrdiff_t,
                                   args.as_mut_ptr())
            };
            return LispObject::from_raw(ret);
        }
    }
    arith_driver(ArithOp::Div, args)
}

#[lisp_fn(name = "logand")]
#[lisp_doc("Return bitwise-and of all the arguments.")]
#[lisp_doc("Arguments may be integers, or markers, converted to integers.")]
fn logand(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logand, args)
}

#[lisp_fn(name = "logior")]
#[lisp_doc("Return bitwise-or of all the arguments.")]
#[lisp_doc("Arguments may be integers, or markers converted to integers.")]
#[lisp_doc("(fn &rest INTS-OR-MARKERS)")]
fn logior(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logior, args)
}

#[lisp_fn(name = "logxor")]
#[lisp_doc("Return bitwise-exclusive-or of all the arguments.")]
#[lisp_doc("Arguments may be integers, or markers converted to integers.")]
#[lisp_doc("(fn &rest INTS-OR-MARKERS)")]
fn logxor(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logxor, args)
}

#[lisp_fn(name = "max", min = "1")]
#[lisp_doc("Return largest of all the arguments (which must be numbers or markers).")]
#[lisp_doc("The value is always a number; markers are converted to numbers.")]
#[lisp_doc("(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)")]
fn max(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Max, args)
}

#[lisp_fn(name = "min", min = "1")]
#[lisp_doc("Return smallest of all the arguments (which must be numbers or markers).")]
#[lisp_doc("The value is always a number; markers are converted to numbers.")]
#[lisp_doc("fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS")]
fn min(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Min, args)
}


#[lisp_fn(name = "abs", min = "1")]
#[lisp_doc("Return the absolute value of ARG")]
#[lisp_doc("(fn ARG)")]
fn abs(obj: LispObject) -> LispObject {
    CHECK_TYPE(obj.is_number(),
               LispObject::from_raw(unsafe { Qnumberp }),
               obj); // does not return on failure

    match obj.get_type() {
        LispType::Lisp_Float => LispObject::from_float(obj.to_float().unwrap().abs()),
        _ => make_number(obj.to_fixnum().unwrap().abs() as EmacsInt),
    }
}
