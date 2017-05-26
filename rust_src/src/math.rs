use std::ptr;
use libc::ptrdiff_t;

use floatfns;
use lisp;
use lisp::{LispObject, XINT, make_number, CHECK_TYPE, LispType};
use eval::xsignal0;
use remacs_sys::{EmacsInt, Lisp_Object, Qarith_error, Qnumberp};

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

fn plus(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Add, args)
}

defun_many!("+",
            Fplus,
            Splus,
            plus,
            0,
            ptr::null(),
            "Return sum of any number of arguments, which are numbers or markers.

(fn &rest \
             NUMBERS-OR-MARKERS)");

fn minus(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Sub, args)
}

defun_many!("-",
            Fminus,
            Sminus,
            minus,
            0,
            ptr::null(),
            "Negate number or subtract numbers or markers and return the result.
With one arg, \
             negates it.  With more than one arg,
subtracts all but the first from the first.

\
             (fn &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)");

fn times(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Mult, args)
}

defun_many!("*",
            Ftimes,
            Stimes,
            times,
            0,
            ptr::null(),
            "Return product of any number of arguments, which are numbers or markers.

(fn \
             &optional NUMBER-OR-MARKERS)");

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

defun_many!("/",
            Fquo,
            Squo,
            quo,
            1,
            ptr::null(),
            "Divide number by divisors and return the result.
With two or more arguments, return \
             first argument divided by the rest.
With one argument, return 1 divided by the \
             argument.
The arguments must be numbers or markers.

(fn NUMBER &rest DIVISORS)");

fn logand(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logand, args)
}

defun_many!("logand",
            Flogand,
            Slogand,
            logand,
            0,
            ptr::null(),
            "Return bitwise-and of all the arguments.
Arguments may be integers, or markers \
             converted to integers.

(fn &rest INTS-OR-MARKERS)");

fn logior(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logior, args)
}

defun_many!("logior",
            Flogior,
            Slogior,
            logior,
            0,
            ptr::null(),
            "Return bitwise-or of all the arguments.
Arguments may be integers, or markers \
             converted to integers.

(fn &rest INTS-OR-MARKERS)");

fn logxor(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Logxor, args)
}

defun_many!("logxor",
            Flogxor,
            Slogxor,
            logxor,
            0,
            ptr::null(),
            "Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or \
             markers converted to integers.

(fn &rest INTS-OR-MARKERS)");

fn max(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Max, args)
}

defun_many!("max",
            Fmax,
            Smax,
            max,
            1,
            ptr::null(),
            "Return largest of all the arguments (which must be numbers or markers).
The value \
             is always a number; markers are converted to numbers.

(fn NUMBER-OR-MARKER &rest \
             NUMBERS-OR-MARKERS)");

fn min(args: &mut [LispObject]) -> LispObject {
    arith_driver(ArithOp::Min, args)
}

defun_many!("min",
            Fmin,
            Smin,
            min,
            1,
            ptr::null(),
            "Return smallest of all the arguments (which must be numbers or markers).
The value \
             is always a number; markers are converted to numbers.

(fn NUMBER-OR-MARKER &rest \
             NUMBERS-OR-MARKERS)");


fn abs(obj: LispObject) -> LispObject {
    CHECK_TYPE(obj.is_number(),
               LispObject::from_raw(unsafe { Qnumberp }),
               obj); // does not return on failure

    match obj.get_type() {
        LispType::Lisp_Float => LispObject::from_float(obj.to_float().unwrap().abs()),
        _ => make_number(obj.to_fixnum().unwrap().abs() as EmacsInt),
    }

}

defun!("abs",
       Fabs(obj),
       Sabs,
       abs,
       1,
       1,
       ptr::null(),
       "Return the absolute value of ARG.

(fn ARG)");
