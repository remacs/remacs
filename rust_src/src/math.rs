extern crate libc;

use floatfns;
use lisp;

use std::os::raw::c_char;
use std::ptr;
use std::slice;
use libc::ptrdiff_t;

use lisp::{LispSubr, MANY, PSEUDOVECTOR_AREA_BITS, PvecType, VectorLikeHeader, LispObject,
           Qarith_error, XINT, make_number, EmacsInt};
use eval::xsignal0;

fn Fmod(x: LispObject, y: LispObject) -> LispObject {
    let x = lisp::check_number_coerce_marker(x);
    let y = lisp::check_number_coerce_marker(y);

    if lisp::FLOATP(x) || lisp::FLOATP(y) {
        return floatfns::fmod_float(x, y);
    }

    let mut i1 = XINT(x);
    let i2 = XINT(y);

    if i2 == 0 {
        unsafe {
            xsignal0(Qarith_error);
        }
    }

    i1 %= i2;

    // Ensure that the remainder has the correct sign.
    if i2 < 0 && i1 > 0 || i2 > 0 && i1 < 0 {
        i1 += i2
    }

    make_number(i1)
}

lazy_static! {
    // TODO: this is blindly hoping we have the correct alignment.
    // We should ensure we have GCALIGNMENT (8 bytes).
    pub static ref Smod: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Fmod as *const libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("mod\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        // TODO: There's some magic somewhere in core Emacs that means
        // `(fn X Y)` is added to the docstring automatically. We
        // should do something similar.
        doc: ("Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers or markers.

(fn X Y)\0".as_ptr()) as *const c_char,
    };
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
    Max,
    Min,
}

extern "C" {
    fn float_arith_driver(accum: f64, argnum: ptrdiff_t, code: ArithOp, nargs: ptrdiff_t, args: *const LispObject) -> LispObject;
}

/// Given an array of LispObject, reduce over them according to the
/// arithmetic operation specified.
///
/// Modifies the array in place.
#[no_mangle]
pub extern "C" fn arith_driver(code: ArithOp, nargs: ptrdiff_t, args: *mut LispObject) ->  LispObject {
    let mut accum: EmacsInt = match code {
        ArithOp::Add | ArithOp::Sub | ArithOp::Logior | ArithOp::Logxor => 0,
        ArithOp::Logand => -1,
        _ => 1,
    };

    // TODO: use better variable names rather than just copying the C.
    let mut overflow = false;
    let mut ok_accum = accum;
    let mut ok_args: ptrdiff_t = 0;

    let mut args_slice = unsafe { slice::from_raw_parts_mut(args, nargs as usize) };

    for (argnum, val) in args_slice.iter_mut().enumerate() {
        if !overflow {
            ok_args = argnum as ptrdiff_t;
            ok_accum = accum;
        }

        let coerced_val = lisp::check_number_coerce_marker(*val);

        if lisp::FLOATP(coerced_val) {
            unsafe {
                return float_arith_driver(ok_accum as f64, ok_args, code, nargs, args);
            }
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
                    if nargs == 1 {
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
                if nargs > 1 && argnum == 0 {
                    accum = next;
                } else {
                    if next == 0 {
                        unsafe {
                            xsignal0(Qarith_error);
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

#[no_mangle]
pub extern "C" fn Fplus(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Add, nargs, args)
}

// TODO: define a macro that saves us repeating lazy_static!.
lazy_static! {
    pub static ref Splus: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Fplus as *const libc::c_void),
        min_args: 0,
        max_args: MANY,
        symbol_name: ("+\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return sum of any number of arguments, which are numbers or markers.

(fn &rest NUMBERS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

#[no_mangle]
pub extern "C" fn Fminus(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Sub, nargs, args)
}

// TODO: define a macro that saves us repeating lazy_static!.
lazy_static! {
    pub static ref Sminus: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Fminus as *const libc::c_void),
        min_args: 0,
        max_args: MANY,
        symbol_name: ("-\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Negate number or subtract numbers or markers and return the result.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.

(fn &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

#[no_mangle]
pub extern "C" fn Ftimes(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Mult, nargs, args)
}

lazy_static! {
    pub static ref Stimes: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Ftimes as *const libc::c_void),
        min_args: 0,
        max_args: MANY,
        symbol_name: ("*\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return product of any number of arguments, which are numbers or markers.

(fn &optional NUMBER-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

fn Flogand(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Logand, nargs, args)
}

lazy_static! {
    pub static ref Slogand: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Flogand as *const libc::c_void),
        min_args: 0,
        max_args: MANY,
        symbol_name: ("logand\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return bitwise-and of all the arguments.
Arguments may be integers, or markers converted to integers.

(fn &rest INTS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

fn Flogior(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Logior, nargs, args)
}

lazy_static! {
    pub static ref Slogior: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Flogior as *const libc::c_void),
        min_args: 0,
        max_args: MANY,
        symbol_name: ("logior\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return bitwise-or of all the arguments.
Arguments may be integers, or markers converted to integers.

(fn &rest INTS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

fn Flogxor(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Logxor, nargs, args)
}

lazy_static! {
    pub static ref Slogxor: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Flogxor as *const libc::c_void),
        min_args: 0,
        max_args: MANY,
        symbol_name: ("logxor\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers converted to integers.

(fn &rest INTS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

#[no_mangle]
pub extern "C" fn Fmax(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Max, nargs, args)
}

lazy_static! {
    pub static ref Smax: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Fmax as *const libc::c_void),
        min_args: 1,
        max_args: MANY,
        symbol_name: ("max\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return largest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}

#[no_mangle]
pub extern "C" fn Fmin(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject {
    arith_driver(ArithOp::Min, nargs, args)
}

lazy_static! {
    pub static ref Smin: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as ptrdiff_t,
        },
        function: (Fmin as *const libc::c_void),
        min_args: 1,
        max_args: MANY,
        symbol_name: ("min\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return smallest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.

(fn NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)\0".as_ptr()) as *const c_char,
    };
}
