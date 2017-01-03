#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

extern crate libc;

mod lisp;
mod marker;
mod eval;
mod floatfns;

use std::os::raw::c_char;
use std::ptr;
use lisp::{LispObject, LispSubr, PvecType, make_number, PSEUDOVECTOR_AREA_BITS, XINT,
           VectorLikeHeader, Qarith_error, Qt, Qnil};
use eval::xsignal0;

#[allow(dead_code)]
#[repr(C)]
enum ArithOp {
    Add,
    Sub,
    Mult,
    Div,
    Logand,
    Logior,
    Logxor,
    Max,
    Min,
}

extern "C" {
    fn defsubr(sname: *const LispSubr);
    fn arith_driver(code: ArithOp, nargs: libc::ptrdiff_t, args: LispObject) -> LispObject;
}

fn Fsymbolp(object: LispObject) -> LispObject {
    if lisp::SYMBOLP(object) {
        unsafe {
            Qt
        }
    } else {
        Qnil
    }
}

lazy_static! {
    static ref Ssymbolp: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fsymbolp as *const libc::c_void),
        min_args: 1,
        max_args: 1,
        symbol_name: ("rust-symbolp\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return t if OBJECT is a symbol.

(fn OBJECT)\0".as_ptr()) as *const c_char,
    };
}

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
    static ref Smod: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
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

/// Used to denote functions that have no limit on the maximum number
/// of arguments.
const MANY: i16 = -2;

#[no_mangle]
pub extern "C" fn Fplus(nargs: libc::ptrdiff_t, args: LispObject) -> LispObject {
    unsafe {
        return arith_driver(ArithOp::Add, nargs, args)
    }
}

// TODO: define a macro that saves us repeating lazy_static!.
lazy_static! {
    static ref Splus: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
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
pub extern "C" fn rust_init_syms() {
    unsafe {
        defsubr(&*Smod);
        defsubr(&*Ssymbolp);
        defsubr(&*Splus);
    }
}
