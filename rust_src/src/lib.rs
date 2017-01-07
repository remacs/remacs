#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

extern crate libc;

mod lisp;
mod marker;
mod eval;
mod floatfns;
mod math;

use std::os::raw::c_char;
use std::ptr;
use lisp::{LispObject, LispSubr, PvecType, PSEUDOVECTOR_AREA_BITS,
           VectorLikeHeader, Qt, Qnil};

// These need to be exported as bytecode.c depends upon them.
pub use math::Fplus;
pub use math::Fminus;
pub use math::Ftimes;

extern "C" {
    fn defsubr(sname: *const LispSubr);
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
        symbol_name: ("symbolp\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return t if OBJECT is a symbol.

(fn OBJECT)\0".as_ptr()) as *const c_char,
    };
}

#[no_mangle]
pub extern "C" fn rust_init_syms() {
    unsafe {
        defsubr(&*math::Smod);
        defsubr(&*math::Splus);
        defsubr(&*math::Sminus);
        defsubr(&*math::Stimes);
        defsubr(&*math::Slogand);
        defsubr(&*math::Slogior);
        defsubr(&*math::Slogxor);
        defsubr(&*Ssymbolp);
    }
}
