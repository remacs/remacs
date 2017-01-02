// TODO: this is just to ensure that Srust_mod does not generate a
// warning. However, as it's defined with a macro, there doesn't seem
// to be a way to disable the warning for just that variable.
#![allow(non_upper_case_globals)]

#[macro_use]
extern crate lazy_static;

extern crate libc;

mod lisp;
mod marker;
mod eval;
mod floatfns;

use std::os::raw::c_char;
use std::ptr;
use lisp::{LispObject, LispSubr, PvecType, defsubr, make_number, PSEUDOVECTOR_AREA_BITS, XINT,
           VectorLikeHeader, Qarith_error};
use eval::xsignal0;

#[no_mangle]
#[allow(unused_variables)]
pub unsafe extern "C" fn rust_mod(x: LispObject, y: LispObject) -> LispObject {
    let x = lisp::check_number_coerce_marker(x);
    let y = lisp::check_number_coerce_marker(y);

    if lisp::FLOATP(x) || lisp::FLOATP(y) {
        return floatfns::fmod_float(x, y);
    }

    let mut i1 = XINT(x);
    let i2 = XINT(y);

    if i2 == 0 {
        xsignal0(Qarith_error);
    }

    i1 %= i2;

    // Ensure that the remainder has the correct sign.
    if i2 < 0 && i1 > 0 || i2 > 0 && i1 < 0 {
        i1 += i2
    }

    make_number(i1)
}

#[allow(non_upper_case_globals)]
lazy_static! {
    // TODO: this is blindly hoping we have the correct alignment.
    // We should ensure we have GCALIGNMENT (8 bytes).
    static ref Srust_mod: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (rust_mod as *const libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("rust-mod\0".as_ptr()) as *const c_char,
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

#[no_mangle]
#[allow(non_snake_case)]
pub unsafe extern "C" fn rust_init_syms() {
    defsubr(&*Srust_mod);
}
