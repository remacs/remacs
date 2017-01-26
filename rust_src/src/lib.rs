#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

#![cfg_attr(feature = "strict", deny(warnings))]

#[macro_use]
extern crate lazy_static;

extern crate libc;

mod lisp;
mod lists;
mod marker;
mod eval;
mod floatfns;
mod math;
mod numbers;
mod strings;
mod symbols;
mod globals;
mod character;
#[macro_use]
mod variables;

use lisp::LispSubr;

// These need to be exported as bytecode.c depends upon them.
pub use math::Fplus;
pub use math::Fminus;
pub use math::Ftimes;
pub use math::Fmax;
pub use math::Fmin;
pub use math::Fquo;

// Widely used in the C codebase.
pub use lists::Fsetcar;
pub use lists::Fsetcdr;
pub use lists::Fcar;
pub use lists::Fcdr;
pub use lists::Flistp;
pub use floatfns::extract_float;
pub use floatfns::fmod_float;

// These need to be exported as marker.c depends upon them.
pub use marker::CHECK_MARKER;

// Defined in lisp.h and widely used in the C codebase.
pub use lisp::CHECK_STRING;

use lisp::LispObject;
use lisp::EmacsInt;
use lisp::EMACS_INT_MAX;
use lisp::INTTYPEBITS;

const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> INTTYPEBITS;
const MOST_NEGATIVE_FIXNUM: EmacsInt = -1 - MOST_POSITIVE_FIXNUM;

extern "C" {
    fn defsubr(sname: *const LispSubr);
    fn rust_make_symbol_constant(sym: lisp::LispObject);
    fn intern_c_string_1(str_: *const libc::c_char) -> LispObject;
}

#[no_mangle]
pub extern "C" fn rust_init_syms() {
    unsafe {
        defsubr(&*lists::Satom);
        defsubr(&*lists::Slistp);
        defsubr(&*lists::Snlistp);
        defsubr(&*math::Smod);
        defsubr(&*math::Splus);
        defsubr(&*math::Sminus);
        defsubr(&*math::Stimes);
        defsubr(&*math::Squo);
        defsubr(&*math::Slogand);
        defsubr(&*math::Slogior);
        defsubr(&*math::Slogxor);
        defsubr(&*math::Smax);
        defsubr(&*math::Smin);
        defsubr(&*math::Sabs);
        defsubr(&*numbers::Sintegerp);
        defsubr(&*numbers::Sinteger_or_marker_p);
        defsubr(&*numbers::Sfloatp);
        defsubr(&*numbers::Snatnump);
        defsubr(&*numbers::Snumber_or_marker_p);
        defsubr(&*numbers::Snumberp);
        defsubr(&*symbols::Ssymbolp);
        defsubr(&*lists::Sconsp);
        defsubr(&*lists::Ssetcar);
        defsubr(&*lists::Ssetcdr);
        defsubr(&*lists::Scar);
        defsubr(&*lists::Scdr);
        defsubr(&*strings::Sstringp);
        defsubr(&*strings::Seq);
        defsubr(&*strings::Sbase64_encode_string);
        defsubr(&*strings::Snull);
        defsubr(&*character::Smax_char);

        floatfns::init_float_syms();

        // The largest value that is representable in a Lisp integer
        defvar!(b"most-positive-fixnum\0", f_Vmost_positive_fixnum);
        rust_make_symbol_constant(intern_c_string_1(b"most-positive-fixnum\0"
            .as_ptr() as *const libc::c_char));
        globals::globals.f_Vmost_positive_fixnum =
            LispObject::from_fixnum_unchecked(MOST_POSITIVE_FIXNUM);

        // The smallest value that is representable in a Lisp integer.
        defvar!(b"most-negative-fixnum\0", f_Vmost_negative_fixnum);
        rust_make_symbol_constant(intern_c_string_1(b"most-negative-fixnum\0"
            .as_ptr() as *const libc::c_char));
        globals::globals.f_Vmost_negative_fixnum =
            LispObject::from_fixnum_unchecked(MOST_NEGATIVE_FIXNUM);
    }
}

#[no_mangle]
pub extern "C" fn rust_print_lisp_object(v: lisp::LispObject) {
    println!("{:?}", v)
}
