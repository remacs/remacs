#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]

#![cfg_attr(feature = "strict", deny(warnings))]

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test), target_os = "macos"))]
extern crate alloc_unexecmacosx;

#[macro_use]
extern crate lazy_static;

extern crate remacs_sys;
extern crate remacs_lib;
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
mod character;
mod base64;

use remacs_sys::Lisp_Subr;

pub use base64::base64_encode_1;
pub use base64::base64_decode_1;

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
pub use symbols::Fsymbolp;

// These need to be exported as marker.c depends upon them.
pub use marker::CHECK_MARKER;

// Defined in lisp.h and widely used in the C codebase.
pub use lisp::CHECK_STRING;

extern "C" {
    fn defsubr(sname: *const Lisp_Subr);
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
        defsubr(&*strings::Sbase64_decode_string);
        defsubr(&*strings::Snull);
        defsubr(&*character::Smax_char);

        floatfns::init_float_syms();
    }
}

#[no_mangle]
pub extern "C" fn rust_print_lisp_object(v: lisp::LispObject) {
    println!("{:?}", v)
}
