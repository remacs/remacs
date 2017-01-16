#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

extern crate libc;

mod lisp;
mod lists;
mod marker;
mod eval;
mod floatfns;
mod math;
mod strings;
mod symbols;
mod globals;

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

extern "C" {
    fn defsubr(sname: *const LispSubr);
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
        defsubr(&*symbols::Ssymbolp);
        defsubr(&*lists::Sconsp);
        defsubr(&*lists::Ssetcar);
        defsubr(&*lists::Ssetcdr);
        defsubr(&*lists::Scar);
        defsubr(&*lists::Scdr);
        defsubr(&*strings::Sstringp);
        defsubr(&*strings::Seq);
        defsubr(&*strings::Snull);

        floatfns::init_float_syms();
    }
}
