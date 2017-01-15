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
mod atimer;

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

// FIXME part of the used function is used inside mod atimer,
// we should stop exposing those function after the porting
// is complete.
pub use atimer::block_atimers;
pub use atimer::unblock_atimers;
pub use atimer::schedule_atimer;
pub use atimer::handle_alarm_signal;

pub use atimer::cancel_atimer;
pub use atimer::stop_other_atimers;
pub use atimer::run_all_atimers;

// These need to be exported as marker.c depends upon them.
pub use marker::CHECK_MARKER;

// Defined in lisp.h and widely used in the C codebase.
pub use lisp::CHECK_STRING;

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
        defsubr(&*strings::Snull);
        defsubr(&*character::Smax_char);

        floatfns::init_float_syms();
    }
}
