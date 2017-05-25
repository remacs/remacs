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
extern crate sha1;
extern crate sha2;

mod lisp;
mod lists;
mod marker;
mod eval;
mod floatfns;
mod math;
mod numbers;
mod objects;
mod strings;
mod symbols;
mod character;
mod base64;
mod crypto;
mod str2sig;

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
pub use lists::Fcar_safe;
pub use lists::Fcdr_safe;
pub use lists::Flistp;
pub use lists::Fnthcdr;
pub use lists::Fnth;
pub use lists::Fmemq;
pub use lists::Fmember;
pub use lists::Fassq;
pub use lists::Fassoc;
pub use lists::Frassq;
pub use lists::Frassoc;
pub use lists::Fdelq;
pub use lists::Fplist_get;
pub use lists::Fplist_member;
pub use lists::Fplist_put;
pub use floatfns::extract_float;
pub use floatfns::fmod_float;
pub use objects::Fequal;
pub use objects::Fequal_including_properties;
pub use symbols::Fsymbolp;
pub use strings::Fstring_equal;

// Cryptographic functions used in the C codebase.
pub use crypto::sha256_buffer;
pub use crypto::sha1_buffer;
pub use crypto::sha384_buffer;
pub use crypto::sha512_buffer;
pub use crypto::sha224_buffer;
pub use crypto::sha1_ctx::sha1_ctx_new;
pub use crypto::sha1_ctx::sha1_process_bytes;
pub use crypto::sha1_ctx::sha1_finish_ctx;

// Used in process.c
pub use str2sig::str2sig;

// These need to be exported as marker.c depends upon them.
pub use marker::CHECK_MARKER;

// Defined in lisp.h and widely used in the C codebase.
pub use lisp::CHECK_STRING;
pub use lisp::CHECK_NUMBER;
pub use lisp::CHECK_LIST_END;
pub use lisp::CHECK_CONS;

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
        defsubr(&*objects::Snull);
        defsubr(&*objects::Seq);
        defsubr(&*objects::Seql);
        defsubr(&*objects::Sequal);
        defsubr(&*objects::Sequal_including_properties);
        defsubr(&*symbols::Ssymbolp);
        defsubr(&*lists::Sconsp);
        defsubr(&*lists::Ssetcar);
        defsubr(&*lists::Ssetcdr);
        defsubr(&*lists::Scar);
        defsubr(&*lists::Scdr);
        defsubr(&*lists::Scar_safe);
        defsubr(&*lists::Scdr_safe);
        defsubr(&*lists::Snthcdr);
        defsubr(&*lists::Snth);
        defsubr(&*lists::Smemq);
        defsubr(&*lists::Smemql);
        defsubr(&*lists::Smember);
        defsubr(&*lists::Sassq);
        defsubr(&*lists::Sassoc);
        defsubr(&*lists::Srassq);
        defsubr(&*lists::Srassoc);
        defsubr(&*lists::Sdelq);
        defsubr(&*lists::Splist_get);
        defsubr(&*lists::Slax_plist_get);
        defsubr(&*lists::Splist_member);
        defsubr(&*lists::Splist_put);
        defsubr(&*lists::Slax_plist_put);
        defsubr(&*marker::Smarkerp);
        defsubr(&*strings::Sstringp);
        defsubr(&*strings::Sbase64_encode_string);
        defsubr(&*strings::Sbase64_decode_string);
        defsubr(&*strings::Sstring_bytes);
        defsubr(&*strings::Sstring_equal);
        defsubr(&*character::Smax_char);
        defsubr(&*character::Scharacterp);

        floatfns::init_float_syms();
    }
}
