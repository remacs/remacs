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
pub use lists::Flistp;
pub use floatfns::extract_float;
pub use floatfns::fmod_float;
pub use symbols::Fsymbolp;

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

extern "C" {
    fn defsubr(sname: *const Lisp_Subr);
}

/// Define Emacs Lisp native functions.
///
/// TODO: Remove `S` prefix of symbols (requires *nightly*).
/// TODO: Declare modules instead of importing it manually
macro_rules! define_functions {
    ($($module_name:ident = { $($native_symbol:ident),+ }),+) => {
        unsafe {
        $($(
            $crate::defsubr(&*$module_name::$native_symbol);
        )+)+
        }
    }
}


#[no_mangle]
pub extern "C" fn rust_init_syms() {
    define_functions! {
        lists = {
            Satom,
            Slistp,
            Snlistp,
            Sconsp,
            Ssetcar,
            Ssetcdr,
            Scar,
            Scdr
        },
        math = {
            Smod,
            Splus,
            Sminus,
            Stimes,
            Squo,
            Slogand,
            Slogior,
            Slogxor,
            Smax,
            Smin,
            Sabs
        },
        numbers = {
            Sintegerp,
            Sinteger_or_marker_p,
            Sfloatp,
            Snatnump,
            Snumber_or_marker_p,
            Snumberp
        },
        symbols = {
            Ssymbolp
        },
        strings = {
            Sstringp,
            Sstring_bytes,
            Seq,
            Sbase64_encode_string,
            Sbase64_decode_string,
            Snull
        },
        character = {
            Smax_char,
            Scharacterp
        },
        floatfns = {
            Sisnan,
            Sacos,
            Sasin,
            Satan,
            Scos,
            Ssin,
            Stan,
            Slog,
            Ssqrt,
            Sexp,
            Sffloor,
            Sfceiling,
            Sftruncate,
            Sfloat
        }
    }
}
