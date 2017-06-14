#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]
#![feature(proc_macro)]
#![cfg_attr(feature = "strict", deny(warnings))]

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test) ), target_os = "macos"))]
extern crate alloc_unexecmacosx;

#[macro_use]
extern crate lazy_static;

extern crate remacs_sys;
extern crate remacs_lib;
extern crate remacs_macros;
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
mod vectors;
mod character;
mod base64;
mod crypto;
mod str2sig;
mod multibyte;
mod buffers;

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
pub use lists::Flist;
pub use lists::Fmake_list;
pub use floatfns::extract_float;
pub use floatfns::fmod_float;
pub use objects::Fequal;
pub use objects::Fequal_including_properties;
pub use symbols::Fsymbolp;
pub use strings::Fstring_equal;
pub use strings::Fstring_as_multibyte;
pub use strings::Fstring_to_multibyte;
pub use vectors::Flength;
pub use vectors::Fsort;
pub use lists::merge;

// Cryptographic functions used in the C codebase.
pub use crypto::sha1_buffer;
pub use crypto::sha224_buffer;
pub use crypto::sha256_buffer;
pub use crypto::sha384_buffer;
pub use crypto::sha512_buffer;

// Used in process.c
pub use str2sig::str2sig;

// Used in character.c
pub use multibyte::char_resolve_modifier_mask;
pub use multibyte::char_string;
pub use multibyte::string_char;
pub use multibyte::count_size_as_multibyte;
pub use multibyte::multibyte_chars_in_text;
pub use multibyte::parse_str_as_multibyte;
pub use multibyte::str_as_multibyte;
pub use multibyte::str_to_multibyte;
pub use multibyte::str_as_unibyte;
pub use multibyte::str_to_unibyte;

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
        defsubr(&*lists::Slist);
        defsubr(&*lists::Smake_list);
        defsubr(&*lists::Ssafe_length);
        defsubr(&*marker::Smarkerp);
        defsubr(&*strings::Sstringp);
        defsubr(&*strings::Sbase64_encode_string);
        defsubr(&*strings::Sbase64_decode_string);
        defsubr(&*strings::Sstring_bytes);
        defsubr(&*strings::Sstring_equal);
        defsubr(&*strings::Sstring_as_multibyte);
        defsubr(&*strings::Sstring_to_multibyte);
        defsubr(&*character::Smax_char);
        defsubr(&*character::Scharacterp);
        defsubr(&*character::Scharacter_or_string_p);
        defsubr(&*vectors::Sarrayp);
        defsubr(&*vectors::Sbool_vector_p);
        defsubr(&*vectors::Sbufferp);
        defsubr(&*vectors::Sbyte_code_function_p);
        defsubr(&*vectors::Schar_table_p);
        defsubr(&*vectors::Scondition_variable_p);
        defsubr(&*vectors::Smutexp);
        defsubr(&*vectors::Ssequencep);
        defsubr(&*vectors::Ssort);
        defsubr(&*vectors::Ssubrp);
        defsubr(&*vectors::Sthreadp);
        defsubr(&*vectors::Svector_or_char_table_p);
        defsubr(&*vectors::Svectorp);
        defsubr(&*vectors::Slength);
        defsubr(&*crypto::Sbuffer_hash);

        floatfns::init_float_syms();
    }
}
