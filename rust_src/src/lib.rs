#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]
#![feature(proc_macro)]
#![cfg_attr(feature = "strict", deny(warnings))]
#![feature(global_allocator)]

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test), target_os = "macos"))]
extern crate alloc_unexecmacosx;

#[macro_use]
extern crate lazy_static;

extern crate remacs_sys;

// Needed for linking.
#[allow(unused_extern_crates)]
extern crate remacs_lib;

extern crate remacs_macros;
extern crate libc;
extern crate md5;
extern crate rand;
extern crate sha1;
extern crate sha2;
extern crate base64 as base64_crate;

#[cfg(test)]
extern crate mock_derive;

#[cfg(test)]
mod functions;

#[macro_use]
mod eval;
mod lisp;
mod lists;
mod marker;
mod floatfns;
mod math;
mod numbers;
mod objects;
mod strings;
mod symbols;
#[macro_use]
mod vectors;
mod character;
mod base64;
mod crypto;
mod str2sig;
mod multibyte;
mod buffers;
mod windows;
mod hashtable;
mod interactive;
mod process;
mod fonts;
mod threads;
mod chartable;
mod category;
mod obarray;
mod editfns;
mod util;
mod minibuf;
mod cmds;

#[cfg(all(not(test), target_os = "macos"))]
use alloc_unexecmacosx::OsxUnexecAlloc;

#[cfg(all(not(test), target_os = "macos"))]
#[global_allocator]
static ALLOCATOR: OsxUnexecAlloc = OsxUnexecAlloc;

use remacs_sys::Lisp_Subr;

pub use base64::base64_encode_1;
pub use base64::base64_decode_1;

pub use util::clip_to_bounds;

// Used in buffer.c
pub use buffers::Fbuffer_live_p;
pub use buffers::Fbuffer_modified_p;

// Used in window.c
pub use windows::Fwindow_buffer;

// used in process.c
pub use buffers::Fbuffer_name;

pub use buffers::validate_region;

// Used in nsfns.m
pub use buffers::Fbuffer_file_name;

// These need to be exported as bytecode.c depends upon them.
pub use math::Fplus;
pub use math::Fminus;
pub use math::Ftimes;
pub use math::Fmax;
pub use math::Fmin;
pub use math::Fquo;
pub use math::Flss;
pub use math::Fleq;
pub use math::Frem;
pub use math::Fadd1;
pub use math::Fsub1;
pub use math::arithcompare;
pub use editfns::Feobp;
pub use editfns::Fbobp;

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
pub use lists::Fget;
pub use lists::Fput;
pub use lists::Flist;
pub use lists::Fmake_list;
pub use floatfns::extract_float;
pub use numbers::Frandom;
pub use objects::Fequal;
pub use objects::Fequal_including_properties;
pub use symbols::Fsymbolp;
pub use symbols::Fsymbol_name;
pub use symbols::Ffboundp;
pub use symbols::Fsymbol_function;
pub use symbols::Fsymbol_plist;
pub use strings::Fstring_equal;
pub use strings::Fstring_as_multibyte;
pub use strings::Fstring_to_multibyte;
pub use strings::Fstring_to_unibyte;
pub use strings::Fmultibyte_string_p;
pub use strings::Fstring_lessp;
pub use vectors::Flength;
pub use vectors::Felt;
pub use vectors::Fsort;
pub use lists::merge;
pub use buffers::Fget_buffer;
pub use buffers::Fcurrent_buffer;
pub use buffers::Fset_buffer;
pub use obarray::intern_1;
pub use obarray::Fintern;
pub use obarray::Fintern_soft;
pub use marker::Fmarker_position;
pub use marker::Fmarker_buffer;
pub use windows::Fwindow_point;

// Used in fileio.c
pub use editfns::Fpoint;

// used in chartab.c
pub use chartable::Fset_char_table_parent;

// used in category.c
pub use category::Fcategory_table_p;

// Used in process.c
pub use str2sig::str2sig;
pub use process::Fget_process;

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

// Used in xdisp.c
pub use buffers::Foverlay_start;
pub use buffers::Foverlay_end;

// Used in window.c, macros.c
pub use interactive::Fprefix_numeric_value;
pub use editfns::Fbolp;
pub use editfns::Feolp;

extern "C" {
    fn defsubr(sname: *const Lisp_Subr);
}

#[no_mangle]
pub extern "C" fn rust_init_syms() {
    unsafe {
        defsubr(&*buffers::Soverlayp);
        defsubr(&*buffers::Sbuffer_live_p);
        defsubr(&*buffers::Sget_buffer);
        defsubr(&*buffers::Scurrent_buffer);
        defsubr(&*buffers::Sbuffer_file_name);
        defsubr(&*buffers::Sbuffer_modified_p);
        defsubr(&*buffers::Sbuffer_modified_tick);
        defsubr(&*buffers::Sbuffer_chars_modified_tick);
        defsubr(&*buffers::Sbuffer_name);
        defsubr(&*buffers::Sset_buffer);
        defsubr(&*buffers::Soverlay_start);
        defsubr(&*buffers::Soverlay_end);
        defsubr(&*buffers::Soverlay_buffer);
        defsubr(&*windows::Swindowp);
        defsubr(&*windows::Swindow_live_p);
        defsubr(&*windows::Swindow_point);
        defsubr(&*windows::Sselected_window);
        defsubr(&*windows::Swindow_buffer);
        defsubr(&*windows::Swindow_valid_p);
        defsubr(&*windows::Swindow_start);
        defsubr(&*process::Sget_process);
        defsubr(&*process::Sprocessp);
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
        defsubr(&*math::Seqlsign);
        defsubr(&*math::Slss);
        defsubr(&*math::Sgtr);
        defsubr(&*math::Sleq);
        defsubr(&*math::Sgeq);
        defsubr(&*math::Sneq);
        defsubr(&*math::Srem);
        defsubr(&*math::Sadd1);
        defsubr(&*math::Ssub1);
        defsubr(&*math::Slognot);
        defsubr(&*numbers::Sintegerp);
        defsubr(&*numbers::Sinteger_or_marker_p);
        defsubr(&*numbers::Sfloatp);
        defsubr(&*numbers::Snatnump);
        defsubr(&*numbers::Snumber_or_marker_p);
        defsubr(&*numbers::Snumberp);
        defsubr(&*numbers::Srandom);
        defsubr(&*objects::Snull);
        defsubr(&*objects::Seq);
        defsubr(&*objects::Seql);
        defsubr(&*objects::Sequal);
        defsubr(&*objects::Sequal_including_properties);
        defsubr(&*symbols::Ssymbolp);
        defsubr(&*symbols::Ssymbol_name);
        defsubr(&*symbols::Sfboundp);
        defsubr(&*symbols::Ssymbol_function);
        defsubr(&*symbols::Ssymbol_plist);
        defsubr(&*symbols::Ssetplist);
        defsubr(&*symbols::Sfmakunbound);
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
        defsubr(&*lists::Sget);
        defsubr(&*lists::Sput);
        defsubr(&*lists::Slist);
        defsubr(&*lists::Smake_list);
        defsubr(&*lists::Ssafe_length);
        defsubr(&*marker::Smarkerp);
        defsubr(&*marker::Smarker_position);
        defsubr(&*marker::Smarker_buffer);
        defsubr(&*strings::Sstringp);
        defsubr(&*strings::Smultibyte_string_p);
        defsubr(&*base64::Sbase64_encode_string);
        defsubr(&*base64::Sbase64_decode_string);
        defsubr(&*strings::Sstring_bytes);
        defsubr(&*strings::Sstring_equal);
        defsubr(&*strings::Sstring_as_multibyte);
        defsubr(&*strings::Sstring_to_multibyte);
        defsubr(&*strings::Sstring_to_unibyte);
        defsubr(&*strings::Sstring_lessp);
        defsubr(&*character::Smax_char);
        defsubr(&*character::Scharacterp);
        defsubr(&*character::Schar_or_string_p);
        defsubr(&*character::Sunibyte_char_to_multibyte);
        defsubr(&*character::Smultibyte_char_to_unibyte);
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
        defsubr(&*vectors::Selt);
        defsubr(&*vectors::Srecordp);
        defsubr(&*hashtable::Scopy_hash_table);
        defsubr(&*fonts::Sfontp);
        defsubr(&*crypto::Smd5);
        defsubr(&*crypto::Ssecure_hash);
        defsubr(&*crypto::Sbuffer_hash);
        defsubr(&*interactive::Sprefix_numeric_value);
        defsubr(&*chartable::Schar_table_subtype);
        defsubr(&*chartable::Schar_table_parent);
        defsubr(&*chartable::Sset_char_table_parent);
        defsubr(&*category::Scategory_table_p);
        defsubr(&*category::Scategory_table);
        defsubr(&*obarray::Sintern_soft);
        defsubr(&*obarray::Sintern);

        defsubr(&*floatfns::Sisnan);
        defsubr(&*floatfns::Sacos);
        defsubr(&*floatfns::Sasin);
        defsubr(&*floatfns::Satan);
        defsubr(&*floatfns::Scos);
        defsubr(&*floatfns::Ssin);
        defsubr(&*floatfns::Stan);
        defsubr(&*floatfns::Slog);
        defsubr(&*floatfns::Ssqrt);
        defsubr(&*floatfns::Sexp);
        defsubr(&*floatfns::Sffloor);
        defsubr(&*floatfns::Sfceiling);
        defsubr(&*floatfns::Sftruncate);
        defsubr(&*floatfns::Sfloat);
        defsubr(&*floatfns::Scopysign);
        defsubr(&*floatfns::Sfrexp);
        defsubr(&*floatfns::Sldexp);
        defsubr(&*floatfns::Sexpt);
        defsubr(&*floatfns::Slogb);
        defsubr(&*floatfns::Sfround);
        defsubr(&*floatfns::Sceiling);
        defsubr(&*floatfns::Sfloor);
        defsubr(&*floatfns::Sround);
        defsubr(&*floatfns::Struncate);
        defsubr(&*editfns::Spoint);
        defsubr(&*editfns::Sbuffer_size);
        defsubr(&*editfns::Seobp);
        defsubr(&*editfns::Sbobp);
        defsubr(&*editfns::Sbolp);
        defsubr(&*editfns::Seolp);
        defsubr(&*editfns::Sregion_beginning);
        defsubr(&*editfns::Sregion_end);
        defsubr(&*editfns::Smark_marker);
        defsubr(&*editfns::Spoint_min);
        defsubr(&*editfns::Spoint_max);
        defsubr(&*minibuf::Sminibufferp);
        defsubr(&*minibuf::Sactive_minibuffer_window);
        defsubr(&*cmds::Sforward_point)
    }
}
