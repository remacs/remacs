#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]
#![feature(proc_macro)]
#![cfg_attr(feature = "strict", deny(warnings))]
#![feature(global_allocator)]

#[macro_use]
extern crate lazy_static;

extern crate base64 as base64_crate;
extern crate libc;
extern crate md5;
extern crate rand;
extern crate sha1;
extern crate sha2;

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test), target_os = "macos"))]
extern crate alloc_unexecmacosx;

// Needed for linking.
extern crate remacs_lib;
extern crate remacs_macros;
extern crate remacs_sys;

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
mod frames;
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
mod data;
mod fns;
mod dispnew;
mod indent;

#[cfg(all(not(test), target_os = "macos"))]
use alloc_unexecmacosx::OsxUnexecAlloc;

#[cfg(all(not(test), target_os = "macos"))]
#[global_allocator]
static ALLOCATOR: OsxUnexecAlloc = OsxUnexecAlloc;

pub use base64::base64_decode_1;
pub use base64::base64_encode_1;

// Used in buffer.c
pub use buffers::Fbuffer_live_p;
pub use buffers::Fbuffer_modified_p;
// used in process.c
pub use buffers::Fbuffer_name;
pub use buffers::validate_region;
// Used in nsfns.m
pub use buffers::Fbuffer_file_name;
// Used in xdisp.c
pub use buffers::Foverlay_end;
pub use buffers::Foverlay_start;

// used in category.c
pub use category::Fcategory_table_p;

// used in chartab.c
pub use chartable::Fset_char_table_parent;

// Used in fileio.c
pub use editfns::Fpoint;
// Used in bytecode.c, charset.c
pub use editfns::Fchar_after;
// Used in window.c, macros.c
pub use editfns::Fbolp;
pub use editfns::Feolp;

pub use interactive::Fprefix_numeric_value;

// Used in xdisp.c, coding.c, et. al.
pub use hashtable::Fgethash;
pub use hashtable::Fhash_table_rehash_threshold;
pub use hashtable::Fputhash;
pub use hashtable::Fremhash;

// Used in character.c
pub use multibyte::char_resolve_modifier_mask;
pub use multibyte::char_string;
pub use multibyte::count_size_as_multibyte;
pub use multibyte::multibyte_chars_in_text;
pub use multibyte::parse_str_as_multibyte;
pub use multibyte::str_as_multibyte;
pub use multibyte::str_as_unibyte;
pub use multibyte::str_to_multibyte;
pub use multibyte::str_to_unibyte;
pub use multibyte::string_char;

// Used in term.c, dired.c
pub use objects::Fidentity;

// Used in process.c
pub use process::Fget_process;
pub use str2sig::str2sig;

pub use util::clip_to_bounds;

// Used in window.c
pub use windows::Fwindow_buffer;
// Used in minibuffer.c
pub use windows::Fframe_root_window;
pub use windows::Fwindow_minibuffer_p;

// These need to be exported as bytecode.c depends upon them.
pub use editfns::Fbobp;
pub use editfns::Feobp;
pub use editfns::Ffollowing_char;
pub use math::Fadd1;
pub use math::Fleq;
pub use math::Flss;
pub use math::Fmax;
pub use math::Fmin;
pub use math::Fminus;
pub use math::Fplus;
pub use math::Fquo;
pub use math::Frem;
pub use math::Fsub1;
pub use math::Ftimes;
pub use math::arithcompare;

// Widely used in the C codebase.
pub use buffers::Fcurrent_buffer;
pub use buffers::Fget_buffer;
pub use buffers::Fset_buffer;
pub use data::Findirect_function;
pub use data::indirect_function;
pub use dispnew::Fsleep_for;
pub use editfns::Fgoto_char;
pub use floatfns::extract_float;
pub use lists::Fassoc;
pub use lists::Fassq;
pub use lists::Fcar;
pub use lists::Fcar_safe;
pub use lists::Fcdr;
pub use lists::Fcdr_safe;
pub use lists::Fdelq;
pub use lists::Fget;
pub use lists::Flist;
pub use lists::Flistp;
pub use lists::Fmake_list;
pub use lists::Fmember;
pub use lists::Fmemq;
pub use lists::Fnth;
pub use lists::Fnthcdr;
pub use lists::Fplist_get;
pub use lists::Fplist_member;
pub use lists::Fplist_put;
pub use lists::Fput;
pub use lists::Frassoc;
pub use lists::Frassq;
pub use lists::Fsetcar;
pub use lists::Fsetcdr;
pub use lists::merge;
pub use marker::Fmarker_buffer;
pub use marker::Fmarker_position;
pub use numbers::Frandom;
pub use obarray::Fintern;
pub use obarray::Fintern_soft;
pub use obarray::intern_1;
pub use objects::Fequal;
pub use objects::Fequal_including_properties;
pub use process::Fget_buffer_process;
pub use strings::Fmultibyte_string_p;
pub use strings::Fstring_as_multibyte;
pub use strings::Fstring_equal;
pub use strings::Fstring_lessp;
pub use strings::Fstring_to_multibyte;
pub use strings::Fstring_to_unibyte;
pub use symbols::Ffboundp;
pub use symbols::Findirect_variable;
pub use symbols::Fkeywordp;
pub use symbols::Fsymbol_function;
pub use symbols::Fsymbol_name;
pub use symbols::Fsymbol_plist;
pub use symbols::Fsymbol_value;
pub use symbols::Fsymbolp;
pub use symbols::indirect_variable;
pub use vectors::Felt;
pub use vectors::Flength;
pub use vectors::Fsort;
pub use windows::Fwindow_point;

#[cfg(test)]
pub use functions::make_float;

#[no_mangle]
pub extern "C" fn rust_init_syms() {
    base64::rust_init_syms();
    buffers::rust_init_syms();
    category::rust_init_syms();
    character::rust_init_syms();
    chartable::rust_init_syms();
    cmds::rust_init_syms();
    crypto::rust_init_syms();
    data::rust_init_syms();
    dispnew::rust_init_syms();
    editfns::rust_init_syms();
    floatfns::rust_init_syms();
    fns::rust_init_syms();
    fonts::rust_init_syms();
    frames::rust_init_syms();
    hashtable::rust_init_syms();
    indent::rust_init_syms();
    interactive::rust_init_syms();
    lists::rust_init_syms();
    marker::rust_init_syms();
    math::rust_init_syms();
    minibuf::rust_init_syms();
    numbers::rust_init_syms();
    obarray::rust_init_syms();
    objects::rust_init_syms();
    process::rust_init_syms();
    strings::rust_init_syms();
    symbols::rust_init_syms();
    threads::rust_init_syms();
    vectors::rust_init_syms();
    windows::rust_init_syms();
}
