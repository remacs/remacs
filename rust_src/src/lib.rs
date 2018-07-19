#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]
#![cfg_attr(feature = "clippy", allow(not_unsafe_ptr_arg_deref, wrong_self_convention))]
#![feature(const_fn)]
#![feature(const_size_of)]
#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]
#![allow(improper_ctypes)] // we need this to be able to inclde FieldOffsets in C structs
// we have a bunch of unused code during testing at the moment, somehow
#![cfg_attr(test, allow(unused))]
#![feature(proc_macro)]
#![cfg_attr(feature = "strict", deny(warnings))]
#![feature(global_allocator)]
#![feature(concat_idents)]
#![feature(stmt_expr_attributes)]
#![feature(repr_transparent)]
#![feature(untagged_unions)]
#![feature(never_type)]

#[macro_use]
extern crate lazy_static;

extern crate base64 as base64_crate;
extern crate libc;
extern crate md5;
extern crate rand;
extern crate sha1;
extern crate sha2;

extern crate field_offset;

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test), target_os = "macos"))]
extern crate alloc_unexecmacosx;

// Needed for linking.
extern crate remacs_lib;
extern crate remacs_macros;

#[cfg(test)]
#[macro_use]
mod functions;

#[macro_use]
mod eval_macros;
#[macro_use]
mod lisp;
#[macro_use]
mod vector_macros;
mod str2sig;

mod base64;
mod buffers;
mod casefiddle;
mod casetab;
mod category;
mod character;
mod chartable;
mod cmds;
mod crypto;
mod data;
mod dired;
#[cfg(unix)]
mod dired_unix;
#[cfg(windows)]
mod dired_windows;
mod dispnew;
mod editfns;
mod eval;
mod ffi;
mod fileio;
mod floatfns;
mod fns;
mod fonts;
mod frames;
mod hashtable;
mod indent;
mod interactive;
mod keyboard;
mod keymap;
mod libm;
mod lists;
mod lread;
mod marker;
mod math;
mod minibuf;
mod multibyte;
mod numbers;
mod obarray;
mod objects;
mod process;
mod remacs_sys;
mod strings;
mod symbols;
mod syntax;
mod textprop;
mod threads;
mod time;
mod util;
mod vectors;
mod windows;

#[cfg(all(not(test), target_os = "macos"))]
use alloc_unexecmacosx::OsxUnexecAlloc;

#[cfg(all(not(test), target_os = "macos"))]
#[global_allocator]
static ALLOCATOR: OsxUnexecAlloc = OsxUnexecAlloc;

#[cfg(not(test))]
include!(concat!(env!("OUT_DIR"), "/c_exports.rs"));

#[cfg(test)]
pub use functions::{lispsym, make_string, make_unibyte_string, Fcons, Fsignal};

#[cfg(feature = "compile-errors")]
mod compile_errors {
    use lisp::LispObject;
    use remacs_macros::lisp_fn;

    #[lisp_fn]
    fn dummy(x: LispObject) -> LispObject {
        compile_error!("error 001");
    }
}

mod hacks {
    #[allow(unions_with_drop_fields)]
    union Hack<T> {
        t: T,
        u: (),
    }
    #[allow(const_err)]
    pub const unsafe fn uninitialized<T>() -> T {
        Hack { u: () }.t
    }
}
