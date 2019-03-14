#![allow(clippy::cyclomatic_complexity)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]
// we need this to be able to inclde FieldOffsets in C structs
#![allow(improper_ctypes)]
// we have a bunch of unused code during testing at the moment, somehow
#![cfg_attr(test, allow(unused))]
#![cfg_attr(feature = "strict", deny(warnings))]
#![feature(concat_idents)]
#![feature(const_fn)]
#![feature(const_fn_union)]
#![feature(never_type)]
#![feature(ptr_offset_from)]
#![feature(slice_patterns)]
#![feature(specialization)]
#![feature(stmt_expr_attributes)]
#![feature(type_alias_enum_variants)]
#![feature(untagged_unions)]

extern crate errno;
#[macro_use]
extern crate lazy_static;

extern crate base64 as base64_crate;
extern crate libc;
extern crate md5;
extern crate rand;
extern crate sha1;
extern crate sha2;

extern crate field_offset;
extern crate flate2;

extern crate core;

// Wilfred/remacs#38 : Need to override the allocator for legacy unexec support on Mac.
#[cfg(all(not(test), target_os = "macos", feature = "unexecmacosx"))]
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
mod frames;
#[macro_use]
mod vector_macros;
mod str2sig;

mod alloc;
mod base64;
mod buffers;
mod bytecode;
mod callint;
mod callproc;
mod casefiddle;
mod casetab;
mod category;
mod character;
mod charset;
mod chartable;
mod cmds;
mod coding;
mod crypto;
mod data;
mod decompress;
mod dired;
#[cfg(unix)]
mod dired_unix;
#[cfg(windows)]
mod dired_windows;
mod dispnew;
mod editfns;
mod emacs;
mod eval;
mod ffi;
mod fileio;
mod floatfns;
mod fns;
mod fonts;
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
mod print;
mod process;
mod profiler;
#[allow(clippy::all)]
mod remacs_sys;
mod search;
mod strings;
mod symbols;
mod syntax;
mod terminal;
mod textprop;
mod threads;
mod time;
mod util;
mod vectors;
mod window_configuration;
mod windows;
mod xfaces;
mod xml;

#[cfg(all(not(test), target_os = "macos", feature = "unexecmacosx"))]
use alloc_unexecmacosx::OsxUnexecAlloc;

#[cfg(all(not(test), target_os = "macos", feature = "unexecmacosx"))]
#[global_allocator]
static ALLOCATOR: OsxUnexecAlloc = OsxUnexecAlloc;

#[cfg(not(test))]
include!(concat!(env!("OUT_DIR"), "/c_exports.rs"));

#[cfg(test)]
pub use crate::functions::{lispsym, make_string, make_unibyte_string, Fcons};

mod hacks {
    use core::mem::ManuallyDrop;

    #[allow(unions_with_drop_fields)]
    pub union Hack<T> {
        t: ManuallyDrop<T>,
        u: (),
    }

    impl<T> Hack<T> {
        pub const unsafe fn uninitialized() -> Self {
            Hack { u: () }
        }

        pub unsafe fn get_mut(&mut self) -> &mut T {
            &mut *self.t
        }
    }
}
