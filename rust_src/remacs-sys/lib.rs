#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

//! This module contains all FFI declarations.
//!
//! These types and constants are generated at build time to mimic how they are
//! in C:
//!
//! - `EmacsInt`
//! - `EmacsUint`
//! - `EmacsDouble`
//! - `EMACS_INT_MAX`
//! - `EMACS_INT_SIZE`
//! - `EMACS_FLOAT_SIZE`
//! - `GCTYPEBITS`
//! - `USE_LSB_TAG`

extern crate libc;

pub mod libm;

include!(concat!(env!("OUT_DIR"), "/definitions.rs"));
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

// In order to use `lazy_static!` with LispSubr, it must be Sync. Raw
// pointers are not Sync, but it isn't a problem to define Sync if we
// never mutate LispSubr values. If we do, we will need to create
// these objects at runtime, perhaps using forget().
//
// Based on http://stackoverflow.com/a/28116557/509706
unsafe impl Sync for Lisp_Subr {}

// These signal an error, therefore are marked as non-returning.
extern "C" {
    pub fn circular_list(tail: Lisp_Object) -> !;
    pub fn wrong_type_argument(predicate: Lisp_Object, value: Lisp_Object) -> !;
    // defined in eval.c, where it can actually take an arbitrary
    // number of arguments.
    // TODO: define a Rust version of this that uses Rust strings.
    pub fn error(m: *const u8, ...) -> !;
    pub fn nsberror(spec: Lisp_Object) -> !;
    pub fn emacs_abort() -> !;
    pub fn Fsignal(error_symbol: Lisp_Object, data: Lisp_Object) -> !;
}

#[repr(C)]
#[derive(Debug)]
pub struct Lisp_Vectorlike {
    pub header: vectorlike_header,
    // shouldn't look at the contents without knowing the structure...
}

#[repr(C)]
pub enum EqualKind {
    NoQuit,
    Plain,
    IncludingProperties,
}

// bindgen apparently misses these, for various reasons
pub const INTMASK: EmacsInt = (EMACS_INT_MAX >> (Lisp_Bits::INTTYPEBITS as u32 - 1));
extern "C" {
    // this one wasn't declared in a header, for example
    pub static Vprocess_alist: Lisp_Object;
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: libc::c_int,
        ht: Lisp_Object,
    ) -> bool;

    pub fn SYMBOL_NAME(sym: Lisp_Object) -> Lisp_Object;
    pub fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const std::os::raw::c_void);

    pub fn build_string(s: *const std::os::raw::c_char) -> Lisp_Object;
}

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> Lisp_Bits::INTTYPEBITS as u32;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);
