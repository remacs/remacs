#![feature(const_size_of)]
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]
// due to a bug in bindgen related to how it processes unions that
// contain bitfields, we need to use the new untagged_unions feature,
// which is unstable. bindgen also generates const functions when in
// this mode, so that's turned on as well.
#![feature(const_fn)]
#![feature(untagged_unions)]
#![feature(const_max_value)]

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
//! - `BoolBF`

extern crate libc;

pub mod libm;

include!(concat!(env!("OUT_DIR"), "/definitions.rs"));
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub const WAIT_READING_MAX: i64 = i64::max_value();

pub type PseudovecType = pvec_type;

pub const PSEUDOVECTOR_SIZE_BITS: More_Lisp_Bits = More_Lisp_Bits_PSEUDOVECTOR_SIZE_BITS;
pub const PSEUDOVECTOR_SIZE_MASK: More_Lisp_Bits = More_Lisp_Bits_PSEUDOVECTOR_SIZE_MASK;
pub const PSEUDOVECTOR_REST_BITS: More_Lisp_Bits = More_Lisp_Bits_PSEUDOVECTOR_REST_BITS;
pub const PSEUDOVECTOR_REST_MASK: More_Lisp_Bits = More_Lisp_Bits_PSEUDOVECTOR_REST_MASK;
pub const PSEUDOVECTOR_AREA_BITS: More_Lisp_Bits = More_Lisp_Bits_PSEUDOVECTOR_AREA_BITS;
pub const PVEC_TYPE_MASK: More_Lisp_Bits = More_Lisp_Bits_PVEC_TYPE_MASK;

pub type Lisp_Vectorlike_Header = vectorlike_header;

pub const VALBITS: Lisp_Bits = Lisp_Bits_VALBITS;
pub const INTTYPEBITS: Lisp_Bits = Lisp_Bits_INTTYPEBITS;
pub const FIXNUM_BITS: Lisp_Bits = Lisp_Bits_FIXNUM_BITS;
pub const VAL_MAX: EmacsInt = (EMACS_INT_MAX >> (GCTYPEBITS - 1));
pub const VALMASK: EmacsInt = [VAL_MAX, -(1 << GCTYPEBITS)][USE_LSB_TAG as usize];
pub const INTMASK: EmacsInt = (EMACS_INT_MAX >> (INTTYPEBITS - 1));

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

/// Type of comparison for `internal_equal()`.
#[repr(C)]
pub enum EqualKind {
    NoQuit,
    Plain,
    IncludingProperties,
}

// bindgen apparently misses these, for various reasons
extern "C" {
    // these weren't declared in a header, for example
    pub static Vprocess_alist: Lisp_Object;
    pub fn hash_clear(h: *mut Lisp_Hash_Table);
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: libc::c_int,
        ht: Lisp_Object,
    ) -> bool;

    // these are inline function, so in principle we shouldn't try to
    // call it (it may not actually exist, if the compiler inlined all
    // the call sites), but it hasn't caused us any problems so
    // far. See puresize.rs
    // TODO: make the C side of Remacs build with --keep-inline
    pub fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const libc::c_void);
    pub fn make_lisp_ptr(ptr: *mut libc::c_void, ty: Lisp_Type) -> Lisp_Object;
    pub fn make_lisp_symbol(sym: *mut Lisp_Symbol) -> Lisp_Object;
    pub fn gc_aset(array: *mut Lisp_Object, idx: libc::ptrdiff_t, val: Lisp_Object);
    pub fn current_timespec() -> timespec;
}

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> Lisp_Bits_INTTYPEBITS as u32;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

// In order to use `lazy_static!` with LispSubr, it must be Sync. Raw
// pointers are not Sync, but it isn't a problem to define Sync if we
// never mutate LispSubr values. If we do, we will need to create
// these objects at runtime, perhaps using forget().
//
// Based on http://stackoverflow.com/a/28116557/509706
unsafe impl Sync for Lisp_Subr {}
