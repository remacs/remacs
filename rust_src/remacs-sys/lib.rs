// due to a bug in bindgen related to how it processes unions that
// contain bitfields, we need to use the new untagged_unions feature,
// which is unstable. bindgen also generates const functions when in
// this mode, so that's turned on as well.
#![feature(const_fn)]
#![feature(untagged_unions)]
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

    // this is an inline function, so in principle we shouldn't try to
    // call it (it may not actually exist, if the compiler inlined all
    // the call sites), but it hasn't caused us any problems so
    // far. See puresize.rs
    pub fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const libc::c_void);
    // same here
    pub fn make_lisp_ptr(ptr: *mut libc::c_void, ty: Lisp_Type) -> Lisp_Object;
}

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> Lisp_Bits::INTTYPEBITS as u32;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

/// Contains C definitions from the font.h header.
pub mod fontmod {
    use libc::c_int;

    /// Represents the indices of font properties in the contents of a font
    /// vector.
    ///
    /// # C Porting Notes
    ///
    /// The equivalent C enum is `font_property_index`. Since it is meant to
    /// represent indices for three different length vectors, the C definition
    /// contains duplicate variants, e.g `FONT_OBJLIST_INDEX = FONT_SPEC_MAX`,
    /// to represent sizes. These have been moved out of this enum and are
    /// available as constant `c_int` values on this module.
    #[allow(non_camel_case_types, dead_code)]
    #[repr(C)]
    pub enum FontPropertyIndex {
        FONT_TYPE_INDEX,
        FONT_FOUNDRY_INDEX,
        FONT_FAMILY_INDEX,
        FONT_ADSTYLE_INDEX,
        FONT_REGISTRY_INDEX,
        FONT_WEIGHT_INDEX,
        FONT_SLANT_INDEX,
        FONT_WIDTH_INDEX,
        FONT_SIZE_INDEX,
        FONT_DPI_INDEX,
        FONT_SPACING_INDEX,
        FONT_AVGWIDTH_INDEX,
        FONT_EXTRA_INDEX,
        // In C, we have FONT_SPEC_MAX, FONT_OBJLIST_INDEX = FONT_SPEC_MAX here.
        FONT_OBJLIST_INDEX,
        // In C, we have FONT_ENTITY_MAX, FONT_NAME_INDEX = FONT_ENTITY_MAX here.
        FONT_NAME_INDEX,
        FONT_FULLNAME_INDEX,
        FONT_FILE_INDEX,
        // In C, we have FONT_OBJECT_MAX here.
    }

    pub const FONT_SPEC_MAX: c_int = FontPropertyIndex::FONT_OBJLIST_INDEX as c_int;
    pub const FONT_ENTITY_MAX: c_int = FontPropertyIndex::FONT_NAME_INDEX as c_int;
    pub const FONT_OBJECT_MAX: c_int = (FontPropertyIndex::FONT_FILE_INDEX as c_int) + 1;
}
