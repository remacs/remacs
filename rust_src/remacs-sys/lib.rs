#![allow(non_camel_case_types)]

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

include!(concat!(env!("OUT_DIR"), "/definitions.rs"));

pub type Lisp_Object = EmacsInt;

pub const PSEUDOVECTOR_SIZE_BITS: libc::c_int = 12;
pub const PSEUDOVECTOR_SIZE_MASK: libc::c_int = (1 << PSEUDOVECTOR_SIZE_BITS) - 1;
pub const PSEUDOVECTOR_REST_BITS: libc::c_int = 12;
pub const PSEUDOVECTOR_REST_MASK: libc::c_int = (((1 << PSEUDOVECTOR_REST_BITS) - 1) <<
                                                 PSEUDOVECTOR_SIZE_BITS);
pub const PSEUDOVECTOR_AREA_BITS: libc::c_int = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS;
pub const PVEC_TYPE_MASK: libc::c_int = 0x3f << PSEUDOVECTOR_AREA_BITS;

pub type pvec_type = libc::c_int;
pub const PVEC_NORMAL_VECTOR: pvec_type = 0;
pub const PVEC_FREE: pvec_type = 1;
pub const PVEC_PROCESS: pvec_type = 2;
pub const PVEC_FRAME: pvec_type = 3;
pub const PVEC_WINDOW: pvec_type = 4;
pub const PVEC_BOOL_VECTOR: pvec_type = 5;
pub const PVEC_BUFFER: pvec_type = 6;
pub const PVEC_HASH_TABLE: pvec_type = 7;
pub const PVEC_TERMINAL: pvec_type = 8;
pub const PVEC_WINDOW_CONFIGURATION: pvec_type = 9;
pub const PVEC_SUBR: pvec_type = 10;
pub const PVEC_OTHER: pvec_type = 11;
pub const PVEC_XWIDGET: pvec_type = 12;
pub const PVEC_XWIDGET_VIEW: pvec_type = 13;
pub const PVEC_COMPILED: pvec_type = 14;
pub const PVEC_CHAR_TABLE: pvec_type = 15;
pub const PVEC_SUB_CHAR_TABLE: pvec_type = 16;
pub const PVEC_FONT: pvec_type = 17;

#[derive(Debug)]
#[repr(C)]
pub struct vectorlike_header {
    pub size: libc::ptrdiff_t,
}

/// Representation of an Emacs Lisp function symbol.
#[derive(Debug)]
#[repr(C)]
pub struct Lisp_Subr {
    pub header: vectorlike_header,

    /// This is the function pointer that will be called when the user invokes
    /// the Emacs Lisp function. Also, this field is actually an union in C.
    pub function: *const libc::c_void,

    /// The minimum number of arguments that can be passed to the Emacs Lisp
    /// function.
    pub min_args: libc::c_short,

    /// The maximum number of arguments that can be passed to te Emacs Lisp
    /// function.
    pub max_args: libc::c_short,

    /// The name of the function in Emacs Lisp.
    pub symbol_name: *const libc::c_char,

    /// The interactive specification. This may be a normal prompt
    /// string, such as `"bBuffer: "` or an elisp form as a string.
    /// If the function is not interactive, this should be a null
    /// pointer.
    pub intspec: *const libc::c_char,

    // TODO: Change this to EMACS_INT
    //
    // If you wan't to give it a try and solve this you should see this commit:
    // https://github.com/Wilfred/remacs/commit/c5461d03a411ff5c6f43885a0a9030e8a94bbc2e
    /// The docstring of the Emacs Lisp function.
    pub doc: *const libc::c_char,
}

// In order to use `lazy_static!` with LispSubr, it must be Sync. Raw
// pointers are not Sync, but it isn't a problem to define Sync if we
// never mutate LispSubr values. If we do, we will need to create
// these objects at runtime, perhaps using forget().
//
// Based on http://stackoverflow.com/a/28116557/509706
unsafe impl Sync for Lisp_Subr {}

extern "C" {
    pub fn make_unibyte_string(s: *const libc::c_char, length: libc::ptrdiff_t) -> Lisp_Object;
}
