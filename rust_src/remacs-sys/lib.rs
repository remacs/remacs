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

extern "C" {
    pub fn make_unibyte_string(s: *const libc::c_char, length: libc::ptrdiff_t) -> Lisp_Object;
}
