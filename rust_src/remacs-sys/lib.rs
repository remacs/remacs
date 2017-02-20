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

pub type char_bits = u32;
pub const CHAR_ALT: char_bits = 0x0400000;
pub const CHAR_SUPER: char_bits = 0x0800000;
pub const CHAR_HYPER: char_bits = 0x1000000;
pub const CHAR_SHIFT: char_bits = 0x2000000;
pub const CHAR_CTL: char_bits = 0x4000000;
pub const CHAR_META: char_bits = 0x8000000;
pub const CHAR_MODIFIER_MASK: char_bits =
    CHAR_ALT | CHAR_SUPER | CHAR_HYPER | CHAR_SHIFT | CHAR_CTL | CHAR_META;
pub const CHARACTERBITS: char_bits = 22;

extern "C" {
    pub fn make_unibyte_string(s: *const libc::c_char, length: libc::ptrdiff_t) -> Lisp_Object;
}
