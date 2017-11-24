#![cfg_attr(feature = "strict", deny(warnings))]

#[macro_use]
extern crate darling;
extern crate errno;
extern crate libc;
extern crate rand;
extern crate syn;

mod attributes;

// Used by remacs-macros and remacs-lib
pub use attributes::parse_lisp_fn;
