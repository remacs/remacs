#![cfg_attr(feature = "strict", deny(warnings))]

extern crate darling;
extern crate errno;
extern crate libc;
extern crate syn;

mod attributes;

// Used by remacs-macros and remacs-lib
pub use self::attributes::parse_lisp_fn;
