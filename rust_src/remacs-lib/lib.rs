#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]

#![cfg_attr(feature = "strict", deny(warnings))]

extern crate libc;

mod files;

// Used for creating temporary files in emacs
pub use files::rust_make_temp;
