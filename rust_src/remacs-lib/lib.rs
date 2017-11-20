#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]
#![cfg_attr(feature = "strict", deny(warnings))]

#[macro_use]
extern crate darling;
extern crate errno;
extern crate libc;
extern crate rand;
extern crate syn;

mod attributes;
mod docfile;
mod files;
mod math;

// Used by remacs-macros
pub use attributes::parse_lisp_fn;

// Used for creating temporary files in emacs
pub use files::rust_make_temp;

pub use math::rust_count_one_bits;
pub use math::rust_count_trailing_zero_bits;

// Used by make-docfile
pub use docfile::scan_rust_file;

#[cfg(all(not(test), target_os = "windows"))]
#[no_mangle]
pub unsafe extern "C" fn __chkstk() {}
