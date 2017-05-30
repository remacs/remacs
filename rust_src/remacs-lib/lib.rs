#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(private_no_mangle_fns)]

#![cfg_attr(feature = "strict", deny(warnings))]

extern crate libc;

mod files;
mod math;

// Used for creating temporary files in emacs
pub use files::rust_make_temp;

pub use math::rust_count_trailing_zeros;
pub use math::rust_count_trailing_zeros_l;
pub use math::rust_count_trailing_zeros_ll;
pub use math::rust_count_one_bits;
pub use math::rust_count_one_bits_l;
pub use math::rust_count_one_bits_ll;
