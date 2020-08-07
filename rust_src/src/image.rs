//! Functions for image support on window system.

#[cfg(feature = "glyph-debug")]
use remacs_macros::lisp_fn;

#[cfg(feature = "glyph-debug")]
use crate::{lisp::LispObject, remacs_sys::valid_image_p};

/// Value is non-nil if SPEC is a valid image specification.
#[cfg(feature = "glyph-debug")]
#[lisp_fn]
pub fn imagep(spec: LispObject) -> bool {
    unsafe { valid_image_p(spec) }
}

include!(concat!(env!("OUT_DIR"), "/image_exports.rs"));
