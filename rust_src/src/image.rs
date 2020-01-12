//! Functions for image support on window system.

use crate::lisp::LispObject;
use remacs_macros::lisp_fn;
use crate::remacs_sys::lookup_image_type;

#[cfg(feature = "glyph-debug")]
use crate::remacs_sys::valid_image_p;

/// Value is non-nil if SPEC is a valid image specification.
#[cfg(feature = "glyph-debug")]
#[lisp_fn]
pub fn imagep(spec: LispObject) -> bool {
    unsafe { valid_image_p(spec) }
}

/// Initialize image library implementing image type TYPE.
/// Return non-nil if TYPE is a supported image type.
///
/// If image libraries are loaded dynamically (currently only the case on
/// MS-Windows), load the library for TYPE if it is not yet loaded, using
/// the library file(s) specified by `dynamic-library-alist'.
#[lisp_fn]
pub fn init_image_library(r#type: LispObject) -> bool {
    unsafe { !lookup_image_type(r#type).is_null() }
}

include!(concat!(env!("OUT_DIR"), "/image_exports.rs"));
