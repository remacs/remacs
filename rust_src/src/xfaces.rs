//! "Face" primitives.

use remacs_macros::lisp_fn;

use crate::remacs_sys::{clear_face_cache, set_face_change, windows_or_buffers_changed};

/// Clear face caches on all frames.
/// Optional THOROUGHLY non-nil means try to free unused fonts, too.
#[lisp_fn(min = "0", name = "clear-face-cache", c_name = "clear_face_cache")]
pub fn clear_face_cache_lisp(thoroughly: bool) {
    unsafe {
        clear_face_cache(thoroughly);
        set_face_change(true);
        windows_or_buffers_changed = 53;
    }
}

include!(concat!(env!("OUT_DIR"), "/xfaces_exports.rs"));
