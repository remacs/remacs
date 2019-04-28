//! "Face" primitives.

use remacs_macros::lisp_fn;

use crate::{
    frames::{LispFrameOrSelected, LispFrameRef},
    multibyte::LispStringRef,
    remacs_sys::{
        clear_face_cache, face_color_supported_p, set_face_change, windows_or_buffers_changed,
    },
};

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

/// Return non-nil if COLOR can be displayed on FRAME.
/// BACKGROUND-P non-nil means COLOR is used as a background.
/// Otherwise, this function tells whether it can be used as a foreground.
/// If FRAME is nil or omitted, use the selected frame.
/// COLOR must be a valid color name.
#[lisp_fn(min = "1")]
pub fn color_supported_p(
    mut color: LispStringRef,
    frame: LispFrameOrSelected,
    background_p: bool,
) -> bool {
    let mut frame: LispFrameRef = frame.into();
    unsafe { face_color_supported_p(frame.as_mut(), color.sdata_ptr(), background_p) }
}

include!(concat!(env!("OUT_DIR"), "/xfaces_exports.rs"));
