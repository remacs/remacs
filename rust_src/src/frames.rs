//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::Lisp_Frame;
use remacs_sys::selected_frame as current_frame;

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

pub type LispFrameRef = ExternalPtr<Lisp_Frame>;

/// Return the frame that is now selected.
#[lisp_fn]
pub fn selected_frame() -> LispObject {
    unsafe { LispObject::from(current_frame) }
}

include!(concat!(env!("OUT_DIR"), "/frames_exports.rs"));
