//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::{selected_frame as current_frame, Lisp_Frame};
use remacs_sys::Qframe_live_p;
use remacs_sys::fget_terminal;

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

pub type LispFrameRef = ExternalPtr<Lisp_Frame>;

impl LispFrameRef {
    #[inline]
    pub fn is_live(&self) -> bool {
        unsafe { !fget_terminal(self.as_ptr()).is_null() }
    }
}

pub fn frame_or_selected(object: LispObject) -> LispFrameRef {
    let frame = if object.is_nil() {
        selected_frame()
    } else {
        object
    };

    frame.as_frame_or_error()
}

pub fn frame_live_or_selected(object: LispObject) -> LispFrameRef {
    let frame = frame_or_selected(object);

    if !frame.is_live() {
        wrong_type!(Qframe_live_p, object);
    }

    frame
}

pub fn window_frame_live_or_selected(object: LispObject) -> LispFrameRef {
    if object.is_nil() {
        selected_frame().as_frame_or_error()
    } else {
        if let Some(win) = object.as_valid_window() {
            // the window's frame does not need a live check
            win.frame().as_frame_or_error()
        } else {
            object.as_live_frame_or_error()
        }
    }
}

/// Return the frame that is now selected.
#[lisp_fn]
pub fn selected_frame() -> LispObject {
    unsafe { LispObject::from(current_frame) }
}

include!(concat!(env!("OUT_DIR"), "/frames_exports.rs"));
