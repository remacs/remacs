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

/// Return non-nil if OBJECT is a frame.
/// Value is:
///   t for a termcap frame (a character-only terminal),
///  `x' for an Emacs frame that is really an X window,
///  `w32' for an Emacs frame that is a window on MS-Windows display,
///  `ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
/// See also `frame-live-p'.
#[lisp_fn]
pub fn framep(object: LispObject) -> LispObject {
    if let Some(frame) = object.as_frame() {
        match unsafe { fget_output_method(frame.as_ptr()) } {
            OutputMethod::output_initial => LispObject::constant_t(),
            OutputMethod::output_termcap => LispObject::constant_t(),
            OutputMethod::output_x_window => LispObject::from(Qx),
            OutputMethod::output_w32 => LispObject::from(Qw32),
            OutputMethod::output_ns => LispObject::from(Qns),
        }
    } else {
        LispObject::constant_nil()
    }
}

include!(concat!(env!("OUT_DIR"), "/frames_exports.rs"));
