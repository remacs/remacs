//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::{fget_output_method, fget_terminal, Fselect_window};
use remacs_sys::{selected_frame as current_frame, Lisp_Frame, OutputMethod};
use remacs_sys::{Qframe_live_p, Qns, Qw32, Qx};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;


pub type LispFrameRef = ExternalPtr<Lisp_Frame>;

impl LispFrameRef {
    #[inline]
    pub fn is_live(&self) -> bool {
        unsafe { !fget_terminal(self.as_ptr()).is_null() }
    }

    #[inline]
    pub fn fset_selected_window(&mut self, window: LispObject) {
        self.selected_window = window.to_raw();
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

/// Return non-nil if OBJECT is a frame which has not been deleted.
/// Value is nil if OBJECT is not a live frame.  If object is a live
/// frame, the return value indicates what sort of terminal device it is
/// displayed on.  See the documentation of `framep' for possible
/// return values.
#[lisp_fn]
fn frame_live_p(object: LispObject) -> LispObject {
    if object.as_frame().map_or(false, |f| f.is_live()) {
        LispObject::from(framep(object))
    } else {
        LispObject::constant_nil()
    }
}

/// Set selected window of FRAME to WINDOW.
/// FRAME must be a live frame and defaults to the selected one.  If FRAME
/// is the selected frame, this makes WINDOW the selected window.  Optional
/// argument NORECORD non-nil means to neither change the order of recently
/// selected windows nor the buffer list.  WINDOW must denote a live window.
/// Return WINDOW.
#[lisp_fn(min = "2")]
pub fn set_frame_selected_window(
    frame: LispObject,
    window: LispObject,
    norecord: LispObject,
) -> LispObject {
    let f = if frame.is_nil() {
        selected_frame()
    } else {
        frame
    };
    let mut frame_ref = f.as_live_frame_or_error();
    let w = window.as_live_window_or_error();

    if !f.eq(w.frame()) {
        error!("In `set-frame-selected-window', WINDOW is not on FRAME")
    }
    if f.eq(selected_frame()) {
        unsafe { LispObject::from(Fselect_window(window.to_raw(), norecord.to_raw())) }
    } else {
        frame_ref.fset_selected_window(window);
        window
    }
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
