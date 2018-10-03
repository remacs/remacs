//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::{frame_dimension, output_method, Fcons, Fselect_window};
use remacs_sys::{selected_frame as current_frame, Lisp_Frame, Lisp_Type};
use remacs_sys::{Qframe_live_p, Qframep, Qicon, Qnil, Qns, Qpc, Qt, Qw32, Qx};

use lisp::defsubr;
use lisp::{ExternalPtr, LispObject};
use windows::{selected_window, LispWindowRef};

pub type LispFrameRef = ExternalPtr<Lisp_Frame>;

impl LispFrameRef {
    pub fn as_lisp_obj(self) -> LispObject {
        LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
    }

    #[inline]
    pub fn is_live(self) -> bool {
        !self.terminal.is_null()
    }

    // Pixel-width of internal border lines.
    #[inline]
    pub fn internal_border_width(self) -> i32 {
        unsafe { frame_dimension(self.internal_border_width) }
    }

    #[inline]
    pub fn is_visible(self) -> bool {
        self.visible() != 0
    }
}

/// Same as the `decode_any_frame` function
pub fn frame_or_selected(object: LispObject) -> LispFrameRef {
    let frame = if object.is_nil() {
        selected_frame()
    } else {
        object
    };

    frame.as_frame_or_error()
}

/// Same as the `decode_live_frame` function
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
    } else if let Some(win) = object.as_valid_window() {
        // the window's frame does not need a live check
        win.frame().as_frame_or_error()
    } else {
        object.as_live_frame_or_error()
    }
}

/// Get the live frame either from the passed in object directly, from the object
/// as a window, or by using the selected window when object is nil.
/// When the object is a window the provided `window_action` is called.
pub fn window_frame_live_or_selected_with_action<W: FnMut(LispWindowRef) -> ()>(
    mut object: LispObject,
    mut window_action: W,
) -> LispFrameRef {
    if object.is_nil() {
        object = selected_window();
    }

    if object.is_window() {
        let w = object.as_live_window_or_error();
        window_action(w);
        object = w.frame();
    }

    object.as_live_frame_or_error()
}

/// Return the frame that is now selected.
#[lisp_fn]
pub fn selected_frame() -> LispObject {
    unsafe { current_frame }
}

/// Return non-nil if OBJECT is a frame which has not been deleted.
/// Value is nil if OBJECT is not a live frame.  If object is a live
/// frame, the return value indicates what sort of terminal device it is
/// displayed on.  See the documentation of `framep' for possible
/// return values.
#[lisp_fn]
pub fn frame_live_p(object: LispObject) -> LispObject {
    if let Some(frame) = object.as_frame() {
        if frame.is_live() {
            return framep_1(frame);
        }
    }

    Qnil
}

/// Return the selected window of FRAME-OR-WINDOW.
/// If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
/// Else if FRAME-OR-WINDOW denotes a valid window, return the selected
/// window of that window's frame.  If FRAME-OR-WINDOW denotes a live frame,
/// return the selected window of that frame.
#[lisp_fn(min = "0")]
pub fn frame_selected_window(frame_or_window: LispObject) -> LispObject {
    let frame = window_frame_live_or_selected(frame_or_window);
    frame.selected_window
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
    let mut frame_ref = frame_live_or_selected(frame);
    let w = window.as_live_window_or_error();

    if frame_ref != w.frame().as_frame().unwrap() {
        error!("In `set-frame-selected-window', WINDOW is not on FRAME")
    }
    if frame_ref == selected_frame().as_frame().unwrap() {
        unsafe { Fselect_window(window, norecord) }
    } else {
        frame_ref.selected_window = window;
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
    object.as_frame().map_or(Qnil, framep_1)
}

fn framep_1(frame: LispFrameRef) -> LispObject {
    match frame.output_method() {
        output_method::output_initial | output_method::output_termcap => Qt,
        output_method::output_x_window => Qx,
        output_method::output_w32 => Qw32,
        output_method::output_msdos_raw => Qpc,
        output_method::output_ns => Qns,
    }
}

/// The name of the window system that FRAME is displaying through.
/// The value is a symbol:
///  nil for a termcap frame (a character-only terminal),
///  `x' for an Emacs frame that is really an X window,
///  `w32' for an Emacs frame that is a window on MS-Windows display,
///  `ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
///  `pc' for a direct-write MS-DOS frame.
///
/// FRAME defaults to the currently selected frame.
///
/// Use of this function as a predicate is deprecated.  Instead,
/// use `display-graphic-p' or any of the other `display-*-p'
/// predicates which report frame's specific UI-related capabilities.
#[lisp_fn(min = "0")]
pub fn window_system(frame: Option<LispFrameRef>) -> LispObject {
    let frame = frame.unwrap_or_else(|| selected_frame().as_frame_or_error());

    let window_system = framep_1(frame);

    if window_system.is_nil() {
        wrong_type!(Qframep, frame.into());
    }

    if window_system.is_t() {
        Qnil
    } else {
        window_system
    }
}

/// Return t if FRAME is \"visible\" (actually in use for display).
/// Return the symbol `icon' if FRAME is iconified or \"minimized\".
/// Return nil if FRAME was made invisible, via `make-frame-invisible'.
/// On graphical displays, invisible frames are not updated and are
/// usually not displayed at all, even in a window system's \"taskbar\".
///
/// If FRAME is a text terminal frame, this always returns t.
/// Such frames are always considered visible, whether or not they are
/// currently being displayed on the terminal.
#[lisp_fn]
pub fn frame_visible_p(frame: LispFrameRef) -> LispObject {
    if frame.is_visible() {
        Qt
    } else if frame.iconified() {
        Qicon
    } else {
        Qnil
    }
}

/// Return top left corner of FRAME in pixels.
/// FRAME must be a live frame and defaults to the selected one.  The return
/// value is a cons (x, y) of the coordinates of the top left corner of
/// FRAME's outer frame, in pixels relative to an origin (0, 0) of FRAME's
/// display.
#[lisp_fn(min = "0")]
pub fn frame_position(frame: LispObject) -> LispObject {
    let frame_ref = frame_live_or_selected(frame);
    unsafe {
        Fcons(
            LispObject::from(frame_ref.left_pos),
            LispObject::from(frame_ref.top_pos),
        )
    }
}

/// Returns t if the mouse pointer displayed on FRAME is visible.
/// Otherwise it returns nil. FRAME omitted or nil means the selected frame.
/// This is useful when `make-pointer-invisible` is set
#[lisp_fn(min = "0")]
pub fn frame_pointer_visible_p(frame: LispObject) -> bool {
    let frame_ref = frame_or_selected(frame);
    !frame_ref.pointer_invisible()
}

/// Return the root window of FRAME-OR-WINDOW.
/// If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
/// With a frame argument, return that frame's root window.
/// With a window argument, return the root window of that window's frame.
#[lisp_fn(min = "0")]
pub fn frame_root_window(frame_or_window: LispObject) -> LispObject {
    let frame = window_frame_live_or_selected(frame_or_window);
    frame.root_window
}

/* Don't move this to window.el - this must be a safe routine.  */
/// Return the topmost, leftmost live window on FRAME-OR-WINDOW.
/// If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
/// Else if FRAME-OR-WINDOW denotes a valid window, return the first window
/// of that window's frame. If FRAME-OR-WINDOW denotes a live frame, return
/// the first window of that frame.
#[lisp_fn(min = "0")]
pub fn frame_first_window(frame_or_window: LispObject) -> LispWindowRef {
    let mut window = frame_root_window(frame_or_window).as_window_or_error();

    while let Some(win) = window.contents().as_window() {
        window = win;
    }

    window
}

include!(concat!(env!("OUT_DIR"), "/frames_exports.rs"));
