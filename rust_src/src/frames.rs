//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::{selected_frame as current_frame, BoolBF, EmacsInt, Lisp_Frame};
use remacs_sys::{fget_column_width, fget_iconified, fget_internal_border_width, fget_left_pos,
                 fget_line_height, fget_minibuffer_window, fget_output_method, fget_root_window,
                 fget_terminal, fget_top_pos, fget_visible, frame_dimension, Fcons, Fselect_window};
use remacs_sys::{Qframe_live_p, Qicon, Qns, Qpc, Qt, Qw32, Qx};

use libc::c_int;
use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use std::mem;
use windows::{selected_window, LispWindowRef};

pub type OutputMethod = c_int;
pub const output_initial: OutputMethod = 0;
pub const output_termcap: OutputMethod = 1;
pub const output_x_window: OutputMethod = 2;
pub const output_w32: OutputMethod = 3;
pub const output_msdos_raw: OutputMethod = 4;
pub const output_ns: OutputMethod = 5;

pub type LispFrameRef = ExternalPtr<Lisp_Frame>;

impl LispFrameRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(self.as_ptr()) }
    }

    #[inline]
    pub fn is_live(self) -> bool {
        unsafe { !fget_terminal(self.as_ptr()).is_null() }
    }

    #[inline]
    pub fn column_width(self) -> i32 {
        unsafe { fget_column_width(self.as_ptr()) }
    }

    // Pixel-width of internal border lines.
    #[inline]
    pub fn internal_border_width(self) -> i32 {
        unsafe { frame_dimension(fget_internal_border_width(self.as_ptr())) }
    }

    #[inline]
    pub fn line_height(self) -> i32 {
        unsafe { fget_line_height(self.as_ptr()) }
    }

    #[inline]
    pub fn top_pos(self) -> i32 {
        unsafe { fget_top_pos(self.as_ptr()) }
    }

    #[inline]
    pub fn left_pos(self) -> i32 {
        unsafe { fget_left_pos(self.as_ptr()) }
    }

    #[inline]
    pub fn minibuffer_window(self) -> LispObject {
        LispObject::from_raw(unsafe { fget_minibuffer_window(self.as_ptr()) })
    }

    #[inline]
    pub fn root_window(self) -> LispObject {
        LispObject::from_raw(unsafe { fget_root_window(self.as_ptr()) })
    }

    #[inline]
    pub fn set_selected_window(&mut self, window: LispObject) {
        self.selected_window = window.to_raw();
    }

    #[inline]
    pub fn is_visible(self) -> bool {
        unsafe { fget_visible(self.as_ptr()) }
    }

    #[inline]
    pub fn is_iconified(self) -> bool {
        unsafe { LispObject::from_bool(fget_iconified(self.as_ptr()) as BoolBF).is_not_nil() }
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
    unsafe { LispObject::from_raw(current_frame) }
}

/// Return non-nil if OBJECT is a frame which has not been deleted.
/// Value is nil if OBJECT is not a live frame.  If object is a live
/// frame, the return value indicates what sort of terminal device it is
/// displayed on.  See the documentation of `framep' for possible
/// return values.
#[lisp_fn]
pub fn frame_live_p(object: LispObject) -> LispObject {
    if object.as_frame().map_or(false, |f| f.is_live()) {
        framep(object)
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
    let mut frame_ref = frame_live_or_selected(frame);
    let w = window.as_live_window_or_error();

    if frame_ref != w.frame().as_frame().unwrap() {
        error!("In `set-frame-selected-window', WINDOW is not on FRAME")
    }
    if frame_ref == selected_frame().as_frame().unwrap() {
        unsafe { LispObject::from_raw(Fselect_window(window.to_raw(), norecord.to_raw())) }
    } else {
        frame_ref.set_selected_window(window);
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
        let output_method = match unsafe { fget_output_method(frame.as_ptr()) } {
            output_initial | output_termcap => Qt,
            output_x_window => Qx,
            output_w32 => Qw32,
            output_msdos_raw => Qpc,
            output_ns => Qns,
            _ => panic!("Invalid frame output_method!"),
        };
        LispObject::from_raw(output_method)
    } else {
        LispObject::constant_nil()
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
    let f = frame.map_or(selected_frame(), |f| f.as_lisp_obj());

    if framep(f).is_t() {
        LispObject::constant_nil()
    } else {
        framep(f)
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
        LispObject::constant_t()
    } else if frame.is_iconified() {
        LispObject::from_raw(Qicon)
    } else {
        LispObject::constant_nil()
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
        LispObject::from_raw(Fcons(
            LispObject::from_fixnum(frame_ref.left_pos() as EmacsInt).to_raw(),
            LispObject::from_fixnum(frame_ref.top_pos() as EmacsInt).to_raw(),
        ))
    }
}

include!(concat!(env!("OUT_DIR"), "/frames_exports.rs"));
