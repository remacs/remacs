//! Functions operating on windows.

use lisp::{LispObject, ExternalPtr};
use remacs_macros::lisp_fn;
use remacs_sys::{Lisp_Window, selected_window as current_window};
use marker::marker_position;
use editfns::point;

pub type LispWindowRef = ExternalPtr<Lisp_Window>;

const FLAG_MINI: u16 = 1 << 0;

impl LispWindowRef {
    /// Check if window is a live window (displays a buffer).
    /// This is also sometimes called a "leaf window" in Emacs sources.
    #[inline]
    pub fn is_live(self) -> bool {
        LispObject::from_raw(self.contents).is_buffer()
    }

    #[inline]
    pub fn point_marker(self) -> LispObject {
        LispObject::from_raw(self.pointm)
    }

    #[inline]
    pub fn contents(self) -> LispObject {
        LispObject::from_raw(self.contents)
    }

    #[inline]
    pub fn start_marker(self) -> LispObject {
        LispObject::from_raw(self.start)
    }

    #[inline]
    pub fn is_minibuffer(&self) -> bool {
        self.flags & FLAG_MINI != 0
    }
}

/// Return t if OBJECT is a window and nil otherwise.
#[lisp_fn]
fn windowp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_window())
}

/// Return t if OBJECT is a live window and nil otherwise.
///
/// A live window is a window that displays a buffer.
/// Internal windows and deleted windows are not live.
#[lisp_fn]
pub fn window_live_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_window().map_or(false, |m| m.is_live()))
}

/// Return current value of point in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
///
/// For a nonselected window, this is the value point would have if that
/// window were selected.
///
/// Note that, when WINDOW is selected, the value returned is the same as
/// that returned by `point' for WINDOW's buffer.  It would be more strictly
/// correct to return the top-level value of `point', outside of any
/// `save-excursion' forms.  But that is hard to define.
#[lisp_fn(min = "0")]
pub fn window_point(window: LispObject) -> LispObject {
    if window.is_nil() || window == selected_window() {
        point()
    } else {
        let marker = window.as_live_window_or_error().point_marker();
        marker_position(marker)
    }
}

/// Return the selected window.
/// The selected window is the window in which the standard cursor for
/// selected windows appears and to which many commands apply.
#[lisp_fn]
pub fn selected_window() -> LispObject {
    unsafe { LispObject::from_raw(current_window) }
}

/// Return the buffer displayed in window WINDOW.
/// If WINDOW is omitted or nil, it defaults to the selected window.
/// Return nil for an internal window or a deleted window.
#[lisp_fn(min = "0")]
pub fn window_buffer(window: LispObject) -> LispObject {
    let win = if window.is_nil() {
        selected_window()
    } else {
        window
    };
    let win = win.as_window_or_error();
    if win.is_live() {
        win.contents()
    } else {
        LispObject::constant_nil()
    }
}

/// Return t if OBJECT is a valid window and nil otherwise.
/// A valid window is either a window that displays a buffer or an internal
/// window.  Windows that have been deleted are not valid.
#[lisp_fn]
pub fn window_valid_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_window().map_or(
        false,
        |win| win.contents().is_not_nil(),
    ))
}

/// Return position at which display currently starts in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
/// This is updated by redisplay or by calling `set-window-start'.
#[lisp_fn(min = "0")]
pub fn window_start(window: LispObject) -> LispObject {
    let win = if window.is_nil() {
        selected_window()
    } else {
        window
    };
    marker_position(win.as_live_window_or_error().start_marker())
}

/// Return non-nil if WINDOW is a minibuffer window.
/// WINDOW must be a valid window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_minibuffer_p(window: LispObject) -> LispObject {
    let win = if window.is_nil() {
        selected_window()
    } else {
        window
    };
    LispObject::from_bool(win.as_window_or_error().is_minibuffer())
}
