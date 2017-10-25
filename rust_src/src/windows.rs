//! Functions operating on windows.

use lisp::{LispObject, ExternalPtr};
use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Lisp_Window, Qceiling, Qfloor, selected_window as current_window,
                 minibuf_selected_window as current_minibuf_window, minibuf_level};
use marker::marker_position;
use editfns::point;
use libc::c_int;

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

    #[inline]
    pub fn frame(&self) -> LispObject {
        LispObject::from_raw(self.frame)
    }

    pub fn total_width(&self, round: LispObject) -> i32 {
        let qfloor = LispObject::from_raw(unsafe { Qfloor });
        let qceiling = LispObject::from_raw(unsafe { Qceiling });

        if !(round == qfloor || round == qceiling) {
            self.total_cols
        } else {
            let unit = self.frame().as_frame_or_error().column_width;

            if round == qceiling {
                (self.pixel_width + unit - 1) / unit
            } else {
                self.pixel_width / unit
            }
        }
    }

    pub fn total_height(&self, round: LispObject) -> i32 {
        let qfloor = LispObject::from_raw(unsafe { Qfloor });
        let qceiling = LispObject::from_raw(unsafe { Qceiling });

        if !(round == qfloor || round == qceiling) {
            self.total_lines
        } else {
            let unit = self.frame().as_frame_or_error().line_height;

            if round == qceiling {
                (self.pixel_height + unit - 1) / unit
            } else {
                self.pixel_height / unit
            }
        }
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

/// Get width of marginal areas of window WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
///
/// Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
/// If a marginal area does not exist, its width will be returned
/// as nil.
#[lisp_fn(min = "0")]
pub fn window_margins(window: LispObject) -> LispObject {
    fn margin_as_object(margin: c_int) -> LispObject {
        if margin != 0 {
            LispObject::from_fixnum(margin as EmacsInt)
        } else {
            LispObject::constant_nil()
        }
    }
    let win = if window.is_nil() {
        selected_window()
    } else {
        window
    }.as_live_window_or_error();

    LispObject::cons(
        margin_as_object(win.left_margin_cols),
        margin_as_object(win.right_margin_cols),
    )
}

/// Return the window which was selected when entering the minibuffer.
/// Returns nil, if selected window is not a minibuffer window.
#[lisp_fn]
pub fn minibuffer_selected_window() -> LispObject {
    let level = unsafe { minibuf_level };
    let current_minibuf = unsafe { LispObject::from_raw(current_minibuf_window) };
    if level > 0 && selected_window().as_window_or_error().is_minibuffer() &&
        current_minibuf.as_window().unwrap().is_live()
    {
        current_minibuf
    } else {
        LispObject::constant_nil()
    }
}

/// Return the total width of window WINDOW in columns.
/// WINDOW is optional and defaults to the selected window. If provided it must
/// be a valid window.
///
/// The return value includes the widths of WINDOW's fringes, margins,
/// scroll bars and its right divider, if any.  If WINDOW is an internal
/// window, the total width is the width of the screen areas spanned by its
/// children.
///
/// If WINDOW's pixel width is not an integral multiple of its frame's
/// character width, the number of lines occupied by WINDOW is rounded
/// internally.  This is done in a way such that, if WINDOW is a parent
/// window, the sum of the total widths of all its children internally
/// equals the total width of WINDOW.
///
/// If the optional argument ROUND is `ceiling', return the smallest integer
/// larger than WINDOW's pixel width divided by the character width of
/// WINDOW's frame.  ROUND `floor' means to return the largest integer
/// smaller than WINDOW's pixel width divided by the character width of
/// WINDOW's frame.  Any other value of ROUND means to return the internal
/// total width of WINDOW.
#[lisp_fn(min = "0")]
pub fn window_total_width(window: LispObject, round: LispObject) -> LispObject {
    let win = get_valid_window(window).as_window_or_error();

    LispObject::from_natnum(win.total_width(round) as EmacsInt)
}

/// Return the height of window WINDOW in lines.
/// WINDOW is optional and defaults to the selected window. If provided it must
/// be a valid window.
///
/// The return value includes the heights of WINDOW's mode and header line
/// and its bottom divider, if any.  If WINDOW is an internal window, the
/// total height is the height of the screen areas spanned by its children.
///
/// If WINDOW's pixel height is not an integral multiple of its frame's
/// character height, the number of lines occupied by WINDOW is rounded
/// internally.  This is done in a way such that, if WINDOW is a parent
/// window, the sum of the total heights of all its children internally
/// equals the total height of WINDOW.
///
/// If the optional argument ROUND is `ceiling', return the smallest integer
/// larger than WINDOW's pixel height divided by the character height of
/// WINDOW's frame.  ROUND `floor' means to return the largest integer
/// smaller than WINDOW's pixel height divided by the character height of
/// WINDOW's frame.  Any other value of ROUND means to return the internal
/// total height of WINDOW.  */)
#[lisp_fn(min = "0")]
pub fn window_total_height(window: LispObject, round: LispObject) -> LispObject {
    let win = get_valid_window(window).as_window_or_error();

    LispObject::from_natnum(win.total_height(round) as EmacsInt)
}

fn get_valid_window(window: LispObject) -> LispObject {
    if window.is_nil() {
        selected_window()
    } else {
        window
    }
}
