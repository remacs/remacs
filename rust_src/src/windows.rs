//! Functions operating on windows.

use libc::c_int;

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Lisp_Window};
use remacs_sys::{Qceiling, Qfloor};
use remacs_sys::{fget_column_width, fget_line_height, is_minibuffer, minibuf_level,
                 minibuf_selected_window as current_minibuf_window,
                 selected_window as current_window};

use editfns::point;
use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use marker::marker_position;

pub type LispWindowRef = ExternalPtr<Lisp_Window>;

impl LispWindowRef {
    /// Check if window is a live window (displays a buffer).
    /// This is also sometimes called a "leaf window" in Emacs sources.
    #[inline]
    pub fn is_live(self) -> bool {
        LispObject::from(self.contents).is_buffer()
    }

    /// A window of any sort, leaf or interior, is "valid" if its
    /// contents slot is non-nil.
    #[inline]
    pub fn is_valid(self) -> bool {
        self.contents().is_not_nil()
    }

    #[inline]
    pub fn point_marker(self) -> LispObject {
        LispObject::from(self.pointm)
    }

    #[inline]
    pub fn contents(self) -> LispObject {
        LispObject::from(self.contents)
    }

    #[inline]
    pub fn frame(&self) -> LispObject {
        LispObject::from(self.frame)
    }

    #[inline]
    pub fn start_marker(self) -> LispObject {
        LispObject::from(self.start)
    }

    #[inline]
    pub fn is_internal(&self) -> bool {
        self.contents().is_window()
    }

    #[inline]
    pub fn is_minibuffer(&self) -> bool {
        unsafe { is_minibuffer(self.as_ptr()) }
    }

    pub fn total_width(&self, round: LispObject) -> i32 {
        let qfloor = LispObject::from(Qfloor);
        let qceiling = LispObject::from(Qceiling);

        if !(round == qfloor || round == qceiling) {
            self.total_cols
        } else {
            let frame = self.frame().as_frame_or_error();
            let unit = unsafe { fget_column_width(frame.as_ptr()) };

            if round == qceiling {
                (self.pixel_width + unit - 1) / unit
            } else {
                self.pixel_width / unit
            }
        }
    }

    pub fn total_height(&self, round: LispObject) -> i32 {
        let qfloor = LispObject::from(Qfloor);
        let qceiling = LispObject::from(Qceiling);

        if !(round == qfloor || round == qceiling) {
            self.total_lines
        } else {
            let frame = self.frame().as_frame_or_error();
            let unit = unsafe { fget_line_height(frame.as_ptr()) };

            if round == qceiling {
                (self.pixel_height + unit - 1) / unit
            } else {
                self.pixel_height / unit
            }
        }
    }
}

#[allow(dead_code)] // FIXME: Remove as soon as it is used
fn window_or_selected(window: LispObject) -> LispWindowRef {
    if window.is_nil() {
        selected_window()
    } else {
        window
    }.as_window_or_error()
}

fn window_live_or_selected(window: LispObject) -> LispWindowRef {
    if window.is_nil() {
        selected_window().as_window_or_error()
    } else {
        window.as_live_window_or_error()
    }
}

fn window_valid_or_selected(window: LispObject) -> LispWindowRef {
    if window.is_nil() {
        selected_window().as_window_or_error()
    } else {
        window.as_valid_window_or_error()
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
    let win = window_live_or_selected(window);
    if win == selected_window().as_window_or_error() {
        point()
    } else {
        marker_position(win.point_marker())
    }
}

/// Return the selected window.
/// The selected window is the window in which the standard cursor for
/// selected windows appears and to which many commands apply.
#[lisp_fn]
pub fn selected_window() -> LispObject {
    unsafe { LispObject::from(current_window) }
}

/// Return the buffer displayed in window WINDOW.
/// If WINDOW is omitted or nil, it defaults to the selected window.
/// Return nil for an internal window or a deleted window.
#[lisp_fn(min = "0")]
pub fn window_buffer(window: LispObject) -> LispObject {
    let win = window_valid_or_selected(window);
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
    LispObject::from_bool(object.as_window().map_or(false, |w| w.is_valid()))
}

/// Return position at which display currently starts in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
/// This is updated by redisplay or by calling `set-window-start'.
#[lisp_fn(min = "0")]
pub fn window_start(window: LispObject) -> LispObject {
    let win = window_live_or_selected(window);
    marker_position(win.start_marker())
}

/// Return non-nil if WINDOW is a minibuffer window.
/// WINDOW must be a valid window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_minibuffer_p(window: LispObject) -> LispObject {
    let win = window_valid_or_selected(window);
    LispObject::from_bool(win.is_minibuffer())
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
    let win = window_live_or_selected(window);

    LispObject::cons(
        margin_as_object(win.left_margin_cols),
        margin_as_object(win.right_margin_cols),
    )
}

/// Return combination limit of window WINDOW.
/// WINDOW must be a valid window used in horizontal or vertical combination.
/// If the return value is nil, child windows of WINDOW can be recombined with
/// WINDOW's siblings.  A return value of t means that child windows of
/// WINDOW are never (re-)combined with WINDOW's siblings.
#[lisp_fn]
pub fn window_combination_limit(window: LispObject) -> LispObject {
    let w = window.as_window_or_error();

    if !w.is_internal() {
        error!("Combination limit is meaningful for internal windows only");
    }

    LispObject::from(w.combination_limit)
}

/// Set combination limit of window WINDOW to LIMIT; return LIMIT.
/// WINDOW must be a valid window used in horizontal or vertical combination.
/// If LIMIT is nil, child windows of WINDOW can be recombined with WINDOW's
/// siblings.  LIMIT t means that child windows of WINDOW are never
/// (re-)combined with WINDOW's siblings.  Other values are reserved for
/// future use.
#[lisp_fn]
pub fn set_window_combination_limit(window: LispObject, limit: LispObject) -> LispObject {
    let mut w = window.as_window_or_error();

    if !w.is_internal() {
        error!("Combination limit is meaningful for internal windows only");
    }

    w.combination_limit = limit.to_raw();

    limit
}

/// Return the window which was selected when entering the minibuffer.
/// Returns nil, if selected window is not a minibuffer window.
#[lisp_fn]
pub fn minibuffer_selected_window() -> LispObject {
    let level = unsafe { minibuf_level };
    let current_minibuf = unsafe { LispObject::from(current_minibuf_window) };
    if level > 0 && selected_window().as_window_or_error().is_minibuffer()
        && current_minibuf.as_window().unwrap().is_live()
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
    let win = window_valid_or_selected(window);

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
/// total height of WINDOW.
#[lisp_fn(min = "0")]
pub fn window_total_height(window: LispObject, round: LispObject) -> LispObject {
    let win = window_valid_or_selected(window);

    LispObject::from_natnum(win.total_height(round) as EmacsInt)
}

/// Return the frame that window WINDOW is on.
/// WINDOW is optional and defaults to the selected window. If provided it must
/// be a valid window.
#[lisp_fn(min = "0")]
pub fn window_frame(window: LispObject) -> LispObject {
    let win = window_valid_or_selected(window);

    win.frame()
}

pub fn rust_init_syms() {
    unsafe {
        defsubr!(Sminibuffer_selected_window);
        defsubr!(Sselected_window);
        defsubr!(Sset_window_combination_limit);
        defsubr!(Swindow_buffer);
        defsubr!(Swindow_combination_limit);
        defsubr!(Swindow_frame);
        defsubr!(Swindow_live_p);
        defsubr!(Swindow_margins);
        defsubr!(Swindow_minibuffer_p);
        defsubr!(Swindow_point);
        defsubr!(Swindow_start);
        defsubr!(Swindow_total_height);
        defsubr!(Swindow_total_width);
        defsubr!(Swindow_valid_p);
        defsubr!(Swindowp);
    }
}
