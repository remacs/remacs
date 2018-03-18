//! Functions operating on windows.

use std::ptr;

use libc::c_int;

use remacs_macros::lisp_fn;
use remacs_sys::{face_id, glyph_matrix, EmacsInt, Lisp_Type, Lisp_Window};
use remacs_sys::{MODE_LINE_FACE_ID, MODE_LINE_INACTIVE_FACE_ID};
use remacs_sys::{Qceiling, Qfloor, Qheader_line_format, Qmode_line_format, Qnone};
use remacs_sys::{estimate_mode_line_height, is_minibuffer, minibuf_level,
                 minibuf_selected_window as current_minibuf_window,
                 selected_window as current_window, wget_current_matrix, wget_mode_line_height,
                 wget_parent, wget_pixel_height, wget_pseudo_window_p, wget_window_parameters,
                 window_menu_bar_p, window_parameter, window_tool_bar_p, wset_mode_line_height,
                 wset_window_parameters, window_list_1};
use remacs_sys::Fcons;
use remacs_sys::globals;

use editfns::point;
use frames::{frame_live_or_selected, selected_frame, window_frame_live_or_selected, LispFrameRef};
use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use lists::{assq, setcdr};
use marker::marker_position_lisp;

pub type LispWindowRef = ExternalPtr<Lisp_Window>;

impl LispWindowRef {
    pub fn as_lisp_obj(self) -> LispObject {
        LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
    }

    /// Check if window is a live window (displays a buffer).
    /// This is also sometimes called a "leaf window" in Emacs sources.
    #[inline]
    pub fn is_live(self) -> bool {
        LispObject::from_raw(self.contents).is_buffer()
    }

    #[inline]
    pub fn is_pseudo(self) -> bool {
        unsafe { wget_pseudo_window_p(self.as_ptr()) }
    }

    /// A window of any sort, leaf or interior, is "valid" if its
    /// contents slot is non-nil.
    #[inline]
    pub fn is_valid(self) -> bool {
        self.contents().is_not_nil()
    }

    #[inline]
    pub fn point_marker(self) -> LispObject {
        LispObject::from_raw(self.pointm)
    }

    #[inline]
    pub fn contents(self) -> LispObject {
        LispObject::from_raw(self.contents)
    }

    /// Return the current height of the mode line of window W. If not known
    /// from W->mode_line_height, look at W's current glyph matrix, or return
    /// a default based on the height of the font of the face `mode-line'.
    pub fn current_mode_line_height(&mut self) -> i32 {
        let mode_line_height = unsafe { wget_mode_line_height(self.as_ptr()) };
        let matrix = LispGlyphMatrixRef::new(unsafe { wget_current_matrix(self.as_ptr()) });
        let matrix_mode_line_height = matrix.mode_line_height();

        if mode_line_height >= 0 {
            mode_line_height
        } else if matrix_mode_line_height != 0 {
            unsafe { wset_mode_line_height(self.as_mut(), matrix_mode_line_height) };
            matrix_mode_line_height
        } else {
            let frame = self.frame().as_frame_or_error();
            let window = selected_window().as_window_or_error();
            let mode_line_height = unsafe {
                estimate_mode_line_height(
                    frame.as_ptr(),
                    CURRENT_MODE_LINE_FACE_ID_3(*self, window, *self),
                )
            };
            unsafe { wset_mode_line_height(self.as_mut(), mode_line_height) };
            mode_line_height
        }
    }

    #[inline]
    pub fn frame(self) -> LispObject {
        LispObject::from_raw(self.frame)
    }

    #[inline]
    pub fn start_marker(self) -> LispObject {
        LispObject::from_raw(self.start)
    }

    #[inline]
    pub fn is_internal(self) -> bool {
        self.contents().is_window()
    }

    #[inline]
    pub fn is_minibuffer(self) -> bool {
        unsafe { is_minibuffer(self.as_ptr()) }
    }

    #[inline]
    pub fn is_menu_bar(self) -> bool {
        unsafe { window_menu_bar_p(self.as_ptr()) }
    }

    #[inline]
    pub fn is_tool_bar(self) -> bool {
        unsafe { window_tool_bar_p(self.as_ptr()) }
    }

    pub fn pixel_height(self) -> i32 {
        unsafe { wget_pixel_height(self.as_ptr()) }
    }

    pub fn total_width(self, round: LispObject) -> i32 {
        let qfloor = LispObject::from_raw(Qfloor);
        let qceiling = LispObject::from_raw(Qceiling);

        if !(round == qfloor || round == qceiling) {
            self.total_cols
        } else {
            let frame = self.frame().as_frame_or_error();
            let unit = frame.column_width();

            if round == qceiling {
                (self.pixel_width + unit - 1) / unit
            } else {
                self.pixel_width / unit
            }
        }
    }

    pub fn total_height(self, round: LispObject) -> i32 {
        let qfloor = LispObject::from_raw(Qfloor);
        let qceiling = LispObject::from_raw(Qceiling);

        if !(round == qfloor || round == qceiling) {
            self.total_lines
        } else {
            let frame = self.frame().as_frame_or_error();
            let unit = frame.line_height();

            if round == qceiling {
                (self.pixel_height + unit - 1) / unit
            } else {
                self.pixel_height / unit
            }
        }
    }

    /// The frame x-position at which the text (or left fringe) in
    /// window starts. This does not include a left-hand scroll bar
    /// if any.
    #[inline]
    pub fn left_edge_x(self) -> i32 {
        self.frame().as_frame_or_error().internal_border_width() + self.left_pixel_edge()
    }

    /// The frame y-position at which the window starts.
    #[inline]
    pub fn top_edge_y(self) -> i32 {
        let mut y = self.top_pixel_edge();
        if !(self.is_menu_bar() || self.is_tool_bar()) {
            y += self.frame().as_frame_or_error().internal_border_width();
        }
        y
    }

    /// The pixel value where the text (or left fringe) in window starts.
    #[inline]
    pub fn left_pixel_edge(self) -> i32 {
        self.pixel_left
    }

    /// The top pixel edge at which the window starts.
    /// This includes a header line, if any.
    #[inline]
    pub fn top_pixel_edge(self) -> i32 {
        self.pixel_top
    }

    /// Convert window relative pixel Y to frame pixel coordinates.
    #[inline]
    pub fn frame_pixel_y(self, y: i32) -> i32 {
        y + self.top_edge_y()
    }

    /// True if window wants a mode line and is high enough to
    /// accommodate it, false otherwise.
    ///
    /// Window wants a mode line if it's a leaf window and neither a minibuffer
    /// nor a pseudo window.  Moreover, its 'window-mode-line-format'
    /// parameter must not be 'none' and either that parameter or W's
    /// buffer's 'mode-line-format' value must be non-nil.  Finally, W must
    /// be higher than its frame's canonical character height.
    pub fn wants_mode_line(self) -> bool {
        let window_mode_line_format =
            LispObject::from_raw(unsafe { window_parameter(self.as_ptr(), Qmode_line_format) });

        self.is_live() && !self.is_minibuffer() && !self.is_pseudo()
            && !window_mode_line_format.eq(LispObject::from_raw(Qnone))
            && (window_mode_line_format.is_not_nil()
                || LispObject::from_raw(self.contents().as_buffer_or_error().mode_line_format)
                    .is_not_nil())
            && self.pixel_height() > self.frame().as_frame_or_error().line_height()
    }

    /// True if window wants a header line and is high enough to
    /// accommodate it, false otherwise.
    ///
    /// Window wants a header line if it's a leaf window and neither a minibuffer
    /// nor a pseudo window.  Moreover, its 'window-header-line-format'
    /// parameter must not be 'none' and either that parameter or window's
    /// buffer's 'header-line-format' value must be non-nil.  Finally, window must
    /// be higher than its frame's canonical character height and be able to
    /// accommodate a mode line too if necessary (the mode line prevails).
    pub fn wants_header_line(self) -> bool {
        let window_header_line_format =
            LispObject::from_raw(unsafe { window_parameter(self.as_ptr(), Qheader_line_format) });

        let mut height = self.frame().as_frame_or_error().line_height();
        if self.wants_mode_line() {
            height *= 2;
        }

        self.is_live() && !self.is_minibuffer() && !self.is_pseudo()
            && !window_header_line_format.eq(LispObject::from_raw(Qnone))
            && (window_header_line_format.is_not_nil()
                || LispObject::from_raw(self.contents().as_buffer_or_error().header_line_format)
                    .is_not_nil()) && self.pixel_height() > height
    }
}

pub type LispGlyphMatrixRef = ExternalPtr<glyph_matrix>;

impl LispGlyphMatrixRef {
    pub fn mode_line_height(self) -> i32 {
        if self.is_null() || self.rows.is_null() {
            0
        } else {
            unsafe { (*self.rows.offset((self.nrows - 1) as isize)).height }
        }
    }
}

pub fn window_or_selected_unchecked(window: LispObject) -> LispObject {
    if window.is_nil() {
        selected_window()
    } else {
        window
    }
}

/// Same as the `decode_any_window` function
#[allow(dead_code)] // FIXME: Remove as soon as it is used
fn window_or_selected(window: LispObject) -> LispWindowRef {
    window_or_selected_unchecked(window).as_window_or_error()
}

/// Same as the `decode_live_window` function
fn window_live_or_selected(window: LispObject) -> LispWindowRef {
    if window.is_nil() {
        selected_window().as_window_or_error()
    } else {
        window.as_live_window_or_error()
    }
}

/// Same as the `decode_valid_window` function
fn window_valid_or_selected(window: LispObject) -> LispWindowRef {
    if window.is_nil() {
        selected_window().as_window_or_error()
    } else {
        window.as_valid_window_or_error()
    }
}

/// Return t if OBJECT is a window and nil otherwise.
#[lisp_fn]
pub fn windowp(object: LispObject) -> bool {
    object.is_window()
}

/// Return t if OBJECT is a live window and nil otherwise.
///
/// A live window is a window that displays a buffer.
/// Internal windows and deleted windows are not live.
#[lisp_fn]
pub fn window_live_p(object: Option<LispWindowRef>) -> bool {
    object.map_or(false, |m| m.is_live())
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
pub fn window_point(window: LispObject) -> Option<EmacsInt> {
    let win = window_live_or_selected(window);
    if win == selected_window().as_window_or_error() {
        Some(point())
    } else {
        marker_position_lisp(win.point_marker().into())
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
pub fn window_valid_p(object: Option<LispWindowRef>) -> bool {
    object.map_or(false, |w| w.is_valid())
}

/// Return position at which display currently starts in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
/// This is updated by redisplay or by calling `set-window-start'.
#[lisp_fn(min = "0")]
pub fn window_start(window: LispObject) -> Option<EmacsInt> {
    let win = window_live_or_selected(window);
    marker_position_lisp(win.start_marker().into())
}

/// Return non-nil if WINDOW is a minibuffer window.
/// WINDOW must be a valid window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_minibuffer_p(window: LispObject) -> bool {
    let win = window_valid_or_selected(window);
    win.is_minibuffer()
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
            LispObject::from_fixnum(EmacsInt::from(margin))
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
pub fn window_combination_limit(window: LispWindowRef) -> LispObject {
    if !window.is_internal() {
        error!("Combination limit is meaningful for internal windows only");
    }

    LispObject::from_raw(window.combination_limit)
}

/// Set combination limit of window WINDOW to LIMIT; return LIMIT.
/// WINDOW must be a valid window used in horizontal or vertical combination.
/// If LIMIT is nil, child windows of WINDOW can be recombined with WINDOW's
/// siblings.  LIMIT t means that child windows of WINDOW are never
/// (re-)combined with WINDOW's siblings.  Other values are reserved for
/// future use.
#[lisp_fn]
pub fn set_window_combination_limit(mut window: LispWindowRef, limit: LispObject) -> LispObject {
    if !window.is_internal() {
        error!("Combination limit is meaningful for internal windows only");
    }

    window.combination_limit = limit.to_raw();

    limit
}

/// Return the window which was selected when entering the minibuffer.
/// Returns nil, if selected window is not a minibuffer window.
#[lisp_fn]
pub fn minibuffer_selected_window() -> LispObject {
    let level = unsafe { minibuf_level };
    let current_minibuf = unsafe { LispObject::from_raw(current_minibuf_window) };
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
pub fn window_total_width(window: LispObject, round: LispObject) -> EmacsInt {
    let win = window_valid_or_selected(window);

    EmacsInt::from(win.total_width(round))
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

    LispObject::from_natnum(EmacsInt::from(win.total_height(round)))
}

/// Return the parent window of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
/// Return nil for a window with no parent (e.g. a root window).
#[lisp_fn(min = "0")]
pub fn window_parent(window: LispObject) -> LispObject {
    LispObject::from_raw(unsafe { wget_parent(window_valid_or_selected(window).as_ptr()) })
}

/// Return the frame that window WINDOW is on.
/// WINDOW is optional and defaults to the selected window. If provided it must
/// be a valid window.
#[lisp_fn(min = "0")]
pub fn window_frame(window: LispObject) -> LispObject {
    let win = window_valid_or_selected(window);

    win.frame()
}

/// Return the root window of FRAME-OR-WINDOW.
/// If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
/// With a frame argument, return that frame's root window.
/// With a window argument, return the root window of that window's frame.
#[lisp_fn(min = "0")]
pub fn frame_root_window(frame_or_window: LispObject) -> LispObject {
    let frame = window_frame_live_or_selected(frame_or_window);
    frame.root_window()
}

/// Return the minibuffer window for frame FRAME.
/// If FRAME is omitted or nil, it defaults to the selected frame.
#[lisp_fn(min = "0")]
pub fn minibuffer_window(frame: LispObject) -> LispObject {
    let frame = frame_live_or_selected(frame);
    frame.minibuffer_window()
}

/// Return the display-table that WINDOW is using.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_display_table(window: LispObject) -> LispObject {
    let win = window_live_or_selected(window);
    LispObject::from_raw(win.display_table)
}

pub fn window_wants_mode_line(window: LispWindowRef) -> bool {
    window.wants_mode_line()
}

pub fn window_wants_header_line(window: LispWindowRef) -> bool {
    window.wants_header_line()
}

/// Set WINDOW's value of PARAMETER to VALUE.
/// WINDOW can be any window and defaults to the selected one.
/// Return VALUE.
#[lisp_fn]
pub fn set_window_parameter(
    window: LispObject,
    parameter: LispObject,
    value: LispObject,
) -> LispObject {
    let w = window_or_selected(window);
    let w_params = unsafe { wget_window_parameters(w.as_ptr()) };
    let old_alist_elt = assq(parameter, LispObject::from_raw(w_params));
    if old_alist_elt.is_nil() {
        unsafe {
            wset_window_parameters(
                w.as_ptr(),
                Fcons(Fcons(parameter.to_raw(), value.to_raw()), w_params),
            )
        }
    } else {
        setcdr(old_alist_elt.as_cons_or_error(), value);
    }
    value
}

/// Return the desired face id for the mode line of a window, depending
/// on whether the window is selected or not, or if the window is the
/// scrolling window for the currently active minibuffer window.
///
/// Due to the way display_mode_lines manipulates with the contents of
/// selected_window, this function needs three arguments: SELW which is
/// compared against the current value of selected_window, MBW which is
/// compared against minibuf_window (if SELW doesn't match), and SCRW
/// which is compared against minibuf_selected_window (if MBW matches).
#[no_mangle]
pub extern "C" fn CURRENT_MODE_LINE_FACE_ID_3(
    selw: LispWindowRef,
    mbw: LispWindowRef,
    scrw: LispWindowRef,
) -> i32 {
    let current = if let Some(w) = selected_window().as_window() {
        w
    } else {
        LispWindowRef::new(ptr::null_mut())
    };

    unsafe {
        if !globals.f_mode_line_in_non_selected_windows {
            if selw == current {
                return MODE_LINE_FACE_ID;
            } else if minibuf_level > 0 {
                let minibuf = LispObject::from_raw(current_minibuf_window);
                if let Some(minibuf_window) = minibuf.as_window() {
                    if mbw == minibuf_window && scrw == minibuf_window {
                        return MODE_LINE_FACE_ID;
                    }
                }
            }
        }
    }

    MODE_LINE_INACTIVE_FACE_ID
}

/// Return the desired face id for the mode line of window W.
#[no_mangle]
pub extern "C" fn CURRENT_MODE_LINE_FACE_ID(window: LispWindowRef) -> face_id {
    let current = if let Some(w) = selected_window().as_window() {
        w
    } else {
        LispWindowRef::new(ptr::null_mut())
    };

    CURRENT_MODE_LINE_FACE_ID_3(window, current, window)
}

#[no_mangle]
pub extern "C" fn CURRENT_MODE_LINE_HEIGHT(mut window: LispWindowRef) -> i32 {
    window.current_mode_line_height()
}

/// Return a list of windows on FRAME, starting with WINDOW.
/// FRAME nil or omitted means use the selected frame.
/// WINDOW nil or omitted means use the window selected within FRAME.
/// MINIBUF t means include the minibuffer window, even if it isn't active.
/// MINIBUF nil or omitted means include the minibuffer window only
/// if it's active.
/// MINIBUF neither nil nor t means never include the minibuffer window.
#[lisp_fn(min = "0")]
pub fn window_list(
    frame: Option<LispFrameRef>,
    minibuf: LispObject,
    window: Option<LispWindowRef>,
) -> LispObject {
    let w_obj = match window {
        Some(w) => w.as_lisp_obj(),
        None => {
            if let Some(f) = frame {
                f.selected_window()
            } else {
                selected_window()
            }
        }
    };

    let f_obj = match frame {
        None => selected_frame(),
        Some(f) => f.as_lisp_obj(),
    };

    let w_ref = w_obj
        .as_window()
        .unwrap_or_else(|| panic!("Invalid window reference."));

    if !f_obj.eq(w_ref.frame()) {
        error!("Window is on a different frame");
    }

    unsafe {
        LispObject::from_raw(window_list_1(
            w_obj.to_raw(),
            minibuf.to_raw(),
            f_obj.to_raw(),
        ))
    }
}

/// Return a list of all live windows.
/// WINDOW specifies the first window to list and defaults to the selected
/// window.
///
/// Optional argument MINIBUF nil or omitted means consider the minibuffer
/// window only if the minibuffer is active.  MINIBUF t means consider the
/// minibuffer window even if the minibuffer is not active.  Any other value
/// means do not consider the minibuffer window even if the minibuffer is
/// active.
///
/// Optional argument ALL-FRAMES nil or omitted means consider all windows
/// on WINDOW's frame, plus the minibuffer window if specified by the
/// MINIBUF argument.  If the minibuffer counts, consider all windows on all
/// frames that share that minibuffer too.  The following non-nil values of
/// ALL-FRAMES have special meanings:
///
/// - t means consider all windows on all existing frames.
///
/// - `visible' means consider all windows on all visible frames.
///
/// - 0 (the number zero) means consider all windows on all visible and
///   iconified frames.
///
/// - A frame means consider all windows on that frame only.
///
/// Anything else means consider all windows on WINDOW's frame and no
/// others.
///
/// If WINDOW is not on the list of windows returned, some other window will
/// be listed first but no error is signaled.
#[lisp_fn(min = "0", name = "window-list-1")]
pub fn window_list_one(
    window: LispObject,
    minibuf: LispObject,
    all_frames: LispObject,
) -> LispObject {
    unsafe {
        LispObject::from_raw(window_list_1(
            window.to_raw(),
            minibuf.to_raw(),
            all_frames.to_raw(),
        ))
    }
}

include!(concat!(env!("OUT_DIR"), "/windows_exports.rs"));
