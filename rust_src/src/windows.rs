//! Functions operating on windows.

use std::{cmp, fmt, ptr};

use libc::c_int;

use remacs_macros::lisp_fn;

use crate::{
    buffers::{set_buffer, LispBufferRef},
    editfns::{goto_char, point},
    eval::unbind_to,
    frames::{LispFrameLiveOrSelected, LispFrameOrSelected, LispFrameRef},
    interactive::InteractiveNumericPrefix,
    lisp::{ExternalPtr, LispObject},
    lists::{assq, setcdr},
    marker::{marker_position_lisp, set_marker_restricted},
    numbers::{check_range, LispNumber},
    remacs_sys::face_id::HEADER_LINE_FACE_ID,
    remacs_sys::globals,
    remacs_sys::glyph_row_area::TEXT_AREA,
    remacs_sys::{
        apply_window_adjustment, estimate_mode_line_height, minibuf_level,
        minibuf_selected_window as current_minibuf_window, noninteractive, record_unwind_protect,
        save_excursion_restore, save_excursion_save, select_window,
        selected_window as current_window, set_buffer_internal, set_window_fringes,
        set_window_hscroll, update_mode_lines, window_list_1, window_menu_bar_p, window_scroll,
        window_tool_bar_p, windows_or_buffers_changed, wset_redisplay,
    },
    remacs_sys::{face_id, glyph_matrix, glyph_row, pvec_type, vertical_scroll_bar_type},
    remacs_sys::{EmacsDouble, EmacsInt, Lisp_Type, Lisp_Window},
    remacs_sys::{Fcopy_alist, Fnreverse},
    remacs_sys::{
        Qceiling, Qfloor, Qheader_line_format, Qleft, Qmode_line_format, Qnil, Qnone, Qright, Qt,
        Qwindow_live_p, Qwindow_valid_p, Qwindowp,
    },
    threads::{c_specpdl_index, ThreadState},
    vectors::LispVectorlikeRef,
};

pub type LispWindowRef = ExternalPtr<Lisp_Window>;

impl LispWindowRef {
    /// Check if window is a live window (displays a buffer).
    /// This is also sometimes called a "leaf window" in Emacs sources.
    pub fn is_live(self) -> bool {
        self.contents.is_buffer()
    }

    pub fn is_pseudo(self) -> bool {
        self.pseudo_window_p()
    }

    pub fn is_outdated(self) -> bool {
        let buffer: LispBufferRef = self.contents.into();
        self.last_modified < buffer.modifications()
            || self.last_overlay_modified < buffer.overlay_modifications()
    }

    /// A window of any sort, leaf or interior, is "valid" if its
    /// contents slot is non-nil.
    pub fn is_valid(self) -> bool {
        self.contents.is_not_nil()
    }

    // Equivalent to WINDOW_RIGHTMOST_P
    /// True if window W has no other windows to its right on its frame.
    pub fn is_rightmost(self) -> bool {
        self.right_pixel_edge() == self.get_frame().root_window().right_pixel_edge()
    }

    // Equivalent to WINDOW_BOTTOMMOST_P
    /// True if window W has no other windows below it on its frame (the
    /// minibuffer window is not counted in this respect unless W itself is a
    /// minibuffer window).
    pub fn is_bottommost(self) -> bool {
        self.bottom_pixel_edge() == self.get_frame().root_window().bottom_pixel_edge()
    }

    pub fn get_frame(self) -> LispFrameRef {
        self.frame.into()
    }

    pub fn get_matrix(self) -> LispGlyphMatrixRef {
        LispGlyphMatrixRef::new(self.current_matrix)
    }

    /// Return the current height of the mode line of window W. If not known
    /// from W->mode_line_height, look at W's current glyph matrix, or return
    /// a default based on the height of the font of the face `mode-line'.
    pub fn current_mode_line_height(&mut self) -> i32 {
        let mode_line_height = self.mode_line_height;
        let matrix_mode_line_height = self.get_matrix().mode_line_height();

        if mode_line_height >= 0 {
            mode_line_height
        } else if matrix_mode_line_height != 0 {
            self.mode_line_height = matrix_mode_line_height;
            matrix_mode_line_height
        } else {
            let mut frame = self.get_frame();
            let window: LispWindowRef = selected_window().into();
            let mode_line_height = unsafe {
                estimate_mode_line_height(frame.as_mut(), CURRENT_MODE_LINE_FACE_ID(window))
            };
            self.mode_line_height = mode_line_height;
            mode_line_height
        }
    }

    // Equivalent to WINDOW_MODE_LINE_HEIGHT
    /// Height in pixels of the mode line.
    /// May be zero if W doesn't have a mode line.
    pub fn mode_line_height(mut self) -> i32 {
        if self.wants_header_line() {
            self.current_mode_line_height()
        } else {
            0
        }
    }

    pub fn start_marker(self) -> LispObject {
        self.start
    }

    pub fn is_internal(self) -> bool {
        self.contents.is_window()
    }

    pub fn is_minibuffer(self) -> bool {
        self.mini()
    }

    // Equivalent to MINI_NON_ONLY_P
    /// True if W is a minibuffer window on a frame that contains at least
    /// one other window
    pub fn is_minibuffer_non_only(self) -> bool {
        self.is_minibuffer() && self.prev.is_not_nil()
    }

    // Equivalent to MINI_ONLY_WINDOW_P
    /// True if W is a minibuffer window that is alone on its frame.
    pub fn is_minibuffer_only(self) -> bool {
        self.is_minibuffer() && self.prev.is_nil()
    }

    pub fn is_menu_bar(mut self) -> bool {
        unsafe { window_menu_bar_p(self.as_mut()) }
    }

    pub fn is_tool_bar(mut self) -> bool {
        unsafe { window_tool_bar_p(self.as_mut()) }
    }

    pub fn total_width(self, round: LispObject) -> i32 {
        let qfloor = Qfloor;
        let qceiling = Qceiling;

        if !(round == qfloor || round == qceiling) {
            self.total_cols
        } else {
            let frame = self.get_frame();
            let unit = frame.column_width;

            if round == qceiling {
                (self.pixel_width + unit - 1) / unit
            } else {
                self.pixel_width / unit
            }
        }
    }

    pub fn total_height(self, round: LispObject) -> i32 {
        let qfloor = Qfloor;
        let qceiling = Qceiling;

        if !(round == qfloor || round == qceiling) {
            self.total_lines
        } else {
            let frame = self.get_frame();
            let unit = frame.line_height;

            if round == qceiling {
                (self.pixel_height + unit - 1) / unit
            } else {
                self.pixel_height / unit
            }
        }
    }

    /// Return the number of lines/pixels of W's body. Don't count any mode
    /// or header line or horizontal divider of W. Rounds down to nearest
    /// integer when not working pixelwise.
    pub fn body_height(self, pixelwise: bool) -> i32 {
        let height = self.pixel_height
            - self.header_line_height()
            - if self.has_horizontal_scroll_bar() {
                self.scroll_bar_area_height()
            } else {
                0
            }
            - self.mode_line_height()
            - self.bottom_divider_width();

        if pixelwise {
            cmp::max(height, 0)
        } else {
            cmp::max(height / self.get_frame().line_height, 0)
        }
    }

    /// Return the number of columns/pixels of W's body.  Don't count columns
    /// occupied by the scroll bar or the divider/vertical bar separating W
    /// from its right sibling or margins.  On window-systems don't count
    /// fringes either.  Round down to nearest integer when not working
    /// pixelwise.
    pub fn body_width(self, pixelwise: bool) -> i32 {
        let width = self.pixel_width
            - self.right_divider_width()
            - self.margins_width()
            - if self.has_vertical_scroll_bar() {
                self.scroll_bar_area_width()
            } else if !self.get_frame().is_gui_window()
                && !self.is_rightmost()
                && self.right_divider_width() == 0
            {
                1
            } else {
                0
            }
            - if self.get_frame().is_gui_window() {
                self.fringes_width()
            } else {
                0
            };

        if pixelwise {
            cmp::max(width, 0)
        } else {
            cmp::max(width / self.get_frame().column_width, 0)
        }
    }

    /// The frame x-position at which the text (or left fringe) in
    /// window starts. This does not include a left-hand scroll bar
    /// if any.
    pub fn left_edge_x(self) -> i32 {
        self.get_frame().internal_border_width() + self.left_pixel_edge()
    }

    /// The frame y-position at which the window starts.
    pub fn top_edge_y(self) -> i32 {
        let mut y = self.top_pixel_edge();
        if !(self.is_menu_bar() || self.is_tool_bar()) {
            y += self.get_frame().internal_border_width();
        }
        y
    }

    /// The pixel value where the text (or left fringe) in window starts.
    pub fn left_pixel_edge(self) -> i32 {
        self.pixel_left
    }

    /// The top pixel edge at which the window starts.
    /// This includes a header line, if any.
    pub fn top_pixel_edge(self) -> i32 {
        self.pixel_top
    }

    /// Return the bottom pixel edge before which window W ends.
    /// This includes a mode line, if any.
    pub fn bottom_pixel_edge(self) -> i32 {
        self.top_pixel_edge() + self.pixel_height
    }

    /// Return the right pixel edge before which window W ends.
    /// This includes a right-hand scroll bar, if any.
    pub fn right_pixel_edge(self) -> i32 {
        self.left_pixel_edge() + self.pixel_width
    }

    /// Convert window relative pixel Y to frame pixel coordinates.
    pub fn frame_pixel_y(self, y: i32) -> i32 {
        y + self.top_edge_y()
    }

    pub fn contents_as_buffer(self) -> LispBufferRef {
        self.contents.into()
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
        let window_mode_line_format = self.get_parameter(Qmode_line_format);

        self.is_live()
            && !self.is_minibuffer()
            && !self.is_pseudo()
            && !window_mode_line_format.eq(Qnone)
            && (window_mode_line_format.is_not_nil()
                || self.contents_as_buffer().mode_line_format_.is_not_nil())
            && self.pixel_height > self.get_frame().line_height
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
        let window_header_line_format = self.get_parameter(Qheader_line_format);

        let mut height = self.get_frame().line_height;
        if self.wants_mode_line() {
            height *= 2;
        }

        self.is_live()
            && !self.is_minibuffer()
            && !self.is_pseudo()
            && !window_header_line_format.eq(Qnone)
            && (window_header_line_format.is_not_nil()
                || (self.contents_as_buffer().header_line_format_).is_not_nil())
            && self.pixel_height > height
    }

    pub fn current_header_line_height(mut self) -> i32 {
        let matrix = self.get_matrix();
        if self.header_line_height >= 0 {
            self.header_line_height
        } else {
            self.header_line_height = if matrix.header_line_height() != 0 {
                matrix.header_line_height()
            } else {
                unsafe { estimate_mode_line_height(self.get_frame().as_mut(), HEADER_LINE_FACE_ID) }
            };
            self.header_line_height
        }
    }

    // Equivalent to WINDOW_HEADER_LINE_HEIGHT
    /// Height in pixels of the header line.
    /// Zero if the window doesn't have a header line.
    pub fn header_line_height(self) -> i32 {
        if self.wants_header_line() {
            self.current_header_line_height()
        } else {
            0
        }
    }

    pub fn left_fringe_width(self) -> i32 {
        if self.left_fringe_width >= 0 {
            self.left_fringe_width
        } else {
            self.get_frame().left_fringe_width
        }
    }

    pub fn right_fringe_width(self) -> i32 {
        if self.right_fringe_width >= 0 {
            self.right_fringe_width
        } else {
            self.get_frame().left_fringe_width
        }
    }

    pub fn fringes_width(self) -> i32 {
        self.left_fringe_width() + self.right_fringe_width()
    }

    pub fn left_margin_width(self) -> i32 {
        self.left_margin_cols * self.get_frame().column_width
    }

    pub fn right_margin_width(self) -> i32 {
        self.right_margin_cols * self.get_frame().column_width
    }

    pub fn margins_width(self) -> i32 {
        self.left_margin_width() + self.right_margin_width()
    }

    /// True if window W is a vertical combination of windows.
    pub fn is_vertical_combination(self) -> bool {
        self.is_internal() && !self.horizontal()
    }

    pub fn get_parameter(self, parameter: LispObject) -> LispObject {
        match assq(parameter, self.window_parameters).into() {
            Some((_, cdr)) => cdr,
            None => Qnil,
        }
    }

    pub fn vertical_scroll_bar_type(self) -> u32 {
        use crate::remacs_sys::vertical_scroll_bar_type as scroll;
        if self.is_pseudo() {
            scroll::vertical_scroll_bar_none
        } else {
            match self.vertical_scroll_bar_type {
                Qt => self.get_frame().vertical_scroll_bar_type(),
                Qleft => scroll::vertical_scroll_bar_left,
                Qright => scroll::vertical_scroll_bar_right,
                _ => scroll::vertical_scroll_bar_none,
            }
        }
    }

    pub fn has_vertical_scroll_bar_left(self) -> bool {
        self.vertical_scroll_bar_type() == vertical_scroll_bar_type::vertical_scroll_bar_left
    }

    pub fn has_vertical_scroll_bar_right(self) -> bool {
        self.vertical_scroll_bar_type() == vertical_scroll_bar_type::vertical_scroll_bar_right
    }

    pub fn has_vertical_scroll_bar(self) -> bool {
        self.has_vertical_scroll_bar_left() || self.has_vertical_scroll_bar_right()
    }

    // Equivalent to WINDOW_HAS_HORIZONTAL_SCROLL_BAR
    /// True if horizontal scroll bars are currently enabled for the window.
    /// Horizontal scrollbars only exist when a toolkit is enabled.
    #[cfg(feature = "window-system")]
    pub fn has_horizontal_scroll_bar(self) -> bool {
        use crate::remacs_sys::Qbottom;
        if self.is_pseudo() || self.is_minibuffer_non_only() {
            false
        } else {
            match self.horizontal_scroll_bar_type {
                Qt => self.get_frame().horizontal_scroll_bars(),
                Qbottom => true,
                _ => false,
            }
        }
    }
    #[cfg(not(feature = "window-system"))]
    pub fn has_horizontal_scroll_bar(self) -> bool {
        false
    }

    /// Width of the scroll bar area of the window, measured in pixels.
    pub fn scroll_bar_area_width(self) -> i32 {
        if self.has_vertical_scroll_bar() {
            self.config_scroll_bar_width()
        } else {
            0
        }
    }

    /// Height of scroll bar area in the window, measured in pixels.
    pub fn scroll_bar_area_height(self) -> i32 {
        if self.has_horizontal_scroll_bar() {
            self.config_scroll_bar_height()
        } else {
            0
        }
    }

    /// Width of the bottom divider of the window
    pub fn right_divider_width(self) -> i32 {
        if !self.is_rightmost() {
            self.get_frame().right_divider_width
        } else {
            0
        }
    }

    /// Width of the window's bottom divider
    pub fn bottom_divider_width(self) -> i32 {
        if self.is_bottommost() && self.get_frame().root_window().next.is_not_nil()
            || self.prev.eq(self.get_frame().root_window)
            || self.is_pseudo()
        {
            0
        } else {
            self.get_frame().bottom_divider_width
        }
    }

    /// Width that a scroll bar in window W should have, if there is one.
    /// Measured in pixels.  If scroll bars are turned off, this is still
    /// nonzero.
    pub fn config_scroll_bar_width(self) -> i32 {
        if self.scroll_bar_width >= 0 {
            self.scroll_bar_width
        } else {
            self.get_frame().config_scroll_bar_width
        }
    }

    /// Height that a scroll bar in window W should have, if there is one.
    /// Measured in pixels.  If scroll bars are turned off, this is still
    /// nonzero.
    pub fn config_scroll_bar_height(self) -> i32 {
        if self.scroll_bar_height >= 0 {
            self.scroll_bar_height
        } else {
            self.get_frame().config_scroll_bar_height
        }
    }

    /// Return the bottom boundary y-position for text lines in window W.
    /// This is the first y position at which a line cannot start.
    /// It is relative to the top of the window.
    ///
    /// This is the height of W minus the height of a mode line, if any.
    pub fn text_bottom_y(mut self) -> i32 {
        self.pixel_height
            - self.bottom_divider_width()
            - self.scroll_bar_area_height()
            - if self.wants_mode_line() {
                self.current_mode_line_height()
            } else {
                0
            }
    }

    /// Equivalent to WINDOW_LEFT_FRINGE_WIDTH
    pub fn get_left_fringe_width(self) -> i32 {
        if self.left_fringe_width >= 0 {
            self.left_fringe_width
        } else {
            self.get_frame().left_fringe_width
        }
    }

    /// Equivalent to WINDOW_RIGHT_FRINGE_WIDTH
    pub fn get_right_fringe_width(self) -> i32 {
        if self.right_fringe_width >= 0 {
            self.right_fringe_width
        } else {
            self.get_frame().right_fringe_width
        }
    }
}

impl From<LispObject> for LispWindowRef {
    fn from(o: LispObject) -> Self {
        o.as_window().unwrap_or_else(|| wrong_type!(Qwindowp, o))
    }
}

impl From<LispWindowRef> for LispObject {
    fn from(w: LispWindowRef) -> Self {
        Self::tag_ptr(w, Lisp_Type::Lisp_Vectorlike)
    }
}

impl From<LispObject> for Option<LispWindowRef> {
    fn from(o: LispObject) -> Self {
        o.as_vectorlike().and_then(LispVectorlikeRef::as_window)
    }
}

impl fmt::Debug for LispWindowRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "window({:?})", self.as_ptr())
    }
}

impl LispObject {
    pub fn is_window(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_WINDOW))
    }

    pub fn as_window(self) -> Option<LispWindowRef> {
        self.into()
    }

    pub fn as_minibuffer_or_error(self) -> LispWindowRef {
        let w = self
            .as_window()
            .unwrap_or_else(|| wrong_type!(Qwindowp, self));
        if !w.is_minibuffer() {
            error!("Window is not a minibuffer window");
        }
        w
    }

    pub fn as_live_window(self) -> Option<LispWindowRef> {
        self.as_window()
            .and_then(|w| if w.is_live() { Some(w) } else { None })
    }

    pub fn as_live_window_or_error(self) -> LispWindowRef {
        self.as_live_window()
            .unwrap_or_else(|| wrong_type!(Qwindow_live_p, self))
    }

    pub fn as_valid_window(self) -> Option<LispWindowRef> {
        self.as_window()
            .and_then(|w| if w.is_valid() { Some(w) } else { None })
    }

    pub fn as_valid_window_or_error(self) -> LispWindowRef {
        self.as_valid_window()
            .unwrap_or_else(|| wrong_type!(Qwindow_valid_p, self))
    }
}

pub type LispGlyphMatrixRef = ExternalPtr<glyph_matrix>;

impl LispGlyphMatrixRef {
    /// Get a pointer to row number ROW.
    pub unsafe fn row_unchecked(self, row: usize) -> LispGlyphRowRef {
        LispGlyphRowRef::new(self.rows.add(row))
    }

    /// Get a pointer to row number ROW. Throws an error if out of range.
    pub fn row(self, row: usize) -> LispGlyphRowRef {
        check_range(row as EmacsInt, 0, self.nrows);
        unsafe { self.row_unchecked(row) }
    }

    pub fn mode_line_height(self) -> i32 {
        if self.is_null() || self.rows.is_null() {
            0
        } else {
            unsafe { (*self.rows.offset((self.nrows - 1) as isize)).height }
        }
    }
    pub fn header_line_height(self) -> i32 {
        if self.is_null() || self.rows.is_null() {
            0
        } else {
            unsafe { (*self.rows).height }
        }
    }

    /// Return a pointer to the first row used for text display
    pub fn first_text_row(self) -> LispGlyphRowRef {
        unsafe {
            if (*self.rows).mode_line_p() {
                LispGlyphRowRef::new(self.rows.offset(1))
            } else {
                LispGlyphRowRef::new(self.rows)
            }
        }
    }

    pub fn bottom_text_row(self, window: LispWindowRef) -> LispGlyphRowRef {
        let ptr = unsafe {
            self.rows
                .offset((self.nrows - if window.wants_mode_line() { 1 } else { 0 }) as isize)
        };
        LispGlyphRowRef::new(ptr)
    }
}

pub type LispGlyphRowRef = ExternalPtr<glyph_row>;

#[derive(Debug)]
pub struct LispWindowOrSelected(LispObject);

impl From<LispObject> for LispWindowOrSelected {
    /// Same as `decode_any_window`
    fn from(obj: LispObject) -> Self {
        Self(obj.map_or_else(selected_window, |w| w))
    }
}

impl From<LispWindowOrSelected> for LispObject {
    fn from(w: LispWindowOrSelected) -> Self {
        w.0
    }
}

impl From<LispWindowOrSelected> for LispWindowRef {
    fn from(w: LispWindowOrSelected) -> Self {
        w.0.into()
    }
}

#[derive(Debug)]
pub struct LispWindowLiveOrSelected(LispWindowRef);

impl From<LispObject> for LispWindowLiveOrSelected {
    /// Same as the `decode_live_window` function
    fn from(obj: LispObject) -> Self {
        Self(obj.map_or_else(
            || LispWindowRef::from(unsafe { current_window }),
            LispObject::as_live_window_or_error,
        ))
    }
}

impl From<LispWindowLiveOrSelected> for LispWindowRef {
    fn from(w: LispWindowLiveOrSelected) -> Self {
        w.0
    }
}

pub struct LispWindowValidOrSelected(LispWindowRef);

impl From<LispObject> for LispWindowValidOrSelected {
    /// Same as the `decode_valid_window` function
    fn from(obj: LispObject) -> Self {
        Self(obj.map_or_else(
            || LispWindowRef::from(selected_window()),
            LispObject::as_valid_window_or_error,
        ))
    }
}

impl From<LispWindowValidOrSelected> for LispWindowRef {
    fn from(w: LispWindowValidOrSelected) -> Self {
        w.0
    }
}

#[no_mangle]
pub extern "C" fn window_body_width(window: *mut Lisp_Window, pixelwise: bool) -> i32 {
    LispWindowRef::new(window).body_width(pixelwise)
}

#[no_mangle]
pub extern "C" fn decode_any_window(window: LispObject) -> LispWindowRef {
    LispWindowOrSelected::from(window).into()
}

/// Return the normal height of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
/// If HORIZONTAL is non-nil, return the normal width of WINDOW.
///
/// The normal height of a frame's root window or a window that is
/// horizontally combined (a window that has a left or right sibling) is
/// 1.0.  The normal height of a window that is vertically combined (has a
/// sibling above or below) is the fraction of the window's height with
/// respect to its parent.  The sum of the normal heights of all windows in a
/// vertical combination equals 1.0.
///
/// Similarly, the normal width of a frame's root window or a window that is
/// vertically combined equals 1.0.  The normal width of a window that is
/// horizontally combined is the fraction of the window's width with respect
/// to its parent.  The sum of the normal widths of all windows in a
/// horizontal combination equals 1.0.
///
/// The normal sizes of windows are used to restore the proportional sizes
/// of windows after they have been shrunk to their minimum sizes; for
/// example when a frame is temporarily made very small and afterwards gets
/// re-enlarged to its previous size.
#[lisp_fn(min = "0")]
pub fn window_normal_size(window: LispWindowValidOrSelected, horizontal: bool) -> EmacsDouble {
    let win: LispWindowRef = window.into();
    let frac = if !horizontal {
        win.normal_lines
    } else {
        win.normal_cols
    };
    EmacsDouble::from(frac)
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
    object.map_or(false, LispWindowRef::is_live)
}

/// Return new pixel size of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
///
/// The new pixel size of WINDOW is the value set by the last call of
/// `set-window-new-pixel' for WINDOW.  If it is valid, it will be shortly
/// installed as WINDOW's pixel height (see `window-pixel-height') or pixel
/// width (see `window-pixel-width').
#[lisp_fn(min = "0")]
pub fn window_new_pixel(window: LispWindowValidOrSelected) -> EmacsInt {
    let win: LispWindowRef = window.into();
    win.new_pixel.into()
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
pub fn window_point(window: LispWindowLiveOrSelected) -> Option<EmacsInt> {
    let win: LispWindowRef = window.into();
    if win == LispWindowRef::from(selected_window()) {
        Some(point())
    } else {
        marker_position_lisp(win.pointm.into())
    }
}

/// Return the selected window.
/// The selected window is the window in which the standard cursor for
/// selected windows appears and to which many commands apply.
#[lisp_fn]
pub fn selected_window() -> LispObject {
    unsafe { current_window }
}

/// Return the buffer displayed in window WINDOW.
/// If WINDOW is omitted or nil, it defaults to the selected window.
/// Return nil for an internal window or a deleted window.
#[lisp_fn(min = "0")]
pub fn window_buffer(window: LispWindowOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    if win.is_live() {
        win.contents
    } else {
        Qnil
    }
}

/// Return t if OBJECT is a valid window and nil otherwise.
/// A valid window is either a window that displays a buffer or an internal
/// window.  Windows that have been deleted are not valid.
#[lisp_fn]
pub fn window_valid_p(object: Option<LispWindowRef>) -> bool {
    object.map_or(false, LispWindowRef::is_valid)
}

/// Return position at which display currently starts in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
/// This is updated by redisplay or by calling `set-window-start'.
#[lisp_fn(min = "0")]
pub fn window_start(window: LispWindowLiveOrSelected) -> Option<EmacsInt> {
    let win: LispWindowRef = window.into();
    marker_position_lisp(win.start_marker().into())
}

/// Return non-nil if WINDOW is a minibuffer window.
/// WINDOW must be a valid window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_minibuffer_p(window: LispWindowValidOrSelected) -> bool {
    let win: LispWindowRef = window.into();
    win.is_minibuffer()
}

/// Return the width of window WINDOW in pixels.
/// WINDOW must be a valid window and defaults to the selected one.
///
/// The return value includes the fringes and margins of WINDOW as well as
/// any vertical dividers or scroll bars belonging to WINDOW.  If WINDOW is
/// an internal window, its pixel width is the width of the screen areas
/// spanned by its children.
#[lisp_fn(min = "0")]
pub fn window_pixel_width(window: LispWindowValidOrSelected) -> i32 {
    let win: LispWindowRef = window.into();
    win.pixel_width
}

/// Return the height of window WINDOW in pixels.
/// WINDOW must be a valid window and defaults to the selected one.
///
/// The return value includes the mode line and header line and the bottom
/// divider, if any.  If WINDOW is an internal window, its pixel height is
/// the height of the screen areas spanned by its children.
#[lisp_fn(min = "0")]
pub fn window_pixel_height(window: LispWindowValidOrSelected) -> i32 {
    let win: LispWindowRef = window.into();
    win.pixel_height
}

/// Get width of marginal areas of window WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
///
/// Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
/// If a marginal area does not exist, its width will be returned
/// as nil.
#[lisp_fn(min = "0")]
pub fn window_margins(window: LispWindowLiveOrSelected) -> (LispObject, LispObject) {
    fn margin_as_object(margin: c_int) -> LispObject {
        if margin == 0 {
            Qnil
        } else {
            margin.into()
        }
    }
    let win: LispWindowRef = window.into();

    (
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

    window.combination_limit
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

    window.combination_limit = limit;

    limit
}

/// Return the window selected just before minibuffer window was selected.
/// Return nil if the selected window is not a minibuffer window.
#[lisp_fn]
pub fn minibuffer_selected_window() -> LispObject {
    let level = unsafe { minibuf_level };
    let current_minibuf = unsafe { current_minibuf_window };
    if level > 0
        && LispWindowRef::from(selected_window()).is_minibuffer()
        && current_minibuf.as_window().unwrap().is_live()
    {
        current_minibuf
    } else {
        Qnil
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
pub fn window_total_width(window: LispWindowValidOrSelected, round: LispObject) -> i32 {
    let win: LispWindowRef = window.into();
    win.total_width(round)
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
pub fn window_total_height(window: LispWindowValidOrSelected, round: LispObject) -> i32 {
    let win: LispWindowRef = window.into();
    win.total_height(round)
}

/// Return the parent window of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
/// Return nil for a window with no parent (e.g. a root window).
#[lisp_fn(min = "0")]
pub fn window_parent(window: LispWindowValidOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.parent
}

/// Return the frame that window WINDOW is on.
/// WINDOW is optional and defaults to the selected window. If provided it must
/// be a valid window.
#[lisp_fn(min = "0")]
pub fn window_frame(window: LispWindowValidOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.frame
}

/// Return the minibuffer window for frame FRAME.
/// If FRAME is omitted or nil, it defaults to the selected frame.
#[lisp_fn(min = "0")]
pub fn minibuffer_window(frame: LispFrameLiveOrSelected) -> LispObject {
    let frame: LispFrameRef = frame.into();
    frame.minibuffer_window
}

/// Return WINDOW's value for PARAMETER.
/// WINDOW can be any window and defaults to the selected one.
#[lisp_fn(name = "window-parameter", c_name = "window_parameter")]
pub fn window_parameter_lisp(window: LispWindowOrSelected, parameter: LispObject) -> LispObject {
    let win: LispWindowRef = window.into();
    win.get_parameter(parameter)
}

/// Return the display-table that WINDOW is using.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_display_table(window: LispWindowLiveOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.display_table
}

/// Set WINDOW's display-table to TABLE.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn]
pub fn set_window_display_table(window: LispWindowLiveOrSelected, table: LispObject) -> LispObject {
    let mut win: LispWindowRef = window.into();
    win.display_table = table;
    table
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
    window: LispWindowOrSelected,
    parameter: LispObject,
    value: LispObject,
) -> LispObject {
    let mut win: LispWindowRef = window.into();
    let old_alist_elt = assq(parameter, win.window_parameters);
    if old_alist_elt.is_nil() {
        win.window_parameters = ((parameter, value), win.window_parameters).into();
    } else {
        setcdr(old_alist_elt.into(), value);
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
) -> face_id {
    let current = if let Some(w) = selected_window().as_window() {
        w
    } else {
        LispWindowRef::new(ptr::null_mut())
    };

    unsafe {
        if !globals.mode_line_in_non_selected_windows || selw == current {
            return face_id::MODE_LINE_FACE_ID;
        } else if minibuf_level > 0 {
            if let Some(minibuf_window) = current_minibuf_window.as_window() {
                if mbw == minibuf_window && scrw == minibuf_window {
                    return face_id::MODE_LINE_FACE_ID;
                }
            }
        }

        face_id::MODE_LINE_INACTIVE_FACE_ID
    }
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
    frame: LispFrameOrSelected,
    minibuf: LispObject,
    window: Option<LispWindowRef>,
) -> LispObject {
    let w_obj = match window {
        Some(w) => w.into(),
        None => LispFrameRef::from(frame).selected_window,
    };

    let w_ref = w_obj
        .as_window()
        .unwrap_or_else(|| panic!("Invalid window reference."));

    let f_obj = LispObject::from(frame);

    if !f_obj.eq(w_ref.frame) {
        error!("Window is on a different frame");
    }

    unsafe { (window_list_1(w_obj, minibuf, f_obj)) }
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
#[lisp_fn(min = "0", name = "window-list-1", c_name = "window_list_1")]
pub fn window_list_1_lisp(
    window: LispObject,
    minibuf: LispObject,
    all_frames: LispObject,
) -> LispObject {
    unsafe { (window_list_1(window, minibuf, all_frames)) }
}

/// Return non-nil when WINDOW is dedicated to its buffer.
/// More precisely, return the value assigned by the last call of
/// `set-window-dedicated-p' for WINDOW.  Return nil if that function was
/// never called with WINDOW as its argument, or the value set by that
/// function was internally reset since its last call.  WINDOW must be a
/// live window and defaults to the selected one.
///
/// When a window is dedicated to its buffer, `display-buffer' will refrain
/// from displaying another buffer in it.  `get-lru-window' and
/// `get-largest-window' treat dedicated windows specially.
/// `delete-windows-on', `replace-buffer-in-windows', `quit-window' and
/// `kill-buffer' can delete a dedicated window and the containing frame.
///
/// Functions like `set-window-buffer' may change the buffer displayed by a
/// window, unless that window is "strongly" dedicated to its buffer, that
/// is the value returned by `window-dedicated-p' is t.
#[lisp_fn(min = "0")]
pub fn window_dedicated_p(window: LispWindowOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.dedicated
}

/// Mark WINDOW as dedicated according to FLAG.
/// WINDOW must be a live window and defaults to the selected one.  FLAG
/// non-nil means mark WINDOW as dedicated to its buffer.  FLAG nil means
/// mark WINDOW as non-dedicated.  Return FLAG.
///
/// When a window is dedicated to its buffer, `display-buffer' will refrain
/// from displaying another buffer in it.  `get-lru-window' and
/// `get-largest-window' treat dedicated windows specially.
/// `delete-windows-on', `replace-buffer-in-windows', `quit-window',
/// `quit-restore-window' and `kill-buffer' can delete a dedicated window
/// and the containing frame.
///
/// As a special case, if FLAG is t, mark WINDOW as "strongly" dedicated to
/// its buffer.  Functions like `set-window-buffer' may change the buffer
/// displayed by a window, unless that window is strongly dedicated to its
/// buffer.  If and when `set-window-buffer' displays another buffer in a
/// window, it also makes sure that the window is no more dedicated.
#[lisp_fn]
pub fn set_window_dedicated_p(window: LispWindowOrSelected, flag: LispObject) -> LispObject {
    let mut win: LispWindowRef = window.into();
    win.dedicated = flag;
    flag
}

/// Return old value of point in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_old_point(window: LispWindowLiveOrSelected) -> Option<EmacsInt> {
    let win: LispWindowRef = window.into();
    marker_position_lisp(win.old_pointm.into())
}

/// Return the use time of window WINDOW.
/// WINDOW must be a live window and defaults to the selected one. The
/// window with the highest use time is the most recently selected
/// one.  The window with the lowest use time is the least recently
/// selected one.
#[lisp_fn(min = "0")]
pub fn window_use_time(window: LispWindowLiveOrSelected) -> EmacsInt {
    let win: LispWindowRef = window.into();
    win.use_time
}

/// Return buffers previously shown in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_prev_buffers(window: LispWindowLiveOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.prev_buffers
}

/// Set WINDOW's previous buffers to PREV-BUFFERS.
/// WINDOW must be a live window and defaults to the selected one.
/// PREV-BUFFERS should be a list of elements (BUFFER WINDOW-START POS),
/// where BUFFER is a buffer, WINDOW-START is the start position of the
/// window for that buffer, and POS is a window-specific point value.
#[lisp_fn]
pub fn set_window_prev_buffers(
    window: LispWindowLiveOrSelected,
    prev_buffers: LispObject,
) -> LispObject {
    let mut win: LispWindowRef = window.into();
    win.prev_buffers = prev_buffers;
    prev_buffers
}

/// Return list of buffers recently re-shown in WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_next_buffers(window: LispWindowLiveOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.next_buffers
}

/// Return the next sibling window of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
/// Return nil if WINDOW has no next sibling.
#[lisp_fn(min = "0")]
pub fn window_next_sibling(window: LispWindowValidOrSelected) -> Option<LispWindowRef> {
    let win: LispWindowRef = window.into();
    win.next.as_window()
}

/// Return the previous sibling window of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
/// Return nil if WINDOW has no previous sibling.
#[lisp_fn(min = "0")]
pub fn window_prev_sibling(window: LispWindowValidOrSelected) -> Option<LispWindowRef> {
    let win: LispWindowRef = window.into();
    win.prev.as_window()
}

/// Set WINDOW's next buffers to NEXT-BUFFERS.
/// WINDOW must be a live window and defaults to the selected one.
/// NEXT-BUFFERS should be a list of buffers.
#[lisp_fn]
pub fn set_window_next_buffers(
    window: LispWindowLiveOrSelected,
    next_buffers: LispObject,
) -> LispObject {
    let mut win: LispWindowRef = window.into();
    win.next_buffers = next_buffers;
    next_buffers
}

/// Make point value in WINDOW be at position POS in WINDOW's buffer.
/// WINDOW must be a live window and defaults to the selected one.
/// Return POS.
#[lisp_fn]
pub fn set_window_point(window: LispWindowLiveOrSelected, pos: LispObject) -> LispObject {
    let mut win: LispWindowRef = window.into();

    // Type of POS is checked by Fgoto_char or set_marker_restricted ...
    if win == LispWindowRef::from(selected_window()) {
        let mut current_buffer = ThreadState::current_buffer_unchecked();

        if win
            .contents
            .as_buffer()
            .map_or(false, |b| b == current_buffer)
        {
            goto_char(pos);
        } else {
            // ... but here we want to catch type error before buffer change.
            pos.as_number_coerce_marker_or_error();
            unsafe {
                set_buffer_internal(win.contents_as_buffer().as_mut());
            }
            goto_char(pos);
            unsafe {
                set_buffer_internal(current_buffer.as_mut());
            }
        }
    } else {
        set_marker_restricted(win.pointm, pos, win.contents);
        // We have to make sure that redisplay updates the window to show
        // the new value of point.
        win.set_redisplay(true);
    }
    pos
}

/// Make display in WINDOW start at position POS in WINDOW's buffer.
/// WINDOW must be a live window and defaults to the selected one.  Return
/// POS.  Optional third arg NOFORCE non-nil inhibits next redisplay from
/// overriding motion of point in order to display at this exact start.
#[lisp_fn(min = "2")]
pub fn set_window_start(
    window: LispWindowLiveOrSelected,
    pos: LispObject,
    noforce: bool,
) -> LispObject {
    let mut win: LispWindowRef = window.into();
    set_marker_restricted(win.start, pos, win.contents);
    // This is not right, but much easier than doing what is right.
    win.set_start_at_line_beg(false);
    if !noforce {
        win.set_force_start(true);
    }

    wset_update_mode_line(win);

    // Bug#15957
    win.set_window_end_valid(false);
    unsafe { wset_redisplay(win.as_mut()) };
    pos
}

/// Return the topmost child window of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
/// Return nil if WINDOW is a live window (live windows have no children).
/// Return nil if WINDOW is an internal window whose children form a
/// horizontal combination.
#[lisp_fn(min = "0")]
pub fn window_top_child(window: LispWindowValidOrSelected) -> Option<LispWindowRef> {
    let win: LispWindowRef = window.into();
    if win.is_vertical_combination() {
        win.contents.as_window()
    } else {
        None
    }
}

fn scroll_horizontally(
    arg: Option<InteractiveNumericPrefix>,
    set_minimum: bool,
    left: bool,
) -> LispObject {
    let mut w: LispWindowRef = selected_window().into();
    let requested_arg = match arg {
        None => EmacsInt::from(w.body_width(false)) - 2,
        Some(value) => {
            let tem = value.unwrap();
            if left {
                tem
            } else {
                -tem
            }
        }
    };

    let result = unsafe { set_window_hscroll(w.as_mut(), w.hscroll as EmacsInt + requested_arg) };

    if set_minimum {
        w.min_hscroll = w.hscroll;
    }

    w.set_suspend_auto_hscroll(true);
    result
}

/// Scroll selected window display ARG columns left.
/// Default for ARG is window width minus 2.
/// Value is the total amount of leftward horizontal scrolling in
/// effect after the change.
/// If SET-MINIMUM is non-nil, the new scroll amount becomes the
/// lower bound for automatic scrolling, i.e. automatic scrolling
/// will not scroll a window to a column less than the value returned
/// by this function.  This happens in an interactive call.
#[lisp_fn(min = "0", intspec = "^P\np")]
pub fn scroll_left(arg: Option<InteractiveNumericPrefix>, set_minimum: bool) -> LispObject {
    scroll_horizontally(arg, set_minimum, true)
}

/// Scroll selected window display ARG columns left.
/// Default for ARG is window width minus 2.
/// Value is the total amount of leftward horizontal scrolling in
/// effect after the change.
/// If SET-MINIMUM is non-nil, the new scroll amount becomes the
/// lower bound for automatic scrolling, i.e. automatic scrolling
/// will not scroll a window to a column less than the value returned
/// by this function.  This happens in an interactive call.
#[lisp_fn(min = "0", intspec = "^P\np")]
pub fn scroll_right(arg: Option<InteractiveNumericPrefix>, set_minimum: bool) -> LispObject {
    scroll_horizontally(arg, set_minimum, false)
}

/// Scroll text of selected window upward ARG lines.
/// If ARG is omitted or nil, scroll upward by a near full screen.
/// A near full screen is `next-screen-context-lines' less than a full screen.
/// Negative ARG means scroll downward.
/// If ARG is the atom `-', scroll downward by nearly full screen.
/// When calling from a program, supply as argument a number, nil, or `-'.
#[lisp_fn(min = "0", intspec = "^P")]
pub fn scroll_up(arg: Option<InteractiveNumericPrefix>) {
    scroll_command(arg, 1);
}

/// Scroll text of selected window down ARG lines.
/// If ARG is omitted or nil, scroll down by a near full screen.
/// A near full screen is `next-screen-context-lines' less than a full screen.
/// Negative ARG means scroll upward.
/// If ARG is the atom `-', scroll upward by nearly full screen.
/// When calling from a program, supply as argument a number, nil, or `-'.
#[lisp_fn(min = "0", intspec = "^P")]
pub fn scroll_down(arg: Option<InteractiveNumericPrefix>) {
    scroll_command(arg, -1);
}

// Scroll selected window up or down.  If N is nil, scroll upward by a
// screen-full which is defined as the height of the window minus
// next_screen_context_lines.  If N is the symbol `-', scroll downward
// by a screen-full.  DIRECTION may be 1 meaning to scroll down, or -1
// meaning to scroll up.
fn scroll_command(n: Option<InteractiveNumericPrefix>, direction: EmacsInt) {
    let count = c_specpdl_index();

    assert!(direction.abs() == 1);

    let window: LispWindowRef = selected_window().into();
    let current_buffer = ThreadState::current_buffer_unchecked();
    let window_buffer = window.contents_as_buffer();
    // If selected window's buffer isn't current, make it current for
    // the moment.  But don't screw up if window_scroll gets an error.
    if window_buffer != current_buffer {
        unsafe {
            record_unwind_protect(Some(save_excursion_restore), save_excursion_save());
        }
        set_buffer(window_buffer.into());
    }

    let (direction, whole_screen) = match n {
        None => (direction, true),
        Some(prefix) => {
            if prefix.is_minus() {
                (-direction, true)
            } else {
                let value = prefix.unwrap();
                (value * direction, false)
            }
        }
    };
    unsafe {
        window_scroll(current_window, direction, whole_screen, false);
    }

    unbind_to(count, Qnil);
}

/// Return new normal size of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
///
/// The new normal size of WINDOW is the value set by the last call of
/// `set-window-new-normal' for WINDOW.  If valid, it will be shortly
/// installed as WINDOW's normal size (see `window-normal-size').
#[lisp_fn(min = "0")]
pub fn window_new_normal(window: LispWindowValidOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.new_normal
}

/// Return the new total size of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
///
/// The new total size of WINDOW is the value set by the last call of
/// `set-window-new-total' for WINDOW.  If it is valid, it will be shortly
/// installed as WINDOW's total height (see `window-total-height') or total
/// width (see `window-total-width').
#[lisp_fn(min = "0")]
pub fn window_new_total(window: LispWindowValidOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.new_total
}

/// Set new total size of WINDOW to SIZE.
/// WINDOW must be a valid window and defaults to the selected one.
/// Return SIZE.
///
/// Optional argument ADD non-nil means add SIZE to the new total size of
/// WINDOW and return the sum.
///
/// The new total size of WINDOW, if valid, will be shortly installed as
/// WINDOW's total height (see `window-total-height') or total width (see
/// `window-total-width').
///
/// Note: This function does not operate on any child windows of WINDOW.
#[lisp_fn(min = "2")]
pub fn set_window_new_total(
    window: LispWindowValidOrSelected,
    size: EmacsInt,
    add: bool,
) -> LispObject {
    let mut win: LispWindowRef = window.into();

    let new_total = if add {
        EmacsInt::from(win.new_total) + size
    } else {
        size
    };
    win.new_total = new_total.into();
    win.new_total
}

#[no_mangle]
pub extern "C" fn wset_update_mode_line(mut w: LispWindowRef) {
    // If this window is the selected window on its frame, set the
    // global variable update_mode_lines, so that x_consider_frame_title
    // will consider this frame's title for redisplay.
    let fselected_window = w.get_frame().selected_window;

    if let Some(win) = fselected_window.as_window() {
        if win == w {
            unsafe {
                update_mode_lines = 42;
            }
        }
    } else {
        w.set_update_mode_line(true);
    }
}

/// Return the number of columns by which WINDOW is scrolled from left margin.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_hscroll(window: LispWindowLiveOrSelected) -> EmacsInt {
    let win: LispWindowRef = window.into();
    win.hscroll as EmacsInt
}

#[no_mangle]
pub extern "C" fn window_parameter(w: LispWindowRef, parameter: LispObject) -> LispObject {
    w.get_parameter(parameter)
}

/// Select WINDOW which must be a live window.
/// Also make WINDOW's frame the selected frame and WINDOW that frame's
/// selected window.  In addition, make WINDOW's buffer current and set its
/// buffer's value of `point' to the value of WINDOW's `window-point'.
/// Return WINDOW.
///
/// Optional second arg NORECORD non-nil means do not put this buffer at the
/// front of the buffer list and do not make this window the most recently
/// selected one.  Also, do not mark WINDOW for redisplay unless NORECORD
/// equals the special symbol `mark-for-redisplay'.
///
/// Run `buffer-list-update-hook' unless NORECORD is non-nil.  Note that
/// applications and internal routines often select a window temporarily for
/// various purposes; mostly, to simplify coding.  As a rule, such
/// selections should be not recorded and therefore will not pollute
/// `buffer-list-update-hook'.  Selections that "really count" are those
/// causing a visible change in the next redisplay of WINDOW's frame and
/// should be always recorded.  So if you think of running a function each
/// time a window gets selected put it on `buffer-list-update-hook'.
///
/// Also note that the main editor command loop sets the current buffer to
/// the buffer of the selected window before each command.
#[lisp_fn(min = "1", name = "select-window", c_name = "select_window")]
pub fn select_window_lisp(window: LispObject, norecord: LispObject) -> LispObject {
    unsafe { select_window(window, norecord, false) }
}

/// Return top line of window WINDOW.
/// This is the distance, in lines, between the top of WINDOW and the top
/// of the frame's window area.  For instance, the return value is 0 if
/// there is no window above WINDOW.
///
/// WINDOW must be a valid window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_top_line(window: LispWindowValidOrSelected) -> EmacsInt {
    let win: LispWindowRef = window.into();
    EmacsInt::from(win.top_line)
}

/// Return the parameters of WINDOW and their values.
/// WINDOW must be a valid window and defaults to the selected one.  The
/// return value is a list of elements of the form (PARAMETER . VALUE).
#[lisp_fn(min = "0")]
pub fn window_parameters(window: LispWindowValidOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    unsafe { Fcopy_alist(win.window_parameters) }
}

/// Return WINDOW's redisplay end trigger value.
/// WINDOW must be a live window and defaults to the selected one.
/// See `set-window-redisplay-end-trigger' for more information.
#[lisp_fn(min = "0")]
pub fn window_redisplay_end_trigger(window: LispWindowLiveOrSelected) -> LispObject {
    let win: LispWindowRef = window.into();
    win.redisplay_end_trigger
}

/// Set WINDOW's redisplay end trigger value to VALUE.
/// WINDOW must be a live window and defaults to the selected one.  VALUE
/// should be a buffer position (typically a marker) or nil.  If it is a
/// buffer position, then if redisplay in WINDOW reaches a position beyond
/// VALUE, the functions in `redisplay-end-trigger-functions' are called
/// with two arguments: WINDOW, and the end trigger value.  Afterwards the
/// end-trigger value is reset to nil.
#[lisp_fn]
pub fn set_window_redisplay_end_trigger(
    window: LispWindowLiveOrSelected,
    value: LispObject,
) -> LispObject {
    let mut win: LispWindowRef = window.into();
    win.redisplay_end_trigger = value;
    value
}

/// Return the height of WINDOW's text area.
/// WINDOW must be a live window and defaults to the selected one.  Optional
/// argument PIXELWISE non-nil means return the height of WINDOW's text area
/// in pixels.  The return value does not include the mode line or header
/// line or any horizontal divider.
///
/// If PIXELWISE is nil, return the largest integer smaller than WINDOW's
/// pixel height divided by the character height of WINDOW's frame.  This
/// means that if a line at the bottom of the text area is only partially
/// visible, that line is not counted.
#[lisp_fn(min = "0")]
pub fn window_body_height(window: LispWindowLiveOrSelected, pixelwise: bool) -> EmacsInt {
    let window: LispWindowRef = window.into();
    window.body_height(pixelwise).into()
}

/// Return the width of WINDOW's text area.
/// WINDOW must be a live window and defaults to the selected one.  Optional
/// argument PIXELWISE non-nil means return the width in pixels.  The return
/// value does not include any vertical dividers, fringes or marginal areas,
/// or scroll bars.
///
/// If PIXELWISE is nil, return the largest integer smaller than WINDOW's
/// pixel width divided by the character width of WINDOW's frame.  This
/// means that if a column at the right of the text area is only partially
/// visible, that column is not counted.
///
/// Note that the returned value includes the column reserved for the
/// continuation glyph.
#[lisp_fn(name = "window-body-width", c_name = "window_body_width", min = "0")]
pub fn window_body_width_lisp(window: LispWindowLiveOrSelected, pixelwise: bool) -> EmacsInt {
    let window: LispWindowRef = window.into();
    window.body_width(pixelwise).into()
}

/// Return pixel dimensions of WINDOW's lines.
/// The return value is a list of the x- and y-coordinates of the lower
/// right corner of the last character of each line.  Return nil if the
/// current glyph matrix of WINDOW is not up-to-date.
///
/// Optional argument WINDOW specifies the window whose lines' dimensions
/// shall be returned.  Nil or omitted means to return the dimensions for
/// the selected window.
///
/// FIRST, if non-nil, specifies the index of the first line whose
/// dimensions shall be returned.  If FIRST is nil and BODY is non-nil,
/// start with the first text line of WINDOW.  Otherwise, start with the
/// first line of WINDOW.
///
/// LAST, if non-nil, specifies the last line whose dimensions shall be
/// returned.  If LAST is nil and BODY is non-nil, the last line is the last
/// line of the body (text area) of WINDOW.  Otherwise, last is the last
/// line of WINDOW.
///
/// INVERSE, if nil, means that the y-pixel value returned for a specific
/// line specifies the distance in pixels from the left edge (body edge if
/// BODY is non-nil) of WINDOW to the right edge of the last glyph of that
/// line.  INVERSE non-nil means that the y-pixel value returned for a
/// specific line specifies the distance in pixels from the right edge of
/// the last glyph of that line to the right edge (body edge if BODY is
/// non-nil) of WINDOW.
///
/// LEFT non-nil means to return the x- and y-coordinates of the lower left
/// corner of the leftmost character on each line.  This is the value that
/// should be used for buffers that mostly display text from right to left.
///
/// If LEFT is non-nil and INVERSE is nil, this means that the y-pixel value
/// returned for a specific line specifies the distance in pixels from the
/// left edge of the last (leftmost) glyph of that line to the right edge
/// (body edge if BODY is non-nil) of WINDOW.  If LEFT and INVERSE are both
/// non-nil, the y-pixel value returned for a specific line specifies the
/// distance in pixels from the left edge (body edge if BODY is non-nil) of
/// WINDOW to the left edge of the last (leftmost) glyph of that line.
///
/// Normally, the value of this function is not available while Emacs is
/// busy, for example, when processing a command.  It should be retrievable
/// though when run from an idle timer with a delay of zero seconds.
#[lisp_fn(min = "0")]
pub fn window_lines_pixel_dimensions(
    window: LispWindowLiveOrSelected,
    first: Option<LispNumber>,
    last: Option<LispNumber>,
    body: bool,
    inverse: bool,
    left: bool,
) -> LispObject {
    let window: LispWindowRef = window.into();
    let buffer: LispBufferRef = window.contents.into();
    let max_y = if body {
        window.text_bottom_y()
    } else {
        window.pixel_height
    };
    let window_width = if body {
        window.body_width(true)
    } else {
        window.pixel_width
    };
    let header_line_height = window.header_line_height();
    let subtract = if body { header_line_height } else { 0 };
    let matrix = window.get_matrix();

    if unsafe { noninteractive } || window.is_pseudo() {
        return Qnil;
    }

    // Fail if current matrix is not up to date.
    unsafe {
        if !window.window_end_valid()
            || windows_or_buffers_changed > 0
            || buffer.clip_changed()
            || buffer.prevent_redisplay_optimizations_p()
            || window.is_outdated()
        {
            return Qnil;
        }
    }

    let mut row = match first {
        Some(first) => matrix.row(first.to_fixnum() as usize),
        None => {
            if body {
                matrix.first_text_row()
            } else {
                matrix.row(0)
            }
        }
    };

    let end_row = match last {
        Some(last) => matrix.row(last.to_fixnum() as usize),
        None => {
            if body {
                matrix.bottom_text_row(window)
            } else {
                unsafe { matrix.row_unchecked(matrix.nrows as usize) }
            }
        }
    };

    let mut rows = Qnil;
    while row.as_ptr() <= end_row.as_ptr() && row.enabled_p() && row.y + row.height < max_y {
        #[allow(clippy::collapsible_if)] // The symmetry is worth ignoring this.
        let width = if left {
            let glyph = unsafe { &*row.glyphs[TEXT_AREA as usize] };
            if inverse {
                i32::from(glyph.pixel_width)
            } else {
                window_width - i32::from(glyph.pixel_width)
            }
        } else {
            if inverse {
                window_width - row.pixel_width
            } else {
                row.pixel_width
            }
        };
        rows = ((width, row.y + row.height - subtract), rows).into();
        unsafe { row.ptr_add(1) };
    }
    unsafe { Fnreverse(rows) }
}

/// Return top pixel edge of window WINDOW.
/// WINDOW must be a valid window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_pixel_top(window: LispWindowValidOrSelected) -> EmacsInt {
    let window: LispWindowRef = window.into();
    window.pixel_top.into()
}

/// Get width of fringes of window WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
#[lisp_fn(min = "0")]
pub fn window_fringes(window: LispWindowLiveOrSelected) -> LispObject {
    let window: LispWindowRef = window.into();

    list!(
        window.get_left_fringe_width(),
        window.get_right_fringe_width(),
        window.fringes_outside_margins()
    )
}

/// Set the fringe widths of window WINDOW.
/// WINDOW must be a live window and defaults to the selected one.
///
/// Second arg LEFT-WIDTH specifies the number of pixels to reserve for
/// the left fringe.  Optional third arg RIGHT-WIDTH specifies the right
/// fringe width.  If a fringe width arg is nil, that means to use the
/// frame's default fringe width.  Default fringe widths can be set with
/// the command `set-fringe-style'.
/// If optional fourth arg OUTSIDE-MARGINS is non-nil, draw the fringes
/// outside of the display margins.  By default, fringes are drawn between
/// display marginal areas and the text area.
///
/// Return t if any fringe was actually changed and nil otherwise.
#[lisp_fn(name = "set-window-fringes", min = "2")]
pub fn set_window_fringes_lisp(
    window: LispWindowLiveOrSelected,
    left_width: LispObject,
    right_width: LispObject,
    outside_margins: LispObject,
) -> bool {
    let mut window: LispWindowRef = window.into();

    let updated_window =
        unsafe { set_window_fringes(window.as_mut(), left_width, right_width, outside_margins) };

    if !updated_window.is_null() {
        unsafe { apply_window_adjustment(updated_window.into()) };
        true
    } else {
        false
    }
}

include!(concat!(env!("OUT_DIR"), "/windows_exports.rs"));
