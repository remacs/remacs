//! Functions operating on window configurations.

use remacs_macros::lisp_fn;

use crate::{
    frames::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    objects::equal,
    remacs_sys::Qwindow_configuration_p,
    remacs_sys::{save_window_data, saved_window},
    vectors::LispVectorlikeRef,
    windows::LispWindowRef,
};

pub type SaveWindowDataRef = ExternalPtr<save_window_data>;
pub type SavedWindowRef = ExternalPtr<saved_window>;

impl SaveWindowDataRef {
    pub fn equal(self, other: Self, ignore_positions: bool) -> bool {
        // Frame settings must match.
        if !(self.frame_cols == other.frame_cols
            && self.frame_lines == other.frame_lines
            && self.frame_menu_bar_lines == other.frame_menu_bar_lines
            && self.selected_frame.eq(other.selected_frame)
            && self.f_current_buffer.eq(other.f_current_buffer))
        {
            return false;
        }

        if !(ignore_positions
            || (self.minibuf_scroll_window.eq(other.minibuf_scroll_window)
                && self
                    .minibuf_selected_window
                    .eq(other.minibuf_selected_window)))
        {
            return false;
        }

        if !self.focus_frame.eq(other.focus_frame) {
            return false;
        }

        true
    }
}

impl SavedWindowRef {
    pub fn equal(self, other: Self, ignore_positions: bool) -> bool {
        // Windows' buffers must match.
        if !self.buffer.eq(other.buffer) {
            return false;
        }

        if !(self.pixel_left.eq(other.pixel_left)
            && self.pixel_top.eq(other.pixel_top)
            && self.pixel_height.eq(other.pixel_height)
            && self.pixel_width.eq(other.pixel_width)
            && self.left_col.eq(other.left_col)
            && self.top_line.eq(other.top_line)
            && self.total_cols.eq(other.total_cols)
            && self.total_lines.eq(other.total_lines)
            && self.display_table.eq(other.display_table))
        {
            return false;
        }

        // The next two check the window structure for equality.
        if !(self.parent.eq(other.parent) && self.prev.eq(other.prev)) {
            return false;
        }

        if !(ignore_positions
            || (self.hscroll.eq(other.hscroll)
                && self.min_hscroll.eq(other.min_hscroll)
                && self.start_at_line_beg.eq(other.start_at_line_beg)
                && equal(self.start, other.start))
                && equal(self.pointm, other.pointm))
        {
            return false;
        }

        if !(self.left_margin_cols.eq(other.left_margin_cols)
            && self.right_margin_cols.eq(other.right_margin_cols)
            && self.left_fringe_width.eq(other.left_fringe_width)
            && self.right_fringe_width.eq(other.right_fringe_width)
            && self
                .fringes_outside_margins
                .eq(other.fringes_outside_margins)
            && self.scroll_bar_width.eq(other.scroll_bar_width)
            && self.scroll_bar_height.eq(other.scroll_bar_height)
            && self
                .vertical_scroll_bar_type
                .eq(other.vertical_scroll_bar_type)
            && self
                .horizontal_scroll_bar_type
                .eq(other.horizontal_scroll_bar_type))
        {
            return false;
        }

        true
    }
}

impl From<LispObject> for SaveWindowDataRef {
    fn from(o: LispObject) -> Self {
        o.as_window_configuration()
            .unwrap_or_else(|| wrong_type!(Qwindow_configuration_p, o))
    }
}

impl LispObject {
    pub fn as_window_configuration(self) -> Option<SaveWindowDataRef> {
        self.as_vectorlike()
            .and_then(LispVectorlikeRef::as_window_configuration)
    }
}

/// Return t if OBJECT is a window-configuration object.
#[lisp_fn]
pub fn window_configuration_p(object: LispObject) -> bool {
    object.as_window_configuration().is_some()
}

/// Return the frame that CONFIG, a window-configuration object, is about.
#[lisp_fn]
pub fn window_configuration_frame(config: SaveWindowDataRef) -> LispFrameRef {
    let saved_windows = config.saved_windows.as_vector().unwrap();
    let obj = saved_windows.get(0);
    let saved = SavedWindowRef::new(obj.as_vector().unwrap().as_mut() as *mut saved_window);
    LispWindowRef::from(saved.window).frame.into()
}

// Return true if window configurations CONFIGURATION1 and CONFIGURATION2
// describe the same state of affairs.  This is used by Fequal.
//
// IGNORE_POSITIONS means ignore non-matching scroll positions
// and the like.
//
// This ignores a couple of things like the dedication status of
// window, combination_limit and the like.  This might have to be
// fixed.
#[no_mangle]
pub extern "C" fn compare_window_configurations(
    configuration1: LispObject,
    configuration2: LispObject,
    ignore_positions: bool,
) -> bool {
    compare_window_configurations_rust(
        configuration1.into(),
        configuration2.into(),
        ignore_positions,
    )
}

pub fn compare_window_configurations_rust(
    conf1: SaveWindowDataRef,
    conf2: SaveWindowDataRef,
    ignore_positions: bool,
) -> bool {
    if !conf1.equal(conf2, ignore_positions) {
        return false;
    }

    let sws1 = conf1.saved_windows.as_vector_or_error();
    let sws2 = conf2.saved_windows.as_vector_or_error();

    // Verify that the two configurations have the same number of windows.
    if sws1.len() != sws2.len() {
        return false;
    }

    for i in 0..sws1.len() {
        let obj1 = sws1.get(i as usize);
        let obj2 = sws2.get(i as usize);
        let sw1 = SavedWindowRef::new(obj1.as_vector().unwrap().as_mut() as *mut saved_window);
        let sw2 = SavedWindowRef::new(obj2.as_vector().unwrap().as_mut() as *mut saved_window);

        // The "current" windows in the two configurations must
        // correspond to each other.
        if conf1.current_window.eq(sw1.window) != conf2.current_window.eq(sw2.window) {
            return false;
        }
        if !sw1.equal(sw2, ignore_positions) {
            return false;
        }
    }

    true
}

/// Compare two window configurations as regards the structure of windows.
/// This function ignores details such as the values of point
/// and scrolling positions.
#[lisp_fn(
    name = "compare-window-configurations",
    c_name = "compare_window_configurations"
)]
pub fn compare_window_configurations_lisp(x: SaveWindowDataRef, y: SaveWindowDataRef) -> bool {
    compare_window_configurations_rust(x, y, true)
}

include!(concat!(env!("OUT_DIR"), "/window_configuration_exports.rs"));
