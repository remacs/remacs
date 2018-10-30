//! Updating of data structures for redisplay.

use std::{cmp, ptr};

use remacs_lib::current_timespec;
use remacs_macros::lisp_fn;
use remacs_sys::{clear_current_matrices, dtotimespec, fset_redisplay,
                 mark_window_display_accurate, timespec_add, timespec_sub,
                 wait_reading_process_output};
use remacs_sys::{redisplaying_p, Qnil, Vframe_list, WAIT_READING_MAX};
use remacs_sys::{EmacsDouble, EmacsInt, Lisp_Glyph};

use frames::{frame_live_or_selected, LispFrameRef};
use lisp::{defsubr, ExternalPtr, LispObject};
use terminal::{clear_frame, update_begin, update_end};
use windows::{window_or_selected, LispWindowRef};

pub type LispGlyphRef = ExternalPtr<Lisp_Glyph>;

/// Pause, without updating display, for SECONDS seconds.
/// SECONDS may be a floating-point value, meaning that you can wait for a
/// fraction of a second.  Optional second arg MILLISECONDS specifies an
/// additional wait period, in milliseconds; this is for backwards compatibility.
/// (Not all operating systems support waiting for a fraction of a second.)
#[lisp_fn(min = "1")]
pub fn sleep_for(seconds: EmacsDouble, milliseconds: Option<EmacsInt>) -> () {
    let duration = seconds + (milliseconds.unwrap_or(0) as f64 / 1000.0);
    if duration > 0.0 {
        let mut t = unsafe { dtotimespec(duration) };
        let tend = unsafe { timespec_add(current_timespec(), t) };
        while !t.tv_sec < 0 && (t.tv_sec > 0 || t.tv_nsec > 0) {
            unsafe {
                wait_reading_process_output(
                    cmp::min(t.tv_sec as i64, WAIT_READING_MAX),
                    t.tv_nsec as i32,
                    0,
                    true,
                    Qnil,
                    ptr::null_mut(),
                    0,
                )
            };
            t = unsafe { timespec_sub(tend, current_timespec()) };
        }
    }
}

/**********************************************************************
			    Redrawing Frames
 **********************************************************************/

/// Redraw frame F.
#[no_mangle]
pub unsafe extern "C" fn redraw_frame(mut f: LispFrameRef) {
    // Error if F has no glyphs.
    debug_assert!(f.glyphs_initialized_p());
    update_begin(f);
    clear_frame(f);
    clear_current_matrices(f.as_mut());
    update_end(f);
    fset_redisplay(f.as_mut());
    // Mark all windows as inaccurate, so that every window will have
    // its redisplay done.
    mark_window_display_accurate(f.root_window, false);
    set_window_update_flags(f.root_window.as_window_or_error(), true);
    f.set_garbaged(false);
}

/// Clear frame FRAME and output again what is supposed to appear on it.
/// If FRAME is omitted or nil, the selected frame is used.
#[lisp_fn(c_name = "redraw_frame", name = "redraw-frame", min = "0")]
pub fn redraw_frame_lisp(frame: LispObject) {
    unsafe { redraw_frame(frame_live_or_selected(frame)) };
}

/// Clear and redisplay all visible frames.
#[lisp_fn]
pub fn redraw_display() {
    unsafe { Vframe_list }
        .iter_cars()
        .map(|f| f.as_frame_or_error())
        .filter(|f| f.visible() != 0)
        .for_each(|f| unsafe { redraw_frame(f) })
}

/// Set WINDOW->must_be_updated_p to ON_P for all windows in
/// the window tree rooted at W.
// Make private once all C usages are ported in this file
#[no_mangle]
pub extern "C" fn set_window_update_flags(w: LispWindowRef, on_p: bool) {
    let mut w = Some(w);
    while let Some(mut win) = w {
        if let Some(mut contents) = win.contents.as_window() {
            set_window_update_flags(contents, on_p);
        } else {
            win.set_must_be_updated_p(on_p);
        }

        let next = win.next;
        w = if next.is_nil() {
            None
        } else {
            Some(next.as_window_or_error())
        };
    }
}

/***********************************************************************
			   Blinking cursor
 ***********************************************************************/

/// Set the cursor-visibility flag of WINDOW to SHOW.
/// WINDOW nil means use the selected window.  SHOW non-nil means
/// show a cursor in WINDOW in the next redisplay.  SHOW nil means
/// don't show a cursor.
#[lisp_fn]
pub fn internal_show_cursor(window: LispObject, show: LispObject) {
    // Don't change cursor state while redisplaying.  This could confuse
    // output routines.
    if !unsafe { redisplaying_p } {
        window_or_selected(window).set_cursor_off_p(show.is_nil())
    }
}

/// Value is non-nil if next redisplay will display a cursor in WINDOW.
/// WINDOW nil or omitted means report on the selected window.
#[lisp_fn]
pub fn internal_show_cursor_p(window: LispObject) -> bool {
    !window_or_selected(window).cursor_off_p()
}

include!(concat!(env!("OUT_DIR"), "/dispnew_exports.rs"));
