//! Updating of data structures for redisplay.

use std::{cmp, ptr};

use remacs_lib::current_timespec;
use remacs_macros::lisp_fn;

use crate::{
    eval::unbind_to,
    frames::selected_frame,
    frames::{LispFrameLiveOrSelected, LispFrameRef},
    lisp::{ExternalPtr, LispObject},
    lists::{LispConsCircularChecks, LispConsEndChecks},
    remacs_sys::{
        clear_current_matrices, detect_input_pending_run_timers, dtotimespec, fset_redisplay,
        mark_window_display_accurate, putchar_unlocked, redisplay_preserve_echo_area, ring_bell,
        specbind, swallow_events, timespec_add, timespec_sub, wait_reading_process_output,
    },
    remacs_sys::{
        globals, noninteractive, redisplaying_p, Qnil, Qredisplay_dont_pause, Qt, Vframe_list,
        WAIT_READING_MAX,
    },
    remacs_sys::{EmacsDouble, EmacsInt, Lisp_Glyph},
    terminal::{clear_frame, update_begin, update_end},
    threads::c_specpdl_index,
    windows::{LispWindowOrSelected, LispWindowRef},
};

pub type LispGlyphRef = ExternalPtr<Lisp_Glyph>;

/// Pause, without updating display, for SECONDS seconds.
/// SECONDS may be a floating-point value, meaning that you can wait for a
/// fraction of a second.  Optional second arg MILLISECONDS specifies an
/// additional wait period, in milliseconds; this is for backwards compatibility.
/// (Not all operating systems support waiting for a fraction of a second.)
#[lisp_fn(min = "1")]
pub fn sleep_for(seconds: EmacsDouble, milliseconds: Option<EmacsInt>) {
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

/// Redraw frame FRAME.
#[no_mangle]
pub extern "C" fn redraw_frame(mut frame: LispFrameRef) {
    unsafe {
        // Error if FRAME has no glyphs.
        debug_assert!(frame.glyphs_initialized_p());
        update_begin(frame);
        clear_frame(frame);
        clear_current_matrices(frame.as_mut());
        update_end(frame);
        fset_redisplay(frame.as_mut());
        // Mark all windows as inaccurate, so that every window will have
        // its redisplay done.
        mark_window_display_accurate(frame.root_window, false);
        set_window_update_flags(frame.root_window.into(), true);
        frame.set_garbaged(false);
    }
}

/// Clear frame FRAME and output again what is supposed to appear on it.
/// If FRAME is omitted or nil, the selected frame is used.
#[lisp_fn(c_name = "redraw_frame", name = "redraw-frame", min = "0")]
pub fn redraw_frame_lisp(frame: LispFrameLiveOrSelected) {
    redraw_frame(frame.into());
}

/// Clear and redisplay all visible frames.
#[lisp_fn]
pub fn redraw_display() {
    for_each_frame!(frame => {
        if frame.visible() != 0 {
            redraw_frame(frame);
        }
    });
}

/// Set WINDOW->must_be_updated_p to ON_P for all windows in
/// the window tree rooted at W.
// Make private once all C usages are ported in this file
#[no_mangle]
pub extern "C" fn set_window_update_flags(w: LispWindowRef, on_p: bool) {
    let mut w = Some(w);
    while let Some(mut win) = w {
        if let Some(contents) = win.contents.as_window() {
            set_window_update_flags(contents, on_p);
        } else {
            win.set_must_be_updated_p(on_p);
        }

        let next = win.next;
        w = if next.is_nil() {
            None
        } else {
            Some(next.into())
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
pub fn internal_show_cursor(window: LispWindowOrSelected, show: bool) {
    let mut win: LispWindowRef = window.into();
    // Don't change cursor state while redisplaying.  This could confuse
    // output routines.
    if !unsafe { redisplaying_p } {
        win.set_cursor_off_p(!show)
    }
}

/// Value is non-nil if next redisplay will display a cursor in WINDOW.
/// WINDOW nil or omitted means report on the selected window.
#[lisp_fn(min = "0")]
pub fn internal_show_cursor_p(window: LispWindowOrSelected) -> bool {
    let win: LispWindowRef = window.into();
    !win.cursor_off_p()
}

/// Return whether input is coming from the keyboard.
// Corresponds to the INTERACTIVE macro in commands.h.
pub fn is_interactive() -> bool {
    unsafe { globals.Vexecuting_kbd_macro.is_nil() && !noninteractive }
}

#[no_mangle]
pub extern "C" fn ding_internal(terminate_macro: bool) {
    unsafe {
        if noninteractive {
            putchar_unlocked(0o7);
        } else if terminate_macro && !is_interactive() {
            // Stop executing a keyboard macro.
            user_error!("Keyboard macro terminated by a command ringing the bell");
        } else {
            ring_bell(selected_frame().as_mut())
        }
    }
}

/// Beep, or flash the screen.
/// Also, unless an argument is given,
/// terminate any keyboard macro currently executing.
#[lisp_fn(min = "0")]
pub fn ding(arg: LispObject) {
    ding_internal(arg.is_nil())
}

/// Perform redisplay.
/// Optional arg FORCE, if non-nil, prevents redisplay from being
/// preempted by arriving input, even if `redisplay-dont-pause' is nil.
/// If `redisplay-dont-pause' is non-nil (the default), redisplay is never
/// preempted by arriving input, so FORCE does nothing.
///
/// Return t if redisplay was performed, nil if redisplay was preempted
/// immediately by pending input.
#[lisp_fn(min = "0")]
pub fn redisplay(force: LispObject) -> bool {
    let force: bool = force.is_not_nil();

    unsafe {
        swallow_events(true);

        let ret =
            (detect_input_pending_run_timers(true) && !force && !globals.redisplay_dont_pause)
                || globals.Vexecuting_kbd_macro.is_not_nil();

        if ret {
            let count = c_specpdl_index();

            if force && !globals.redisplay_dont_pause {
                specbind(Qredisplay_dont_pause, Qt);
            }

            redisplay_preserve_echo_area(2);

            unbind_to(count, Qnil);
        }

        ret
    }
}

include!(concat!(env!("OUT_DIR"), "/dispnew_exports.rs"));
