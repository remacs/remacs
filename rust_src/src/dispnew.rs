//! Updating of data structures for redisplay.

use std::{cmp, ptr};

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, EmacsDouble, current_timespec, dtotimespec, timespec_add, timespec_sub,
                 wait_reading_process_output};
use remacs_sys::WAIT_READING_MAX;

use lisp::LispObject;
use lisp::defsubr;

/// Pause, without updating display, for SECONDS seconds.
/// SECONDS may be a floating-point value, meaning that you can wait for a
/// fraction of a second.  Optional second arg MILLISECONDS specifies an
/// additional wait period, in milliseconds; this is for backwards compatibility.
/// (Not all operating systems support waiting for a fraction of a second.)
#[lisp_fn(min = "1")]
pub fn sleep_for(seconds: EmacsDouble, milliseconds: Option<EmacsInt>) -> LispObject {
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
                    LispObject::constant_nil().to_raw(),
                    ptr::null(),
                    0,
                )
            };
            t = unsafe { timespec_sub(tend, current_timespec()) };
        }
    }
    LispObject::constant_nil()
}

include!(concat!(env!("OUT_DIR"), "/dispnew_exports.rs"));
