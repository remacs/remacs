//! Updating of data structures for redisplay.

use std::{cmp, ptr};

use remacs_macros::lisp_fn;
use remacs_sys::{current_timespec, dtotimespec, timespec_add, timespec_sub,
                 wait_reading_process_output};
use remacs_sys::WAIT_READING_MAX;

use floatfns::extract_float;
use lisp::LispObject;
use lisp::defsubr;
use std::{cmp, ptr};

/// Pause, without updating display, for SECONDS seconds.
/// SECONDS may be a floating-point value, meaning that you can wait for a
/// fraction of a second.  Optional second arg MILLISECONDS specifies an
/// additional wait period, in milliseconds; this is for backwards compatibility.
/// (Not all operating systems support waiting for a fraction of a second.)
#[lisp_fn(min = "1")]
fn sleep_for(seconds: LispObject, milliseconds: LispObject) -> LispObject {
    let mut duration = extract_float(seconds.to_raw());
    if milliseconds.is_not_nil() {
        let milliseconds = milliseconds.as_fixnum_or_error() as f64;
        duration += milliseconds / 1000.0;
    }
    if duration > 0.0 {
        let mut t = unsafe { dtotimespec(duration) };
        let tend = unsafe { timespec_add(current_timespec(), t) };
        while !t.tv_sec < 0 && (t.tv_sec > 0 || t.tv_nsec > 0) {
            unsafe {
                wait_reading_process_output(
                    cmp::min(t.tv_sec, WAIT_READING_MAX),
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

pub fn rust_init_syms() {
    unsafe {
        defsubr(&*Ssleep_for);
    }
}
