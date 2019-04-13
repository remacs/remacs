//! Profiler implementation.
use std::ffi::CString;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::{error, globals, Qnil},
    remacs_sys::{make_log, memory_log, profiler_memory_running},
};

/// Return non-nil if memory profiler is running.
#[lisp_fn]
pub fn profiler_memory_running_p() -> bool {
    unsafe { profiler_memory_running }
}

/// Start/restart the memory profiler.
/// The memory profiler will take samples of the call-stack whenever a new allocation takes
/// place. Note that most small allocations only trigger the profiler occasionally.
/// See also `profiler-log-size' and `profiler-max-stack-depth'.
#[lisp_fn]
pub fn profiler_memory_start() -> bool {
    unsafe {
        if profiler_memory_running {
            let error_str = CString::new("Memory profiler is already running").unwrap();
            error(error_str.as_ptr() as *const u8);
        }

        if memory_log.is_nil() {
            memory_log = make_log(globals.profiler_log_size, globals.profiler_max_stack_depth)
        }

        profiler_memory_running = true;
    }

    true
}

/// Stop the memory profiler.  The profiler log is not affected.
/// Return non-nil if the profiler was running.
#[lisp_fn]
pub fn profiler_memory_stop() -> bool {
    unsafe {
        if !profiler_memory_running {
            return false;
        }

        profiler_memory_running = false;

        true
    }
}

/// Return the current memory profiler log.
/// The log is a hash-table mapping backtraces to counters which represent the amount of memory
/// allocated at those points. Every backtrace is a vector of functions, where the last few
/// elements may be nil.
/// Before returning, a new log is allocated for future samples.
#[lisp_fn]
pub fn profiler_memory_log() -> LispObject {
    unsafe {
        let previous = memory_log;

        // Here we're making the log visible to Elisp, so it's not safe anymore
        // for our use afterwards since we can't rely on its special
        // pre-allocated keys anymore. So we have to allocate a new one.
        memory_log = if profiler_memory_running {
            make_log(globals.profiler_log_size, globals.profiler_max_stack_depth)
        } else {
            Qnil
        };

        previous
    }
}

include!(concat!(env!("OUT_DIR"), "/profiler_exports.rs"));
