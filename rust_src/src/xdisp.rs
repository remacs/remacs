//! Display generation from window structure and buffer text.

use remacs_macros::lisp_fn;

use crate::remacs_sys::{trace_redisplay_p, EmacsInt};

/// Toggle tracing of redisplay.
/// With ARG, turn tracing on if and only if ARG is positive.
#[cfg(feature = "glyph-debug")]
#[lisp_fn(min = "0", intspec = "P")]
pub fn trace_redisplay(arg: Option<EmacsInt>) {
    unsafe {
        match arg {
            None => trace_redisplay_p = !trace_redisplay_p,
            Some(n) if n > 0 => trace_redisplay_p = true,
            _ => trace_redisplay_p = false,
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/xdisp_exports.rs"));
