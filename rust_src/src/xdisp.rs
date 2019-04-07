//! Display generation from window structure and buffer text.

// Remove the feature check when this file gains functions
// that are not guarded by it.
#[cfg(feature = "glyph-debug")]
use remacs_macros::lisp_fn;

#[cfg(feature = "glyph-debug")]
use crate::{interactive::InteractiveNumericPrefix, remacs_sys::trace_redisplay_p};

/// Toggle tracing of redisplay.
/// With ARG, turn tracing on if and only if ARG is positive.
#[cfg(feature = "glyph-debug")]
#[lisp_fn(min = "0", intspec = "P")]
pub fn trace_redisplay(arg: Option<InteractiveNumericPrefix>) {
    unsafe {
        match arg {
            None => trace_redisplay_p = !trace_redisplay_p,
            Some(n) => trace_redisplay_p = n.unwrap() > 0,
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/xdisp_exports.rs"));
