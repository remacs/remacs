//! Display generation from window structure and buffer text.

use remacs_macros::lisp_fn;

use crate::lisp::LispObject;
use crate::remacs_sys::bset_update_mode_line;
use crate::threads::ThreadState;
#[cfg(feature = "glyph-debug")]
use crate::{interactive::InteractiveNumericPrefix, remacs_sys::trace_redisplay_p};

/// Mark the current buffer for redisplay.
/// This function may be passed to `add-variable-watcher'.
#[lisp_fn]
pub fn set_buffer_redisplay(
    _symbol: LispObject,
    _newval: LispObject,
    _op: LispObject,
    _where: LispObject,
) {
    let mut current_buffer = ThreadState::current_buffer_unchecked();
    unsafe {
        bset_update_mode_line(current_buffer.as_mut());
    }
    current_buffer.set_prevent_redisplay_optimizations_p(true);
}

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
