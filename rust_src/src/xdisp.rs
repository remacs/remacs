//! Display generation from window structure and buffer text.

use remacs_macros::lisp_fn;

#[cfg(feature = "glyph-debug")]
use crate::{interactive::InteractiveNumericPrefix, remacs_sys::trace_redisplay_p};
use crate::{
    intervals::text_prop_means_invisible,
    lisp::LispObject,
    lists::{LispCons, LispConsCircularChecks, LispConsEndChecks},
    remacs_sys::bset_update_mode_line,
    remacs_sys::{EmacsInt, Fget_char_property, Qinvisible, Qnil, Qt},
    threads::ThreadState,
};

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

pub fn invisible_prop(propval: LispObject, list: LispCons) -> EmacsInt {
    let find_prop = |propelt: LispObject| {
        for tem in list.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
            if propelt.eq(tem) {
                return Some(1);
            }
            if tem.is_cons() {
                let (car, cdr) = tem.into();
                if propelt.eq(car) {
                    return Some(if !cdr { 1 } else { 2 });
                }
            }
        }
        None
    };

    if let Some(val) = find_prop(propval) {
        return val;
    }

    if propval.is_cons() {
        for propelt in propval.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
            if let Some(val) = find_prop(propelt) {
                return val;
            }
        }
    }

    0
}

/// Non-nil if text properties at POS cause text there to be currently invisible.
/// POS should be a marker or a buffer position; the value of the `invisible'
/// property at that position in the current buffer is examined.
/// POS can also be the actual value of the `invisible' text or overlay
/// property of the text of interest, in which case the value itself is
/// examined.
///
/// The non-nil value returned can be t for currently invisible text that is
/// entirely hidden on display, or some other non-nil, non-t value if the
/// text is replaced by an ellipsis.
///
/// Note that whether text with `invisible' property is actually hidden on
/// display may depend on `buffer-invisibility-spec', which see.
#[lisp_fn]
pub fn invisible_p(pos: LispObject) -> LispObject {
    let prop = if pos.is_natnum() || pos.is_marker() {
        unsafe { Fget_char_property(pos, Qinvisible, Qnil) }
    } else {
        pos
    };
    let invis = text_prop_means_invisible(prop);
    match invis {
        0 => Qnil,
        1 => Qt,
        _ => LispObject::from(invis),
    }
}

include!(concat!(env!("OUT_DIR"), "/xdisp_exports.rs"));
