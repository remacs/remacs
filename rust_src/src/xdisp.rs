//! Display generation from window structure and buffer text.

use remacs_macros::lisp_fn;

use crate::lisp::LispObject;
use crate::threads::ThreadState;
#[cfg(feature = "glyph-debug")]
use crate::{interactive::InteractiveNumericPrefix, remacs_sys::trace_redisplay_p};
use crate::{
    lists::{LispCons, LispConsCircularChecks, LispConsEndChecks},
    remacs_sys::bset_update_mode_line,
    remacs_sys::{EmacsInt, Fget_char_property, Qinvisible, Qnil, Qt},
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

fn invisible_prop(propval: LispObject, list: LispCons) -> EmacsInt {
    for tem in list.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
        if propval.eq(tem) {
            return 1;
        }
        if tem.is_cons() && propval.eq(tem.as_cons().unwrap().car()) {
            if tem.as_cons().unwrap().cdr().is_nil() {
                return 1;
            } else {
                return 2;
            }
        }
    }

    if propval.is_cons() {
        for propelt in propval.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
            for tem in list.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
                if propelt.eq(tem) {
                    return 1;
                }
                if tem.is_cons() && propelt.eq(tem.as_cons().unwrap().car()) {
                    if tem.as_cons().unwrap().cdr().is_nil() {
                        return 1;
                    } else {
                        return 2;
                    }
                }
            }
        }
    }

    return 0;
}

// originally the macro TEXT_PROP_MEANS_INVISIBLE from intervals.h
fn text_prop_means_invisible(prop: LispObject) -> EmacsInt {
    let cur_buf = ThreadState::current_buffer_unchecked();
    if cur_buf.invisibility_spec_.is_t() {
        if prop.is_nil() {
            0
        } else {
            1
        }
    } else {
        match cur_buf.invisibility_spec_.as_cons() {
            Some(cons) => invisible_prop(prop, cons),
            None => 0,
        }
    }
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
