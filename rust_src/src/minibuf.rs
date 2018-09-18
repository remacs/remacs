//! Minibuffer input and completion.

use remacs_macros::lisp_fn;
use remacs_sys::{make_buffer_string, minibuf_level, minibuf_prompt, minibuf_window, EmacsInt,
                 Fcopy_sequence};
use remacs_sys::{Qfield, Vminibuffer_list};

use buffers::{current_buffer, get_buffer};
use editfns::field_end;
use lisp::defsubr;
use lisp::LispObject;
use lisp::LispObject;
use lists::memq;
use textprop::get_char_property;
use threads::ThreadState;

/// Return t if BUFFER is a minibuffer.
/// No argument or nil as argument means use current buffer as BUFFER.
/// BUFFER can be a buffer or a buffer name.
#[lisp_fn(min = "0")]
pub fn minibufferp(object: LispObject) -> bool {
    let buffer = if object.is_nil() {
        current_buffer()
    } else if object.is_string() {
        get_buffer(object)
    } else {
        object.as_buffer_or_error();
        object
    };
    memq(buffer, unsafe { Vminibuffer_list }).is_not_nil()
}

/// Return the currently active minibuffer window, or nil if none.
#[lisp_fn]
pub fn active_minibuffer_window() -> LispObject {
    unsafe {
        if minibuf_level == 0 {
            LispObject::constant_nil()
        } else {
            minibuf_window
        }
    }
}

/// Specify which minibuffer window to use for the minibuffer.
/// This affects where the minibuffer is displayed if you put text in it
/// without invoking the usual minibuffer commands.
#[lisp_fn]
pub fn set_minibuffer_window(window: LispObject) -> LispObject {
    window.as_minibuffer_or_error(); // just for the checks

    unsafe {
        minibuf_window = window;
    }

    window
}

/// Return current depth of activations of minibuffer,
/// a nonnegative integer.
#[lisp_fn]
pub fn minibuffer_depth() -> EmacsInt {
    unsafe { minibuf_level }
}

/// Return the prompt string of the currently active
/// minibuffer. If no minibuffer is active return nil.
#[lisp_fn]
pub fn minibuffer_prompt() -> LispObject {
    unsafe { Fcopy_sequence(minibuf_prompt) }
}

/// Return the buffer position of the end of the minibuffer prompt.
/// Return (point-min) if current buffer is not a minibuffer.
#[lisp_fn]
pub fn minibuffer_prompt_end() -> EmacsInt {
    let beg = ThreadState::current_buffer().beg() as EmacsInt;
    if memq(current_buffer(), unsafe { Vminibuffer_list }).is_nil() {
        return beg;
    }

    let end = field_end(Some(beg), false, None);
    let buffer_end = ThreadState::current_buffer().zv as EmacsInt;
    if end == buffer_end && get_char_property(beg, Qfield, LispObject::constant_nil()).is_nil() {
        beg
    } else {
        end
    }
}

/// Return the user input in a minibuffer as a string.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_contents() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    unsafe { make_buffer_string(prompt_end, ThreadState::current_buffer().zv, true) }
}

/// Return the user input in a minibuffer as a string, without text-properties.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_contents_no_properties() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    unsafe { make_buffer_string(prompt_end, ThreadState::current_buffer().zv, false) }
}

/// Return the user input in a minibuffer before point as a string.
/// That is what completion commands operate on.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_completion_contents() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    let pt = ThreadState::current_buffer().pt;
    if pt < prompt_end {
        error!("Cannot do completion in the prompt");
    }
    unsafe { make_buffer_string(prompt_end, pt, true) }
}

include!(concat!(env!("OUT_DIR"), "/minibuf_exports.rs"));
