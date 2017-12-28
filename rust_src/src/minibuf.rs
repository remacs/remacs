//! Minibuffer input and completion.

use remacs_macros::lisp_fn;
use remacs_sys::{minibuf_level, minibuf_window};
use remacs_sys::Vminibuffer_list;

use buffers::{current_buffer, get_buffer};
use lisp::LispObject;
use lisp::defsubr;
use lists::memq;

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
    memq(buffer, LispObject::from_raw(unsafe { Vminibuffer_list })).is_not_nil()
}

/// Return the currently active minibuffer window, or nil if none.
#[lisp_fn]
pub fn active_minibuffer_window() -> LispObject {
    unsafe {
        if minibuf_level == 0 {
            LispObject::constant_nil()
        } else {
            LispObject::from_raw(minibuf_window)
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
        minibuf_window = window.to_raw();
    }

    window
}

include!(concat!(env!("OUT_DIR"), "/minibuf_exports.rs"));
