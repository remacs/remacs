//! Minibuffer input and completion.

use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys::{Vminibuffer_list, minibuf_level, minibuf_window};
use buffers::{current_buffer, get_buffer};
use lists::memq;


/// Return t if BUFFER is a minibuffer.
/// No argument or nil as argument means use current buffer as BUFFER.
/// BUFFER can be a buffer or a buffer name.
#[lisp_fn(min = "0")]
pub fn minibufferp(object: LispObject) -> LispObject {
    let buffer = if object.is_nil() {
        current_buffer()
    } else if object.is_string() {
        get_buffer(object)
    } else {
        object.as_buffer_or_error();
        object
    };
    LispObject::from_bool(
        memq(buffer, LispObject::from_raw(unsafe { Vminibuffer_list })).is_not_nil(),
    )
}

/// Return the currently active minibuffer window, or nil if none.
#[lisp_fn]
fn active_minibuffer_window() -> LispObject {
    if LispObject::from_raw(unsafe { minibuf_level }).is_nil() {
        LispObject::constant_nil()
    } else {
        LispObject::from_raw(unsafe { minibuf_window })
    }
}
