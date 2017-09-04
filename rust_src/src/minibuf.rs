//! Minibuffer input and completion.

use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys::{Lisp_Type, Vminibuffer_list, make_lisp_ptr};
use buffers::{current_buffer, get_buffer};
use lists::memq;
use libc::c_void;


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
        object
    };
    buffer.as_buffer_or_error();
    if memq(buffer, LispObject::from_raw(unsafe { Vminibuffer_list })).is_nil() {
        LispObject::constant_nil()
    } else {
        LispObject::constant_t()
    }
}
