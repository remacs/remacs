//! Keymap support

use lisp::{defsubr, LispObject};
use remacs_macros::lisp_fn;
use remacs_sys::{current_global_map as _current_global_map, Flookup_key};
use threads::ThreadState;

#[inline]
pub fn Ctl(c: char) -> i32 {
    (c as i32) & 0x1f
}

/// Return the binding for command KEYS in current local keymap only.
/// KEYS is a string or vector, a sequence of keystrokes.
/// The binding is probably a symbol with a function definition.
/// If optional argument ACCEPT-DEFAULT is non-nil, recognize default
/// bindings; see the description of `lookup-key' for more details about this.
#[lisp_fn(min = "1")]
fn local_key_binding(keys: LispObject, accept_default: LispObject) -> LispObject {
    let map = current_local_map();
    if map.is_nil() {
        LispObject::constant_nil()
    } else {
        unsafe {
            LispObject::from(Flookup_key(
                map.to_raw(),
                keys.to_raw(),
                accept_default.to_raw(),
            ))
        }
    }
}

/// Return current buffer's local keymap, or nil if it has none.
/// Normally the local keymap is set by the major mode with `use-local-map'.
#[lisp_fn]
pub fn current_local_map() -> LispObject {
    LispObject::from(ThreadState::current_buffer().keymap)
}

/// Return the current global keymap.
#[lisp_fn]
pub fn current_global_map() -> LispObject {
    unsafe { LispObject::from(_current_global_map) }
}

include!(concat!(env!("OUT_DIR"), "/keymap_exports.rs"));
