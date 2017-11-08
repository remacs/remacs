//! Keymap support

use remacs_macros::lisp_fn;
use remacs_sys::{current_global_map as _current_global_map, EmacsInt};
use remacs_sys::{access_keymap, get_keymap, maybe_quit, Faref, Fevent_convert_list};
use remacs_sys::CHAR_META;

use keyboard::lucid_event_type_list_p;
use lisp::{defsubr, LispObject};
use threads::ThreadState;


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
        lookup_key(map, keys, accept_default)
    }
}

/// Return current buffer's local keymap, or nil if it has none.
/// Normally the local keymap is set by the major mode with `use-local-map'.
#[lisp_fn]
fn current_local_map() -> LispObject {
    LispObject::from(ThreadState::current_buffer().keymap)
}

/// Return the current global keymap.
#[lisp_fn]
fn current_global_map() -> LispObject {
    unsafe { LispObject::from(_current_global_map) }
}

// Value is number if KEY is too long; nil if valid but has no definition.
// GC is possible in this function.

/// In keymap KEYMAP, look up key sequence KEY.  Return the definition.
/// A value of nil means undefined.  See doc of `define-key'
/// for kinds of definitions.
///
/// A number as value means KEY is "too long";
/// that is, characters or symbols in it except for the last one
/// fail to be a valid sequence of prefix characters in KEYMAP.
/// The number is how many characters at the front of KEY
/// it takes to reach a non-prefix key.
///
/// Normally, `lookup-key' ignores bindings for t, which act as default
/// bindings, used when nothing else in the keymap applies; this makes it
/// usable as a general function for probing keymaps.  However, if the
/// third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
/// recognize the default bindings, just as `read-key-sequence' does.
#[lisp_fn(min = "2")]
pub fn lookup_key(keymap: LispObject, key: LispObject, accept_default: LispObject) -> LispObject {
    let ok = accept_default.is_not_nil();
    let mut keymap = unsafe { get_keymap(keymap.to_raw(), true, true) };
    let length = key.as_vector_or_string_length() as EmacsInt;
    if length == 0 {
        return LispObject::from(keymap);
    }

    let mut idx = 0;
    loop {
        let mut c = LispObject::from(unsafe {
            Faref(key.to_raw(), LispObject::from_fixnum(idx).to_raw())
        });
        idx += 1;

        if c.is_cons() && lucid_event_type_list_p(c) {
            c = LispObject::from(unsafe { Fevent_convert_list(c.to_raw()) });
        }

        // Turn the 8th bit of string chars into a meta modifier.
        if let Some(k) = key.as_string() {
            if let Some(x) = c.as_fixnum() {
                let x = x as u32;
                if x & 0x80 != 0 && !k.is_multibyte() {
                    c = LispObject::from_fixnum(((x | CHAR_META) & !0x80) as EmacsInt);
                }
            }
        }

        // Allow string since binding for `menu-bar-select-buffer'
        // includes the buffer name in the key sequence.
        if !(c.is_fixnum() || c.is_symbol() || c.is_cons() || c.is_string()) {
            message_with_string!("Key sequence contains invalid event %s", c, true);
        }

        let cmd = unsafe { access_keymap(keymap, c.to_raw(), ok, false, true) };
        if idx == length {
            return LispObject::from(cmd);
        }

        keymap = unsafe { get_keymap(cmd, false, true) };
        if !LispObject::from(keymap).is_cons() {
            return LispObject::from_natnum(idx);
        }

        unsafe {
            maybe_quit();
        };
    }
}

include!(concat!(env!("OUT_DIR"), "/keymap_exports.rs"));
