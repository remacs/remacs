//! Keymap support

use remacs_macros::lisp_fn;
use remacs_sys::{current_global_map as _current_global_map, globals, EmacsInt, CHAR_META};
use remacs_sys::{Fcons, Fevent_convert_list, Ffset, Fmake_char_table, Fpurecopy, Fset};
use remacs_sys::{Qkeymap, Qnil};
use remacs_sys::{access_keymap, get_keymap, maybe_quit};

use data::aref;
use keyboard::lucid_event_type_list_p;
use lisp::{defsubr, LispObject};
use threads::ThreadState;

#[inline]
pub fn Ctl(c: char) -> i32 {
    (c as i32) & 0x1f
}

/// Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
/// CHARTABLE is a char-table that holds the bindings for all characters
/// without modifiers.  All entries in it are initially nil, meaning
/// "command undefined".  ALIST is an assoc-list which holds bindings for
/// function keys, mouse events, and any other things that appear in the
/// input stream.  Initially, ALIST is nil.
///
/// The optional arg STRING supplies a menu name for the keymap
/// in case you use it as a menu with `x-popup-menu'.
#[lisp_fn(min = "0")]
pub fn make_keymap(string: LispObject) -> LispObject {
    let tail: LispObject = if !string.is_nil() {
        list!(string)
    } else {
        LispObject::constant_nil()
    };

    let char_table = unsafe { Fmake_char_table(Qkeymap, Qnil) };
    LispObject::from_raw(unsafe { Fcons(Qkeymap, Fcons(char_table, tail.to_raw())) })
}

/// Return t if OBJECT is a keymap.
///
/// A keymap is a list (keymap . ALIST),
/// or a symbol whose function definition is itself a keymap.
/// ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);
/// a vector of densely packed bindings for small character codes
/// is also allowed as an element.
#[lisp_fn]
pub fn keymapp(object: LispObject) -> bool {
    let map = unsafe { LispObject::from_raw(get_keymap(object.to_raw(), false, false)) };
    map.is_not_nil()
}

/// Return the binding for command KEYS in current local keymap only.
/// KEYS is a string or vector, a sequence of keystrokes.
/// The binding is probably a symbol with a function definition.
/// If optional argument ACCEPT-DEFAULT is non-nil, recognize default
/// bindings; see the description of `lookup-key' for more details about this.
#[lisp_fn(min = "1")]
pub fn local_key_binding(keys: LispObject, accept_default: LispObject) -> LispObject {
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
pub fn current_local_map() -> LispObject {
    LispObject::from_raw(ThreadState::current_buffer().keymap)
}

/// Select KEYMAP as the local keymap.
/// If KEYMAP is nil, that means no local keymap.
#[lisp_fn]
pub fn use_local_map(mut keymap: LispObject) -> () {
    if !keymap.is_nil() {
        keymap = unsafe {
            let map = get_keymap(keymap.to_raw(), true, true);
            LispObject::from_raw(map)
        }
    }

    ThreadState::current_buffer().keymap = keymap.to_raw();
}

/// Return the binding for command KEYS in current global keymap only.
/// KEYS is a string or vector, a sequence of keystrokes.
/// The binding is probably a symbol with a function definition.
/// This function's return values are the same as those of `lookup-key'
/// (which see).
///
/// If optional argument ACCEPT-DEFAULT is non-nil, recognize default
/// bindings; see the description of `lookup-key' for more details about this.
#[lisp_fn(min = "1")]
pub fn global_key_binding(keys: LispObject, accept_default: LispObject) -> LispObject {
    let map = current_global_map();
    if map.is_nil() {
        LispObject::constant_nil()
    } else {
        lookup_key(map, keys, accept_default)
    }
}

/// Return the current global keymap.
#[lisp_fn]
pub fn current_global_map() -> LispObject {
    unsafe { LispObject::from_raw(_current_global_map) }
}

/// Select KEYMAP as the global keymap.
#[lisp_fn]
pub fn use_global_map(keymap: LispObject) -> () {
    unsafe {
        _current_global_map = get_keymap(keymap.to_raw(), true, true);
    }
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
        return LispObject::from_raw(keymap);
    }

    let mut idx = 0;
    loop {
        let mut c = aref(key, idx);
        idx += 1;

        if c.is_cons() && lucid_event_type_list_p(c.as_cons()) {
            c = LispObject::from_raw(unsafe { Fevent_convert_list(c.to_raw()) });
        }

        // Turn the 8th bit of string chars into a meta modifier.
        if let Some(k) = key.as_string() {
            if let Some(x) = c.as_fixnum() {
                let x = x as u32;
                if x & 0x80 != 0 && !k.is_multibyte() {
                    c = LispObject::from_fixnum(EmacsInt::from((x | CHAR_META) & !0x80));
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
            return LispObject::from_raw(cmd);
        }

        keymap = unsafe { get_keymap(cmd, false, true) };
        if !LispObject::from_raw(keymap).is_cons() {
            return LispObject::from_natnum(idx);
        }

        unsafe {
            maybe_quit();
        };
    }
}

/// Define COMMAND as a prefix command.  COMMAND should be a symbol.
/// A new sparse keymap is stored as COMMAND's function definition and its
/// value.
/// This prepares COMMAND for use as a prefix key's binding.
/// If a second optional argument MAPVAR is given, it should be a symbol.
/// The map is then stored as MAPVAR's value instead of as COMMAND's
/// value; but COMMAND is still defined as a function.
/// The third optional argument NAME, if given, supplies a menu name
/// string for the map.  This is required to use the keymap as a menu.
/// This function returns COMMAND.
#[lisp_fn(min = "1")]
pub fn define_prefix_command(
    command: LispObject,
    mapvar: LispObject,
    name: LispObject,
) -> LispObject {
    let map = make_sparse_keymap(name);
    unsafe { Ffset(command.to_raw(), map.to_raw()) };
    if mapvar.is_not_nil() {
        unsafe { Fset(mapvar.to_raw(), map.to_raw()) };
    } else {
        unsafe { Fset(command.to_raw(), map.to_raw()) };
    }
    command
}

/// Construct and return a new sparse keymap.
/// Its car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),
/// which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),
/// which binds the function key or mouse event SYMBOL to DEFINITION.
/// Initially the alist is nil.
///
/// The optional arg STRING supplies a menu name for the keymap
/// in case you use it as a menu with `x-popup-menu'.
#[lisp_fn(min = "0")]
pub fn make_sparse_keymap(string: LispObject) -> LispObject {
    if string.is_not_nil() {
        let s = if LispObject::from_raw(unsafe { globals.f_Vpurify_flag }).is_not_nil() {
            unsafe { LispObject::from_raw(Fpurecopy(string.to_raw())) }
        } else {
            string
        };
        list!(LispObject::from_raw(Qkeymap), s)
    } else {
        list!(LispObject::from_raw(Qkeymap))
    }
}

include!(concat!(env!("OUT_DIR"), "/keymap_exports.rs"));
