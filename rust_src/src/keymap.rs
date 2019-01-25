//! Keymap support

use std;
use std::ptr;

use libc::c_void;

use remacs_macros::lisp_fn;

use crate::{
    buffers::current_buffer,
    data::{aref, fset, indirect_function, set},
    eval::{autoload_do_load, unbind_to},
    indent::indent_to,
    keyboard::lucid_event_type_list_p,
    lisp::LispObject,
    lists::{nth, setcdr},
    lists::{LispCons, LispConsCircularChecks, LispConsEndChecks},
    obarray::intern,
    remacs_sys::{
        access_keymap, copy_keymap_item, describe_vector, make_save_funcptr_ptr_obj,
        map_char_table, map_keymap_call, map_keymap_char_table_item, map_keymap_function_t,
        map_keymap_item, maybe_quit, specbind,
    },
    remacs_sys::{char_bits, current_global_map as _current_global_map, globals, EmacsInt},
    remacs_sys::{
        Fcopy_sequence, Fevent_convert_list, Fmake_char_table, Fpurecopy, Fset_char_table_range,
        Fterpri,
    },
    remacs_sys::{
        Qautoload, Qkeymap, Qkeymapp, Qnil, Qstandard_output, Qt, Qvector_or_char_table_p,
    },
    symbols::LispSymbolRef,
    threads::{c_specpdl_index, ThreadState},
};

pub fn Ctl(c: char) -> i32 {
    (c as i32) & 0x1f
}

/// Hash table used to cache a reverse-map to speed up calls to where-is.
declare_GC_protected_static!(where_is_cache, Qnil);

/// Allows the C code to get the value of `where_is_cache`
#[no_mangle]
pub extern "C" fn get_where_is_cache() -> LispObject {
    unsafe { where_is_cache }
}

/// Allows the C code to set the value of `where_is_cache`
#[no_mangle]
pub extern "C" fn set_where_is_cache(val: LispObject) {
    unsafe {
        where_is_cache = val;
    }
}

/// Which keymaps are reverse-stored in the cache.
declare_GC_protected_static!(where_is_cache_keymaps, Qt);

/// Allows the C code to get the value of `where_is_cache_keymaps`
#[no_mangle]
pub extern "C" fn get_where_is_cache_keymaps() -> LispObject {
    unsafe { where_is_cache_keymaps }
}

/// Allows the C code to set the value of `where_is_cache_keymaps`
#[no_mangle]
pub extern "C" fn set_where_is_cache_keymaps(val: LispObject) {
    unsafe {
        where_is_cache_keymaps = val;
    }
}

/// Check that OBJECT is a keymap (after dereferencing through any
/// symbols).  If it is, return it.
///
/// If AUTOLOAD and if OBJECT is a symbol whose function value
/// is an autoload form, do the autoload and try again.
/// If AUTOLOAD, callers must assume GC is possible.
///
/// `ERROR_IF_NOT_KEYMAP` controls how we respond if OBJECT isn't a keymap.
/// If `ERROR_IF_NOT_KEYMAP`, signal an error; otherwise,
/// just return Qnil.
///
/// Note that most of the time, we don't want to pursue autoloads.
/// Functions like `Faccessible_keymaps` which scan entire keymap trees
/// shouldn't load every autoloaded keymap.  I'm not sure about this,
/// but it seems to me that only `read_key_sequence`, `Flookup_key`, and
/// `Fdefine_key` should cause keymaps to be autoloaded.
///
/// This function can GC when AUTOLOAD is true, because it calls
/// `Fautoload_do_load` which can GC.
#[no_mangle]
pub extern "C" fn get_keymap(
    object: LispObject,
    error_if_not_keymap: bool,
    autoload: bool,
) -> LispObject {
    let object = object;

    let mut autoload_retry = true;
    while autoload_retry {
        autoload_retry = false;

        if object.is_nil() {
            break;
        }

        if let Some((car, _)) = object.into() {
            if car.eq(Qkeymap) {
                return object;
            }
        }

        let tem = indirect_function(object);
        if let Some((car, _)) = tem.into() {
            if car.eq(Qkeymap) {
                return tem;
            }

            // Should we do an autoload?  Autoload forms for keymaps have
            // Qkeymap as their fifth element.
            if (autoload || !error_if_not_keymap) && car.eq(Qautoload) && object.is_symbol() {
                let tail = nth(4, tem);
                if tail.eq(Qkeymap) {
                    if autoload {
                        autoload_do_load(tem, object, Qnil);
                        autoload_retry = true;
                    } else {
                        return object;
                    }
                }
            }
        }
    }

    if error_if_not_keymap {
        wrong_type!(Qkeymapp, object);
    }
    Qnil
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
pub fn make_keymap(string: LispObject) -> (LispObject, (LispObject, LispObject)) {
    let tail: LispObject = if string.is_not_nil() {
        list!(string)
    } else {
        Qnil
    };

    let char_table = unsafe { Fmake_char_table(Qkeymap, Qnil) };
    (Qkeymap, (char_table, tail))
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
    let map = get_keymap(object, false, false);
    map.is_not_nil()
}

/// Return the parent map of KEYMAP, or nil if it has none.
/// We assume that KEYMAP is a valid keymap.
#[no_mangle]
pub extern "C" fn keymap_parent(keymap: LispObject, autoload: bool) -> LispObject {
    let map = get_keymap(keymap, true, autoload);
    let mut current = Qnil;
    for elt in map.iter_tails(LispConsEndChecks::off, LispConsCircularChecks::off) {
        current = elt.cdr();
        if keymapp(current) {
            return current;
        }
    }
    get_keymap(current, false, autoload)
}

/// Return the parent keymap of KEYMAP.
/// If KEYMAP has no parent, return nil.
#[lisp_fn(name = "keymap-parent", c_name = "keymap_parent")]
pub fn keymap_parent_lisp(keymap: LispObject) -> LispObject {
    keymap_parent(keymap, true)
}

/// Check whether MAP is one of MAPS parents.
#[no_mangle]
pub extern "C" fn keymap_memberp(map: LispObject, maps: LispObject) -> bool {
    let map = map;
    let mut maps = maps;
    if map.is_nil() {
        return false;
    }
    while keymapp(maps) && !map.eq(maps) {
        maps = keymap_parent(maps, false);
    }
    map.eq(maps)
}

/// Modify KEYMAP to set its parent map to PARENT.
/// Return PARENT.  PARENT should be nil or another keymap.
#[lisp_fn]
pub fn set_keymap_parent(keymap: LispObject, parent: LispObject) -> LispObject {
    // Flush any reverse-map cache
    unsafe {
        where_is_cache = Qnil;
        where_is_cache_keymaps = Qt;
    }

    let mut parent = parent;
    let keymap = get_keymap(keymap, true, true);
    if parent.is_not_nil() {
        parent = get_keymap(parent, true, false);

        // Check for cycles
        if keymap_memberp(keymap, parent) {
            error!("Cyclic keymap inheritance");
        }
    }

    // Skip past the initial element 'keymap'.
    let mut prev = LispCons::from(keymap);
    let mut list;

    loop {
        list = prev.cdr();

        // If there is a parent keymap here, replace it.
        // If we came to the end, add the parent in PREV.
        match list.as_cons() {
            None => break,
            Some(cons) => {
                if keymapp(list) {
                    break;
                } else {
                    prev = cons;
                }
            }
        }
    }
    prev.check_impure();
    prev.set_cdr(parent);
    parent
}

/// Return the prompt-string of a keymap MAP.
/// If non-nil, the prompt is shown in the echo-area
/// when reading a key-sequence to be looked-up in this keymap.
#[lisp_fn]
pub fn keymap_prompt(map: LispObject) -> LispObject {
    let map = get_keymap(map, false, false);
    for elt in map.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
        let mut tem = elt;
        if tem.is_string() {
            return tem;
        } else if keymapp(tem) {
            tem = keymap_prompt(tem);
            if tem.is_not_nil() {
                return tem;
            }
        }
    }
    Qnil
}

/// Same as `map_keymap_internal`, but traverses parent keymaps as well.
/// AUTOLOAD indicates that autoloaded keymaps should be loaded.
#[no_mangle]
pub unsafe extern "C" fn map_keymap(
    map: LispObject,
    fun: map_keymap_function_t,
    args: LispObject,
    data: *mut c_void,
    autoload: bool,
) {
    let mut map = get_keymap(map, true, autoload);
    while map.is_cons() {
        if let Some((car, cdr)) = map.into() {
            if keymapp(car) {
                map_keymap(car, fun, args, data, autoload);
                map = cdr;
            } else {
                map = map_keymap_internal(map, fun, args, data);
            }
        }

        if !map.is_cons() {
            map = get_keymap(map, false, autoload);
        }
    }
}

/// Call FUNCTION once for each event binding in KEYMAP.
/// FUNCTION is called with two arguments: the event that is bound, and
/// the definition it is bound to.  The event may be a character range.
///
/// If KEYMAP has a parent, the parent's bindings are included as well.
/// This works recursively: if the parent has itself a parent, then the
/// grandparent's bindings are also included and so on.
/// usage: (map-keymap FUNCTION KEYMAP)
#[lisp_fn(name = "map-keymap", c_name = "map_keymap", min = "2")]
pub fn map_keymap_lisp(function: LispObject, keymap: LispObject, sort_first: bool) -> LispObject {
    if sort_first {
        return call!(intern("map-keymap-sorted").into(), function, keymap);
    }
    unsafe {
        map_keymap(
            keymap,
            Some(map_keymap_call),
            function,
            ptr::null_mut(),
            true,
        )
    };
    Qnil
}

/// Call FUN for every binding in MAP and stop at (and return) the parent.
/// FUN is called with 4 arguments: FUN (KEY, BINDING, ARGS, DATA).
#[no_mangle]
pub unsafe extern "C" fn map_keymap_internal(
    map: LispObject,
    fun: map_keymap_function_t,
    args: LispObject,
    data: *mut c_void,
) -> LispObject {
    let map = map;
    let tail = match map.into() {
        None => Qnil,
        Some((car, cdr)) => {
            if car.eq(Qkeymap) {
                cdr
            } else {
                map
            }
        }
    };

    let mut parent = tail;
    for tail_cons in tail.iter_tails(LispConsEndChecks::off, LispConsCircularChecks::off) {
        let binding = tail_cons.car();
        if binding.eq(Qkeymap) {
            break;
        } else {
            // An embedded parent.
            if keymapp(binding) {
                break;
            }

            if let Some((car, cdr)) = binding.into() {
                map_keymap_item(fun, args, car, cdr, data);
            } else if binding.is_vector() {
                if let Some(binding_vec) = binding.as_vectorlike() {
                    for c in 0..binding_vec.pseudovector_size() {
                        map_keymap_item(fun, args, c.into(), aref(binding, c), data);
                    }
                }
            } else if binding.is_char_table() {
                let saved = match fun {
                    Some(f) => make_save_funcptr_ptr_obj(Some(std::mem::transmute(f)), data, args),
                    None => make_save_funcptr_ptr_obj(None, data, args),
                };
                map_char_table(Some(map_keymap_char_table_item), Qnil, binding, saved);
            }
        }

        parent = tail_cons.cdr();
    }

    parent
}

/// Call FUNCTION once for each event binding in KEYMAP.
/// FUNCTION is called with two arguments: the event that is bound, and
/// the definition it is bound to.  The event may be a character range.
/// If KEYMAP has a parent, this function returns it without processing it.
#[lisp_fn(name = "map-keymap-internal", c_name = "map_keymap_internal")]
pub fn map_keymap_internal_lisp(function: LispObject, mut keymap: LispObject) -> LispObject {
    keymap = get_keymap(keymap, true, true);
    unsafe { map_keymap_internal(keymap, Some(map_keymap_call), function, ptr::null_mut()) }
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
        Qnil
    } else {
        lookup_key(map, keys, accept_default)
    }
}

/// Return current buffer's local keymap, or nil if it has none.
/// Normally the local keymap is set by the major mode with `use-local-map'.
#[lisp_fn]
pub fn current_local_map() -> LispObject {
    ThreadState::current_buffer_unchecked().keymap_
}

/// Select KEYMAP as the local keymap.
/// If KEYMAP is nil, that means no local keymap.
#[lisp_fn]
pub fn use_local_map(mut keymap: LispObject) {
    if !keymap.is_nil() {
        let map = get_keymap(keymap, true, true);
        keymap = map;
    }

    ThreadState::current_buffer_unchecked().keymap_ = keymap;
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
        Qnil
    } else {
        lookup_key(map, keys, accept_default)
    }
}

/// Return the current global keymap.
#[lisp_fn]
pub fn current_global_map() -> LispObject {
    unsafe { _current_global_map }
}

/// Select KEYMAP as the global keymap.
#[lisp_fn]
pub fn use_global_map(keymap: LispObject) {
    unsafe { _current_global_map = get_keymap(keymap, true, true) };
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
    let mut keymap = get_keymap(keymap, true, true);
    let length = key.as_vector_or_string_length() as EmacsInt;
    if length == 0 {
        return keymap;
    }

    let mut idx = 0;
    loop {
        let mut c = aref(key, idx);
        idx += 1;

        if c.is_cons() && lucid_event_type_list_p(c.into()) {
            c = unsafe { Fevent_convert_list(c) };
        }

        // Turn the 8th bit of string chars into a meta modifier.
        if let Some(k) = key.as_string() {
            if let Some(x) = c.as_fixnum() {
                let x = x as u32;
                if x & 0x80 != 0 && !k.is_multibyte() {
                    c = ((x | char_bits::CHAR_META) & !0x80).into();
                }
            }
        }

        // Allow string since binding for `menu-bar-select-buffer'
        // includes the buffer name in the key sequence.
        if !(c.is_fixnum() || c.is_symbol() || c.is_cons() || c.is_string()) {
            message_with_string!("Key sequence contains invalid event %s", c, true);
        }

        let cmd = unsafe { access_keymap(keymap, c, ok, false, true) };
        if idx == length {
            return cmd;
        }

        keymap = get_keymap(cmd, false, true);
        if !keymap.is_cons() {
            return idx.into();
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
    command: LispSymbolRef,
    mapvar: LispObject,
    name: LispObject,
) -> LispSymbolRef {
    let map = make_sparse_keymap(name);
    fset(command, map);
    if mapvar.is_not_nil() {
        set(mapvar.into(), map);
    } else {
        set(command, map);
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
        let s = if unsafe { globals.Vpurify_flag }.is_not_nil() {
            unsafe { Fpurecopy(string) }
        } else {
            string
        };
        list!(Qkeymap, s)
    } else {
        list!(Qkeymap)
    }
}

#[no_mangle]
pub extern "C" fn describe_vector_princ(elt: LispObject, fun: LispObject) {
    indent_to(16, 1.into());
    call!(fun, elt);
    unsafe { Fterpri(Qnil, Qnil) };
}

/// Insert a description of contents of VECTOR.
/// This is text showing the elements of vector matched against indices.
/// DESCRIBER is the output function used; nil means use `princ'.
#[lisp_fn(min = "1", name = "describe-vector", c_name = "describe_vector")]
pub fn describe_vector_lisp(vector: LispObject, mut describer: LispObject) {
    if describer.is_nil() {
        describer = intern("princ").into();
    }
    unsafe { specbind(Qstandard_output, current_buffer()) };
    if !(vector.is_vector() || vector.is_char_table()) {
        wrong_type!(Qvector_or_char_table_p, vector);
    }

    let count = c_specpdl_index();
    unsafe {
        describe_vector(
            vector,
            Qnil,
            describer,
            Some(describe_vector_princ),
            false,
            Qnil,
            Qnil,
            false,
            false,
        )
    };

    unbind_to(count, Qnil);
}

#[no_mangle]
pub extern "C" fn copy_keymap_1(chartable: LispObject, idx: LispObject, elt: LispObject) {
    unsafe { Fset_char_table_range(chartable, idx, copy_keymap_item(elt)) };
}

/// Return a copy of the keymap KEYMAP.
///
/// Note that this is almost never needed.  If you want a keymap that's like
/// another yet with a few changes, you should use map inheritance rather
/// than copying.  I.e. something like:
///
/// (let ((map (make-sparse-keymap)))
/// (set-keymap-parent map <theirmap>)
/// (define-key map ...)
/// ...)
///
/// After performing `copy-keymap', the copy starts out with the same definitions
/// of KEYMAP, but changing either the copy or KEYMAP does not affect the other.
/// Any key definitions that are subkeymaps are recursively copied.
/// However, a key definition which is a symbol whose definition is a keymap
/// is not copied.
#[lisp_fn]
pub fn copy_keymap(keymap: LispObject) -> LispObject {
    let keymap = get_keymap(keymap, true, false);
    let mut tail = list!(Qkeymap);
    let copy = tail;

    let (_, mut keymap) = keymap.into(); // Skip the `keymap' symbol.

    while let Some((mut elt, kmd)) = keymap.into() {
        if elt.eq(Qkeymap) {
            break;
        }

        if elt.is_char_table() {
            elt = unsafe { Fcopy_sequence(elt) };
            unsafe { map_char_table(Some(copy_keymap_1), Qnil, elt, elt) };
        } else if let Some(v) = elt.as_vector() {
            elt = unsafe { Fcopy_sequence(elt) };
            let mut v2 = elt.as_vector().unwrap();
            for (i, obj) in v.iter().enumerate() {
                v2.set(i, unsafe { copy_keymap_item(obj) });
            }
        } else if let Some((front, back)) = elt.into() {
            if front.eq(Qkeymap) {
                // This is a sub keymap
                elt = copy_keymap(elt);
            } else {
                elt = (front, unsafe { copy_keymap_item(back) }).into();
            }
        }

        setcdr(tail.into(), list!(elt));
        tail = LispCons::from(tail).cdr();
        keymap = kmd;
    }

    setcdr(tail.into(), keymap);
    copy
}

include!(concat!(env!("OUT_DIR"), "/keymap_exports.rs"));
