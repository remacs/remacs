//! Keymap support

use std::mem;
use std::ptr;

use libc::c_void;

use remacs_macros::lisp_fn;
use remacs_sys::{current_global_map as _current_global_map, globals, EmacsInt, Lisp_Object,
                 CHAR_META};
use remacs_sys::{Fcons, Fevent_convert_list, Ffset, Fmake_char_table, Fpurecopy, Fset};
use remacs_sys::{Qautoload, Qkeymap, Qkeymapp, Qnil, Qt};
use remacs_sys::{access_keymap, make_save_funcptr_ptr_obj, map_char_table, map_keymap_call,
                 map_keymap_char_table_item, map_keymap_function_t, map_keymap_item, maybe_quit,
                 voidfuncptr};

use data::{aref, indirect_function};
use eval::autoload_do_load;
use keyboard::lucid_event_type_list_p;
use lisp::{defsubr, LispObject};
use lists::nth;
use obarray::intern;
use threads::ThreadState;

#[inline]
pub fn Ctl(c: char) -> i32 {
    (c as i32) & 0x1f
}

/// Hash table used to cache a reverse-map to speed up calls to where-is.
declare_GC_protected_static!(where_is_cache, Qnil);

/// Allows the C code to get the value of `where_is_cache`
#[no_mangle]
pub extern "C" fn get_where_is_cache() -> Lisp_Object {
    unsafe { where_is_cache }
}

/// Allows the C code to set the value of `where_is_cache`
#[no_mangle]
pub extern "C" fn set_where_is_cache(val: Lisp_Object) {
    unsafe {
        where_is_cache = val;
    }
}

/// Which keymaps are reverse-stored in the cache.
declare_GC_protected_static!(where_is_cache_keymaps, Qt);

/// Allows the C code to get the value of `where_is_cache_keymaps`
#[no_mangle]
pub extern "C" fn get_where_is_cache_keymaps() -> Lisp_Object {
    unsafe { where_is_cache_keymaps }
}

/// Allows the C code to set the value of `where_is_cache_keymaps`
#[no_mangle]
pub extern "C" fn set_where_is_cache_keymaps(val: Lisp_Object) {
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
    object: Lisp_Object,
    error_if_not_keymap: bool,
    autoload: bool,
) -> Lisp_Object {
    let object = LispObject::from_raw(object);

    let mut autoload_retry = true;
    while autoload_retry {
        autoload_retry = false;

        if object.is_nil() {
            break;
        }

        if let Some(cons) = object.as_cons() {
            if cons.car().eq_raw(Qkeymap) {
                return object.to_raw();
            }
        }

        let tem = indirect_function(object);
        if let Some(cons) = tem.as_cons() {
            if cons.car().eq_raw(Qkeymap) {
                return tem.to_raw();
            }

            // Should we do an autoload?  Autoload forms for keymaps have
            // Qkeymap as their fifth element.
            if (autoload || !error_if_not_keymap) && cons.car().eq_raw(Qautoload)
                && object.is_symbol()
            {
                let tail = nth(4, tem);
                if tail.eq_raw(Qkeymap) {
                    if autoload {
                        autoload_do_load(tem, object, LispObject::constant_nil());
                        autoload_retry = true;
                    } else {
                        return object.to_raw();
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
    let map = LispObject::from_raw(get_keymap(object.to_raw(), false, false));
    map.is_not_nil()
}

/// Return the parent map of KEYMAP, or nil if it has none.
/// We assume that KEYMAP is a valid keymap.
#[no_mangle]
pub extern "C" fn keymap_parent(keymap: Lisp_Object, autoload: bool) -> Lisp_Object {
    let map = LispObject::from_raw(get_keymap(keymap, true, autoload));
    let mut current = LispObject::constant_nil();
    for elt in map.iter_tails_safe() {
        current = elt.cdr();
        if keymapp(current) {
            return current.to_raw();
        }
    }
    get_keymap(current.to_raw(), false, autoload)
}

/// Return the parent keymap of KEYMAP.
/// If KEYMAP has no parent, return nil.
#[lisp_fn(name = "keymap-parent", c_name = "keymap_parent")]
pub fn keymap_parent_lisp(keymap: LispObject) -> LispObject {
    LispObject::from_raw(keymap_parent(keymap.to_raw(), true))
}

/// Check whether MAP is one of MAPS parents.
#[no_mangle]
pub extern "C" fn keymap_memberp(map: Lisp_Object, maps: Lisp_Object) -> bool {
    let map = LispObject::from_raw(map);
    let mut maps = LispObject::from_raw(maps);
    if map.is_nil() {
        return false;
    }
    while keymapp(maps) && map.ne(maps) {
        maps = LispObject::from_raw(keymap_parent(maps.to_raw(), false));
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
    let keymap = LispObject::from_raw(get_keymap(keymap.to_raw(), true, true));
    if parent.is_not_nil() {
        parent = LispObject::from_raw(get_keymap(parent.to_raw(), true, false));

        // Check for cycles
        if keymap_memberp(keymap.to_raw(), parent.to_raw()) {
            error!("Cyclic keymap inheritance");
        }
    }

    // Skip past the initial element 'keymap'.
    let mut prev = keymap.as_cons_or_error();
    let mut list;

    loop {
        list = prev.cdr();

        // If there is a parent keymap here, replace it.
        // If we came to the end, add the parent in PREV.
        match list.as_cons() {
            None => break,
            Some(cons) => if keymapp(list) {
                break;
            } else {
                prev = cons;
            },
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
    let map = LispObject::from_raw(get_keymap(map.to_raw(), false, false));
    for elt in map.iter_cars_safe() {
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
    LispObject::constant_nil()
}

/// Same as `map_keymap_internal`, but traverses parent keymaps as well.
/// AUTOLOAD indicates that autoloaded keymaps should be loaded.
#[no_mangle]
pub extern "C" fn map_keymap(
    map: Lisp_Object,
    fun: map_keymap_function_t,
    args: Lisp_Object,
    data: *const c_void,
    autoload: bool,
) {
    let mut map = LispObject::from_raw(get_keymap(map, true, autoload));
    while map.is_cons() {
        if let Some(cons) = map.as_cons() {
            let (car, cdr) = cons.as_tuple();
            if keymapp(car) {
                map_keymap(car.to_raw(), fun, args, data, autoload);
                map = cdr;
            } else {
                map = LispObject::from_raw(unsafe {
                    map_keymap_internal(map.to_raw(), fun, args, data)
                });
            }
        }

        if !map.is_cons() {
            map = LispObject::from_raw(get_keymap(map.to_raw(), false, autoload));
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
#[lisp_fn(name = "map-keymap", min = "2")]
pub fn map_keymap_lisp(function: LispObject, keymap: LispObject, sort_first: bool) -> LispObject {
    if sort_first {
        return call!(intern("map-keymap-sorted"), function, keymap);
    }
    map_keymap(
        keymap.to_raw(),
        map_keymap_call,
        function.to_raw(),
        ptr::null_mut(),
        true,
    );
    LispObject::constant_nil()
}

/// Call FUN for every binding in MAP and stop at (and return) the parent.
/// FUN is called with 4 arguments: FUN (KEY, BINDING, ARGS, DATA).  */
#[no_mangle]
pub extern "C" fn map_keymap_internal(
    map: Lisp_Object,
    fun: map_keymap_function_t,
    args: Lisp_Object,
    data: *const c_void,
) -> Lisp_Object {
    let mut tail = LispObject::constant_nil();
    let map = LispObject::from_raw(map);
    if let Some(cons) = map.as_cons() {
        let (car, cdr) = cons.as_tuple();
        if car.eq_raw(Qkeymap) {
            tail = cdr
        } else {
            tail = map
        }
    };

    loop {
        match tail.as_cons() {
            None => break,
            Some(tail_cons) => {
                if tail_cons.car().eq_raw(Qkeymap) {
                    break;
                } else {
                    let binding = tail_cons.car();

                    // An embedded parent.
                    if keymapp(binding) {
                        break;
                    }

                    if let Some(binding_cons) = binding.as_cons() {
                        let (car, cdr) = binding_cons.as_tuple();
                        unsafe { map_keymap_item(fun, args, car.to_raw(), cdr.to_raw(), data) };
                    } else if binding.is_vector() {
                        if let Some(binding_vec) = binding.as_vectorlike() {
                            for c in 0..binding_vec.pseudovector_size() {
                                let character = LispObject::from_natnum(c);
                                unsafe {
                                    map_keymap_item(
                                        fun,
                                        args,
                                        character.to_raw(),
                                        aref(binding, c).to_raw(),
                                        data,
                                    )
                                };
                            }
                        }
                    } else if binding.is_char_table() {
                        unsafe {
                            let ptr = fun as *const ();
                            let funcptr: voidfuncptr = mem::transmute(ptr);

                            map_char_table(
                                map_keymap_char_table_item,
                                Qnil,
                                binding.to_raw(),
                                make_save_funcptr_ptr_obj(funcptr, data, args),
                            );
                        }
                    }
                }

                tail = tail_cons.cdr();
            }
        };
    }

    tail.to_raw()
}

/// Call FUNCTION once for each event binding in KEYMAP.
/// FUNCTION is called with two arguments: the event that is bound, and
/// the definition it is bound to.  The event may be a character range.
/// If KEYMAP has a parent, this function returns it without processing it.
#[lisp_fn(name = "map-keymap-internal")]
pub fn map_keymap_internal_lisp(function: LispObject, mut keymap: LispObject) -> LispObject {
    keymap = LispObject::from_raw(get_keymap(keymap.to_raw(), true, true));
    LispObject::from_raw(map_keymap_internal(
        keymap.to_raw(),
        map_keymap_call,
        function.to_raw(),
        ptr::null_mut(),
    ))
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
        let map = get_keymap(keymap.to_raw(), true, true);
        keymap = LispObject::from_raw(map);
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
    unsafe { _current_global_map = get_keymap(keymap.to_raw(), true, true) };
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
    let mut keymap = get_keymap(keymap.to_raw(), true, true);
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

        keymap = get_keymap(cmd, false, true);
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
