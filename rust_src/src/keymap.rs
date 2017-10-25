use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys::Flookup_key;
use threads::ThreadState;

// Return the binding for command KEYS in current local keymap only.
//   KEYS is a string or vector, a sequence of keystrokes.
//   The binding is probably a symbol with a function definition.
//
//   If optional argument ACCEPT-DEFAULT is non-nil, recognize default
//   bindings; see the description of `lookup-key' for more details about this.
#[lisp_fn(min = "1")]
fn local_key_binding(keys: LispObject, accept_default: LispObject) -> LispObject {
    let map = LispObject::from(ThreadState::current_buffer().keymap);
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
