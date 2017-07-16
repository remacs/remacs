//! Functions operating on windows.

use lisp::LispObject;
use remacs_macros::lisp_fn;


/// Return t if OBJECT is a window.
#[lisp_fn]
fn windowp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_window())
}
