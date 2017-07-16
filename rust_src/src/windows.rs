//! Functions operating on windows.

use lisp::LispObject;
use remacs_macros::lisp_fn;


/// Return t if OBJECT is a window and nil otherwise.
#[lisp_fn]
fn windowp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_window())
}
