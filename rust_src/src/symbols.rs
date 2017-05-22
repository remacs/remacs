use remacs_macros::lisp_fn;
use lisp::LispObject;

/// Return t if OBJECT is a symbol
#[lisp_fn(name = "symbolp", min = "1")]
fn symbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}
