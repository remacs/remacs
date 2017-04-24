use remacs_macros::lisp_fn;
use lisp::LispObject;

#[lisp_fn(name = "symbolp", min = "1")]
/// Return t if OBJECT is a symbol
fn symbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}
