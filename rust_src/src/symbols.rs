use remacs_macros::{lisp_fn, lisp_doc};
use lisp::LispObject;

#[lisp_fn(name = "symbolp", min = "1")]
#[lisp_doc("Return t if OBJECT is a symbol")]
fn symbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}
