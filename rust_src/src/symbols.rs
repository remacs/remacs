use std::ptr;

use lisp::LispObject;

fn symbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}

defun!("symbolp",
       Fsymbolp(object),
       Ssymbolp,
       symbolp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a symbol.

(fn OBJECT)");
