use remacs_macros::lisp_fn;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::Lisp_Symbol;

pub type LispSymbolRef = ExternalPtr<Lisp_Symbol>;

impl LispSymbolRef {
    pub fn symbol_name(&self) -> LispObject {
        LispObject::from_raw(self.name)
    }
}

/// Return t if OBJECT is a symbol.
#[lisp_fn]
fn symbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}
