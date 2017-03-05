#![feature(plugin)]
#![plugin(remacs_codegen)]

#[path = "../../lisp-mock/lib.rs"]
pub mod remacs;
#[path = "../../lisp-mock/lib.rs"]
pub mod remacs_sys;
use remacs::lisp::LispObject;

#[lisp_function(name = "symbolp", min = 1)]
fn symbolp(object: LispObject) -> LispObject {
    if object.is_symbol() {
        LispObject::constant_t()
    } else {
        LispObject::constant_nil()
    }
}

fn main() {
    return;
}
