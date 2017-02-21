#![feature(plugin)]
#![plugin(remacs_codegen)]

#[path = "../../lisp-mock/lib.rs"]
pub mod remacs;
#[path = "../../lisp-mock/lib.rs"]
pub mod remacs_sys;
use remacs::lisp::LispObject;

#[lisp_function(name = "foo", cfg(target_pointer_width = "32"))] //~ ERROR expected key = value
fn foo() -> LispObject {
    LispObject::constant_nil()
}

fn main() {
    return;
}
