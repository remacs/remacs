#![feature(plugin)]
#![plugin(remacs_codegen)]

#[path = "../../lisp-mock/lib.rs"]
pub mod remacs;
#[path = "../../lisp-mock/lib.rs"]
pub mod remacs_sys;
use remacs::lisp::LispObject;

#[lisp_function(name = "foo", min = 1024)] //~ ERROR `min` should be less or equal to 255
fn foo() -> LispObject {
    LispObject::constant_nil()
}

fn main() {
    return;
}
