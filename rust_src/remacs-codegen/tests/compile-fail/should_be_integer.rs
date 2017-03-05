#![feature(plugin)]
#![plugin(remacs_codegen)]

#[path = "../../lisp-mock/lib.rs"]
pub mod remacs;
#[path = "../../lisp-mock/lib.rs"]
pub mod remacs_sys;
use remacs::lisp::LispObject;

#[lisp_function(name = "foo", min = "1")] //~ ERROR `min` value must be an integer
fn foo() -> LispObject {
    LispObject::constant_nil()
}

fn main() {
    return;
}
