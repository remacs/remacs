#![feature(plugin)]
#![plugin(remacs_codegen)]

#[path = "../../lisp-mock/lib.rs"]
pub mod remacs;
#[path = "../../lisp-mock/lib.rs"]
pub mod remacs_sys;
use remacs::lisp::LispObject;

#[lisp_function] //~ ERROR incorrect use of attribute
//~^ ERROR malformed attribute
fn foo() -> LispObject {
    LispObject::constant_nil()
}

fn main() {
    return;
}
