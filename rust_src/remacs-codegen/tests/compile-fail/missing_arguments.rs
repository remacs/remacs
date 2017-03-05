#![feature(plugin)]
#![plugin(remacs_codegen)]

#[path = "../../lisp-mock/lib.rs"]
pub mod remacs;
#[path = "../../lisp-mock/lib.rs"]
pub mod remacs_sys;
use remacs::lisp::LispObject;

#[lisp_function(name = "foo")] //~ ERROR #[lisp_function(..)] must have at least two paramters
fn foo() -> LispObject {
    LispObject::contant_nil()
}

fn main() {
    return;
}
