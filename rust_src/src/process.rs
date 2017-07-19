use remacs_macros::lisp_fn;
use lisp::{LispObject, Qnil};
use lists::{assoc, cdr};

static mut Vprocess_alist: LispObject = Qnil;

#[lisp_fn]
fn get_process(name: LispObject) -> LispObject {
    if name.is_process() {
        name
    } else if name.is_string() {
        unsafe {
            cdr(assoc(name, Vprocess_alist, Qnil))
        }
    } else {
        Qnil
    }
}
