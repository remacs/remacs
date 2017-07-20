//! Functions operating on process

use remacs_macros::lisp_fn;
use remacs_sys::Vprocess_alist;
use lisp::LispObject;
use lists::{assoc, cdr};

///Return the process named NAME, or nil if there is none
#[lisp_fn]
fn get_process(name: LispObject) -> LispObject {
    if name.is_process() {
        name
    } else {
        name.as_string_or_error();
        cdr( assoc( name, LispObject::from_raw(unsafe { Vprocess_alist }), LispObject::constant_nil()) )
    }
}
