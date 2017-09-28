//! Functions operating on process.

use remacs_macros::lisp_fn;
use remacs_sys::{Lisp_Process, Vprocess_alist};
use lisp::{LispObject, ExternalPtr};
use lists::{assoc, cdr};

pub type LispProcessRef = ExternalPtr<Lisp_Process>;

/// Return t if OBJECT is a process.
#[lisp_fn]
pub fn processp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_process())
}

/// Return the process named NAME, or nil if there is none.
#[lisp_fn]
fn get_process(name: LispObject) -> LispObject {
    if name.is_process() {
        name
    } else {
        name.as_string_or_error();
        cdr(assoc(
            name,
            LispObject::from_raw(unsafe { Vprocess_alist }),
            LispObject::constant_nil(),
        ))
    }
}
