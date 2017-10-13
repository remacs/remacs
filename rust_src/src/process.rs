//! Functions operating on process.

use remacs_macros::lisp_fn;
use remacs_sys::{Lisp_Process, Vprocess_alist};
use lisp::{LispObject, ExternalPtr};
use lists::{assoc, cdr};
use buffers::get_buffer;

pub type LispProcessRef = ExternalPtr<Lisp_Process>;

impl LispProcessRef {
    #[inline]
    fn name(&self) -> LispObject {
        LispObject::from_raw(self.name)
    }

    #[inline]
    fn buffer(&self) -> LispObject {
        LispObject::from_raw(self.buffer)
    }
}

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

/// Return the name of PROCESS, as a string.
/// This is the name of the program invoked in PROCESS,
/// possibly modified to make it unique among process names.
#[lisp_fn]
fn process_name(process: LispObject) -> LispObject {
    process.as_process_or_error().name()
}

/// Return the (or a) live process associated with BUFFER.
/// BUFFER may be a buffer or the name of one.
/// Return nil if all processes associated with BUFFER have been
/// deleted or killed.
#[lisp_fn]
pub fn get_buffer_process(buffer: LispObject) -> LispObject {
    if buffer.is_nil() {
        return LispObject::constant_nil();
    }
    let buf = get_buffer(buffer);
    if buf.is_nil() {
        return LispObject::constant_nil();
    }
    for tail in LispObject::from_raw(unsafe { Vprocess_alist }).iter_tails() {
        let p = tail.car().as_cons().unwrap().cdr();
        if buf.eq(p.as_process().unwrap().buffer()) {
            return p;
        }
    }
    return LispObject::constant_nil();
}
