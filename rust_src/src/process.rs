//! Functions operating on process.

use buffers::get_buffer;
use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use lists::{assoc, cdr};
use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Fmapcar, Lisp_Process, Qcdr, Qlistp, Vprocess_alist};
use remacs_sys::pget_pid;

pub type LispProcessRef = ExternalPtr<Lisp_Process>;

impl LispProcessRef {
    #[inline]
    fn name(&self) -> LispObject {
        LispObject::from(self.name)
    }

    #[inline]
    fn buffer(&self) -> LispObject {
        LispObject::from(self.buffer)
    }

    #[inline]
    fn set_plist(&mut self, plist: LispObject) {
        self.plist = plist.to_raw();
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
            LispObject::from(unsafe { Vprocess_alist }),
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

/// Return the buffer PROCESS is associated with.
/// The default process filter inserts output from PROCESS into this buffer.
#[lisp_fn]
fn process_buffer(process: LispObject) -> LispObject {
    process.as_process_or_error().buffer()
}

/// Return the process id of PROCESS.
/// This is the pid of the external process which PROCESS uses or talks to.
/// For a network, serial, and pipe connections, this value is nil.
#[lisp_fn]
fn process_id(process: LispObject) -> LispObject {
    let pid = unsafe { pget_pid(process.as_process_or_error().as_ptr()) };
    if pid != 0 {
        LispObject::from_fixnum(pid as EmacsInt)
    } else {
        LispObject::constant_nil()
    }
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
    for tail in LispObject::from(unsafe { Vprocess_alist }).iter_tails() {
        let p = tail.car().as_cons().unwrap().cdr();
        if buf.eq(p.as_process().unwrap().buffer()) {
            return p;
        }
    }
    return LispObject::constant_nil();
}

/// Return a list of all processes that are Emacs sub-processes.
#[lisp_fn]
pub fn process_list() -> LispObject {
    LispObject::from(unsafe { Fmapcar(Qcdr, Vprocess_alist) })
}

/// Replace the plist of PROCESS with PLIST.  Return PLIST.
#[lisp_fn]
pub fn set_process_plist(process: LispObject, plist: LispObject) -> LispObject {
    if plist.is_list() {
        let mut p = process.as_process_or_error();
        p.set_plist(plist);
        plist
    } else {
        wrong_type!(Qlistp, plist)
    }
}

include!(concat!(env!("OUT_DIR"), "/process_exports.rs"));