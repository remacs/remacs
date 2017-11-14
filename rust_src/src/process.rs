//! Functions operating on process.

use remacs_macros::lisp_fn;
use remacs_sys::{BoolBF, EmacsInt, Lisp_Process, QCbuffer, Qcdr, Qclosed, Qexit, Qlistp, Qnetwork,
                 Qopen, Qpipe, Qrun, Qserial, Qstop, Vprocess_alist};
use remacs_sys::{get_process as cget_process, pget_kill_without_query, pget_pid,
                 pget_raw_status_new, pset_kill_without_query, send_process,
                 setup_process_coding_systems, update_status, Fmapcar, STRING_BYTES};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

use buffers::get_buffer;
use lists::{assoc, cdr, plist_put};

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

    #[inline]
    fn set_buffer(&mut self, buffer: LispObject) {
        self.buffer = buffer.to_raw();
    }

    #[inline]
    fn set_childp(&mut self, childp: LispObject) {
        self.childp = childp.to_raw();
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

/// Return the status of PROCESS.
/// The returned value is one of the following symbols:
/// run  -- for a process that is running.
/// stop -- for a process stopped but continuable.
/// exit -- for a process that has exited.
/// signal -- for a process that has got a fatal signal.
/// open -- for a network stream connection that is open.
/// listen -- for a network stream server that is listening.
/// closed -- for a network stream connection that is closed.
/// connect -- when waiting for a non-blocking connection to complete.
/// failed -- when a non-blocking connection has failed.
/// nil -- if arg is a process name and no such process exists.
/// PROCESS may be a process, a buffer, the name of a process, or
/// nil, indicating the current buffer's process.
#[lisp_fn]
pub fn process_status(process: LispObject) -> LispObject {
    let p = if process.is_string() {
        get_process(process)
    } else {
        LispObject::from(unsafe { cget_process(process.to_raw()) })
    };
    if p.is_nil() {
        return p;
    }
    let p_ref = p.as_process_or_error();
    if unsafe { pget_raw_status_new(p_ref.as_ptr()) } != 0 {
        unsafe { update_status(p_ref.as_ptr()) };
    }
    let mut status = LispObject::from(p_ref.status);
    if let Some(c) = status.as_cons() {
        status = LispObject::from(c.car());
    };
    let process_type = LispObject::from(p_ref.process_type);
    if process_type.eq(LispObject::from(Qnetwork)) || process_type.eq(LispObject::from(Qserial))
        || process_type.eq(LispObject::from(Qpipe))
    {
        let process_command = LispObject::from(p_ref.command);
        if status.eq(LispObject::from(Qexit)) {
            status = LispObject::from(Qclosed);
        } else if process_command.eq(LispObject::constant_t()) {
            status = LispObject::from(Qstop);
        } else if status.eq(LispObject::from(Qrun)) {
            status = LispObject::from(Qopen);
        }
    }
    status
}

/// Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
/// Return BUFFER.
#[lisp_fn]
fn set_process_buffer(process: LispObject, buffer: LispObject) -> LispObject {
    let mut p_ref = process.as_process_or_error();
    if buffer.is_not_nil() {
        buffer.as_buffer_or_error();
    }
    p_ref.set_buffer(buffer);
    let process_type = LispObject::from(p_ref.process_type);
    if process_type.eq(LispObject::from(Qnetwork)) || process_type.eq(LispObject::from(Qserial))
        || process_type.eq(LispObject::from(Qpipe))
    {
        let childp = LispObject::from(p_ref.childp);
        p_ref.set_childp(plist_put(
            LispObject::from(childp),
            LispObject::from(QCbuffer),
            buffer,
        ));
    }
    unsafe { setup_process_coding_systems(process.to_raw()) };
    buffer
}

/// Send PROCESS the contents of STRING as input.
/// PROCESS may be a process, a buffer, the name of a process or buffer, or
/// nil, indicating the current buffer's process.
/// If STRING is more than 500 characters long,
/// it is sent in several bunches.  This may happen even for shorter strings.
/// Output from processes can arrive in between bunches.
///
/// If PROCESS is a non-blocking network process that hasn't been fully
/// set up yet, this function will block until socket setup has completed.
#[lisp_fn]
fn process_send_string(process: LispObject, string: LispObject) -> LispObject {
    let s = string.as_string_or_error();
    unsafe {
        send_process(
            cget_process(process.to_raw()),
            s.data,
            STRING_BYTES(s.as_ptr()),
            string.to_raw(),
        )
    };
    LispObject::constant_nil()
}

/// Return the current value of query-on-exit flag for PROCESS.
#[lisp_fn]
pub fn process_query_on_exit_flag(process: LispObject) -> LispObject {
    let kwq = unsafe { pget_kill_without_query(process.as_process_or_error().as_ptr()) };
    LispObject::from_bool(!kwq as BoolBF)
}

/// Specify if query is needed for PROCESS when Emacs is exited.
/// If the second argument FLAG is non-nil, Emacs will query the user before
/// exiting or killing a buffer if PROCESS is running.  This function
/// returns FLAG.
#[lisp_fn]
pub fn set_process_query_on_exit_flag(process: LispObject, flag: LispObject) -> LispObject {
    let p = process.as_process_or_error().as_mut();
    unsafe {
        pset_kill_without_query(p, flag.is_nil());
    }
    flag
}

include!(concat!(env!("OUT_DIR"), "/process_exports.rs"));
