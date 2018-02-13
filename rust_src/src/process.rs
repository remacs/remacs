//! Functions operating on process.

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Lisp_Process, Vprocess_alist};
use remacs_sys::{get_process as cget_process, pget_kill_without_query, pget_pid,
                 pget_raw_status_new, pset_kill_without_query, send_process,
                 setup_process_coding_systems, update_status, Fmapcar, STRING_BYTES};
use remacs_sys::{QCbuffer, Qcdr, Qclosed, Qexit, Qlistp, Qnetwork, Qopen, Qpipe, Qrun, Qserial,
                 Qstop};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

use buffers::get_buffer;
use lists::{assoc, cdr, plist_put};
use multibyte::LispStringRef;
use std::mem;

pub type LispProcessRef = ExternalPtr<Lisp_Process>;

impl LispProcessRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(self.as_ptr()) }
    }

    #[inline]
    fn name(self) -> LispObject {
        LispObject::from_raw(self.name)
    }

    #[inline]
    fn tty_name(self) -> LispObject {
        LispObject::from_raw(self.tty_name)
    }

    #[inline]
    fn command(self) -> LispObject {
        LispObject::from_raw(self.command)
    }

    #[inline]
    fn mark(self) -> LispObject {
        LispObject::from_raw(self.mark)
    }

    #[inline]
    fn filter(self) -> LispObject {
        LispObject::from_raw(self.filter)
    }

    #[inline]
    fn sentinel(self) -> LispObject {
        LispObject::from_raw(self.sentinel)
    }

    #[inline]
    fn plist(self) -> LispObject {
        LispObject::from_raw(self.plist)
    }

    #[inline]
    fn buffer(self) -> LispObject {
        LispObject::from_raw(self.buffer)
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
pub fn processp(object: LispObject) -> bool {
    object.is_process()
}

/// Return the process named NAME, or nil if there is none.
#[lisp_fn]
pub fn get_process(name: LispObject) -> LispObject {
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
pub fn process_name(process: LispProcessRef) -> LispObject {
    process.name()
}

/// Return the buffer PROCESS is associated with.
/// The default process filter inserts output from PROCESS into this buffer.
#[lisp_fn]
pub fn process_buffer(process: LispProcessRef) -> LispObject {
    process.buffer()
}

/// Return the process id of PROCESS.
/// This is the pid of the external process which PROCESS uses or talks to.
/// For a network, serial, and pipe connections, this value is nil.
#[lisp_fn]
pub fn process_id(process: LispObject) -> Option<EmacsInt> {
    let pid = unsafe { pget_pid(process.as_process_or_error().as_ptr()) };
    if pid != 0 {
        Some(EmacsInt::from(pid))
    } else {
        None
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
    for tail in LispObject::from_raw(unsafe { Vprocess_alist }).iter_tails() {
        let p = tail.car().as_cons().unwrap().cdr();
        if buf.eq(p.as_process().unwrap().buffer()) {
            return p;
        }
    }
    LispObject::constant_nil()
}

/// Return the name of the terminal PROCESS uses, or nil if none.
/// This is the terminal that the process itself reads and writes on,
/// not the name of the pty that Emacs uses to talk with that terminal.
#[lisp_fn]
pub fn process_tty_name(process: LispProcessRef) -> LispObject {
    process.tty_name()
}

/// Return the command that was executed to start PROCESS.  This is a
/// list of strings, the first string being the program executed and
/// the rest of the strings being the arguments given to it.  For a
/// network or serial or pipe connection, this is nil (process is
/// running) or t (process is stopped).
#[lisp_fn]
pub fn process_command(process: LispProcessRef) -> LispObject {
    process.command()
}

/// Return the filter function of PROCESS.
/// See `set-process-filter' for more info on filter functions.
#[lisp_fn]
pub fn process_filter(process: LispProcessRef) -> LispObject {
    process.filter()
}

/// Return the sentinel of PROCESS.
/// See `set-process-sentinel' for more info on sentinels.
#[lisp_fn]
pub fn process_sentinel(process: LispProcessRef) -> LispObject {
    process.sentinel()
}

/// Return the marker for the end of the last output from PROCESS.
#[lisp_fn]
pub fn process_mark(process: LispProcessRef) -> LispObject {
    process.mark()
}

/// Return a list of all processes that are Emacs sub-processes.
#[lisp_fn]
pub fn process_list() -> LispObject {
    LispObject::from_raw(unsafe { Fmapcar(Qcdr, Vprocess_alist) })
}

/// Return the plist of PROCESS.
#[lisp_fn]
pub fn process_plist(process: LispProcessRef) -> LispObject {
    process.plist()
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
        LispObject::from_raw(unsafe { cget_process(process.to_raw()) })
    };
    if p.is_nil() {
        return p;
    }
    let p_ref = p.as_process_or_error();
    if unsafe { pget_raw_status_new(p_ref.as_ptr()) } != 0 {
        unsafe { update_status(p_ref.as_ptr()) };
    }
    let mut status = LispObject::from_raw(p_ref.status);
    if let Some(c) = status.as_cons() {
        status = c.car();
    };
    let process_type = LispObject::from_raw(p_ref.process_type);
    if process_type.eq(LispObject::from_raw(Qnetwork))
        || process_type.eq(LispObject::from_raw(Qserial))
        || process_type.eq(LispObject::from_raw(Qpipe))
    {
        let process_command = LispObject::from_raw(p_ref.command);
        if status.eq(LispObject::from_raw(Qexit)) {
            status = LispObject::from_raw(Qclosed);
        } else if process_command.eq(LispObject::constant_t()) {
            status = LispObject::from_raw(Qstop);
        } else if status.eq(LispObject::from_raw(Qrun)) {
            status = LispObject::from_raw(Qopen);
        }
    }
    status
}

/// Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
/// Return BUFFER.
#[lisp_fn]
pub fn set_process_buffer(process: LispObject, buffer: LispObject) -> LispObject {
    let mut p_ref = process.as_process_or_error();
    if buffer.is_not_nil() {
        buffer.as_buffer_or_error();
    }
    p_ref.set_buffer(buffer);
    let process_type = LispObject::from_raw(p_ref.process_type);
    if process_type.eq(LispObject::from_raw(Qnetwork))
        || process_type.eq(LispObject::from_raw(Qserial))
        || process_type.eq(LispObject::from_raw(Qpipe))
    {
        let childp = LispObject::from_raw(p_ref.childp);
        p_ref.set_childp(plist_put(childp, LispObject::from_raw(QCbuffer), buffer));
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
pub fn process_send_string(process: LispObject, string: LispStringRef) -> () {
    unsafe {
        send_process(
            cget_process(process.to_raw()),
            string.data,
            STRING_BYTES(string.as_ptr()),
            string.as_lisp_obj().to_raw(),
        )
    };
}

/// Return the current value of query-on-exit flag for PROCESS.
#[lisp_fn]
pub fn process_query_on_exit_flag(process: LispProcessRef) -> bool {
    unsafe { !pget_kill_without_query(process.as_ptr()) }
}

/// Specify if query is needed for PROCESS when Emacs is exited.
/// If the second argument FLAG is non-nil, Emacs will query the user before
/// exiting or killing a buffer if PROCESS is running.  This function
/// returns FLAG.
#[lisp_fn]
pub fn set_process_query_on_exit_flag(mut process: LispProcessRef, flag: LispObject) -> LispObject {
    unsafe {
        pset_kill_without_query(process.as_mut(), flag.is_nil());
    }
    flag
}

include!(concat!(env!("OUT_DIR"), "/process_exports.rs"));
