//! Functions operating on process.
use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{add_process_read_fd, current_thread, delete_read_fd, emacs_get_tty_pgrp,
                 get_process as cget_process, send_process, setup_process_coding_systems,
                 update_status, Fmapcar, STRING_BYTES};
use remacs_sys::{pvec_type, EmacsInt, Lisp_Process, Lisp_Type, Vprocess_alist};
use remacs_sys::{QCbuffer, QCfilter, QCsentinel, Qcdr, Qclosed, Qexit,
                 Qinternal_default_process_filter, Qinternal_default_process_sentinel, Qlisten,
                 Qlistp, Qnetwork, Qnil, Qopen, Qpipe, Qprocessp, Qreal, Qrun, Qserial, Qstop, Qt};

use lisp::defsubr;
use lisp::{ExternalPtr, LispObject};

use buffers::get_buffer;
use lists::{assoc, car, cdr, plist_put};
use multibyte::LispStringRef;

pub type LispProcessRef = ExternalPtr<Lisp_Process>;

impl LispProcessRef {
    pub fn as_lisp_obj(self) -> LispObject {
        LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
    }

    fn ptype(self) -> LispObject {
        self.type_
    }

    fn set_plist(&mut self, plist: LispObject) {
        self.plist = plist;
    }

    fn set_buffer(&mut self, buffer: LispObject) {
        self.buffer = buffer;
    }

    fn set_childp(&mut self, childp: LispObject) {
        self.childp = childp;
    }
}

impl LispObject {
    pub fn is_process(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_PROCESS))
    }

    pub fn as_process(self) -> Option<LispProcessRef> {
        self.as_vectorlike().and_then(|v| v.as_process())
    }

    pub fn as_process_or_error(self) -> LispProcessRef {
        self.as_process()
            .unwrap_or_else(|| wrong_type!(Qprocessp, self))
    }
}

impl From<LispObject> for LispProcessRef {
    fn from(o: LispObject) -> Self {
        o.as_process_or_error()
    }
}

impl From<LispProcessRef> for LispObject {
    fn from(p: LispProcessRef) -> Self {
        p.as_lisp_obj()
    }
}

impl From<LispObject> for Option<LispProcessRef> {
    fn from(o: LispObject) -> Self {
        o.as_process()
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
        cdr(assoc(name, unsafe { Vprocess_alist }, Qnil))
    }
}

/// Return the name of PROCESS, as a string.
/// This is the name of the program invoked in PROCESS,
/// possibly modified to make it unique among process names.
#[lisp_fn]
pub fn process_name(process: LispProcessRef) -> LispObject {
    process.name
}

/// Return the buffer PROCESS is associated with.
/// The default process filter inserts output from PROCESS into this buffer.
#[lisp_fn]
pub fn process_buffer(process: LispProcessRef) -> LispObject {
    process.buffer
}

/// Return the process id of PROCESS.
/// This is the pid of the external process which PROCESS uses or talks to.
/// For a network, serial, and pipe connections, this value is nil.
#[lisp_fn]
pub fn process_id(process: LispProcessRef) -> Option<EmacsInt> {
    if process.pid != 0 {
        Some(EmacsInt::from(process.pid))
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
        return Qnil;
    }
    let buf = get_buffer(buffer);
    if buf.is_nil() {
        return Qnil;
    }
    for tail in unsafe { Vprocess_alist }.iter_tails() {
        let p = tail.car().as_cons_or_error().cdr();
        if buf.eq(p.as_process_or_error().buffer) {
            return p;
        }
    }
    Qnil
}

/// Return the name of the terminal PROCESS uses, or nil if none.
/// This is the terminal that the process itself reads and writes on,
/// not the name of the pty that Emacs uses to talk with that terminal.
#[lisp_fn]
pub fn process_tty_name(process: LispProcessRef) -> LispObject {
    process.tty_name
}

/// Return the command that was executed to start PROCESS.  This is a
/// list of strings, the first string being the program executed and
/// the rest of the strings being the arguments given to it.  For a
/// network or serial or pipe connection, this is nil (process is
/// running) or t (process is stopped).
#[lisp_fn]
pub fn process_command(process: LispProcessRef) -> LispObject {
    process.command
}

/// Return the filter function of PROCESS.
/// See `set-process-filter' for more info on filter functions.
#[lisp_fn]
pub fn process_filter(process: LispProcessRef) -> LispObject {
    process.filter
}

/// Return the sentinel of PROCESS.
/// See `set-process-sentinel' for more info on sentinels.
#[lisp_fn]
pub fn process_sentinel(process: LispProcessRef) -> LispObject {
    process.sentinel
}

/// Return the marker for the end of the last output from PROCESS.
#[lisp_fn]
pub fn process_mark(process: LispProcessRef) -> LispObject {
    process.mark
}

/// Return a list of all processes that are Emacs sub-processes.
#[lisp_fn]
pub fn process_list() -> LispObject {
    unsafe { Fmapcar(Qcdr, Vprocess_alist) }
}

/// Return the plist of PROCESS.
#[lisp_fn]
pub fn process_plist(process: LispProcessRef) -> LispObject {
    process.plist
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
        unsafe { cget_process(process) }
    };
    if p.is_nil() {
        return p;
    }
    let mut process = p.as_process_or_error();
    if process.raw_status_new() {
        unsafe { update_status(process.as_mut()) };
    }
    let mut status = process.status;
    if let Some(c) = status.as_cons() {
        status = c.car();
    };
    let process_type = process.ptype();
    if process_type.eq(Qnetwork) || process_type.eq(Qserial) || process_type.eq(Qpipe) {
        let process_command = process.command;
        if status.eq(Qexit) {
            status = Qclosed;
        } else if process_command.eq(Qt) {
            status = Qstop;
        } else if status.eq(Qrun) {
            status = Qopen;
        }
    }
    status
}

/// Return a cons of coding systems for decoding and encoding of PROCESS.
#[lisp_fn]
pub fn process_coding_system(process: LispProcessRef) -> LispObject {
    LispObject::cons(process.decode_coding_system, process.encode_coding_system)
}

/// Return the locking thread of PROCESS.
/// If PROCESS is unlocked, this function returns nil.
#[lisp_fn]
pub fn process_thread(process: LispProcessRef) -> LispObject {
    process.thread
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
    let process_type = p_ref.ptype();
    let netconn1_p = process_type.eq(Qnetwork);
    let serialconn1_p = process_type.eq(Qserial);
    let pipeconn1_p = process_type.eq(Qpipe);

    if netconn1_p || serialconn1_p || pipeconn1_p {
        let childp = p_ref.childp;
        p_ref.set_childp(plist_put(childp, QCbuffer, buffer));
    }
    unsafe { setup_process_coding_systems(process) };
    buffer
}

/// Give PROCESS the filter function FILTER; nil means default.
/// A value of t means stop accepting output from the process.
///
/// When a process has a non-default filter, its buffer is not used for output.
/// Instead, each time it does output, the entire string of output is
/// qpassed to the filter.
///
/// The filter gets two arguments: the process and the string of output.
/// The string argument is normally a multibyte string, except:
/// - if the process's input coding system is no-conversion or raw-text,
///   it is a unibyte string (the non-converted input), or else
/// - if `default-enable-multibyte-characters' is nil, it is a unibyte
///   string (the result of converting the decoded input multibyte
///   string to unibyte with `string-make-unibyte').
#[lisp_fn]
pub fn set_process_filter(process: LispObject, mut filter: LispObject) -> LispObject {
    let mut p_ref = process.as_process_or_error();

    // Don't signal an error if the process's input file descriptor
    // is closed.  This could make debugging Lisp more difficult,
    // for example when doing something like
    //
    // (setq process (start-process ...))
    // (debug)
    // (set-process-filter process ...)
    filter = pset_filter(p_ref, filter);
    set_process_filter_masks(p_ref);

    let process_type = p_ref.ptype();
    let netconn1_p = process_type.eq(Qnetwork);
    let serialconn1_p = process_type.eq(Qserial);
    let pipeconn1_p = process_type.eq(Qpipe);

    if netconn1_p || serialconn1_p || pipeconn1_p {
        let childp = p_ref.childp;
        p_ref.set_childp(plist_put(childp, QCfilter, filter));
    }
    unsafe { setup_process_coding_systems(process) };
    filter
}

fn pset_filter(mut process: LispProcessRef, val: LispObject) -> LispObject {
    let filter = if val.is_nil() {
        Qinternal_default_process_filter
    } else {
        val
    };
    process.filter = filter;
    filter
}

fn set_process_filter_masks(process: LispProcessRef) -> () {
    if process.infd != -1 && process.filter.eq(Qt) {
        if process.status.ne(Qlisten) {
            unsafe { delete_read_fd(process.infd) };
        // Network or serial process not stopped:
        } else if process.command.eq(Qt) {
            unsafe { add_process_read_fd(process.infd) };
        }
    }
}

/// Give PROCESS the sentinel SENTINEL; nil for default.
/// The sentinel is called as a function when the process changes state.
/// It gets two arguments: the process, and a string describing the change.
#[lisp_fn]
pub fn set_process_sentinel(mut process: LispProcessRef, mut sentinel: LispObject) -> LispObject {
    sentinel = pset_sentinel(process, sentinel);
    let process_type = process.ptype();
    let netconn1_p = process_type.eq(Qnetwork);
    let serialconn1_p = process_type.eq(Qserial);
    let pipeconn1_p = process_type.eq(Qpipe);

    if netconn1_p || serialconn1_p || pipeconn1_p {
        let childp = process.childp;
        process.set_childp(plist_put(childp, QCsentinel, sentinel));
    }
    sentinel
}

fn pset_sentinel(mut process: LispProcessRef, val: LispObject) -> LispObject {
    let sentinel = if val.is_nil() {
        Qinternal_default_process_sentinel
    } else {
        val
    };
    process.sentinel = sentinel;
    sentinel
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
pub fn process_send_string(process: LispObject, mut string: LispStringRef) -> () {
    unsafe {
        send_process(
            cget_process(process),
            string.data as *mut libc::c_char,
            STRING_BYTES(string.as_mut()),
            string.as_lisp_obj(),
        )
    };
}

/// Return the current value of query-on-exit flag for PROCESS.
#[lisp_fn]
pub fn process_query_on_exit_flag(process: LispProcessRef) -> bool {
    !process.kill_without_query()
}

/// Specify if query is needed for PROCESS when Emacs is exited.
/// If the second argument FLAG is non-nil, Emacs will query the user before
/// exiting or killing a buffer if PROCESS is running.  This function
/// returns FLAG.
#[lisp_fn]
pub fn set_process_query_on_exit_flag(mut process: LispProcessRef, flag: LispObject) -> LispObject {
    process.set_kill_without_query(flag.is_nil());
    flag
}

/// Return non-nil if Emacs is waiting for input from the user.
/// This is intended for use by asynchronous process output filters and sentinels.
#[lisp_fn]
pub fn waiting_for_user_input_p() -> bool {
    unsafe { (*current_thread).m_waiting_for_user_input_p != 0 }
}

/// Return the value of inherit-coding-system flag for PROCESS. If this flag is
/// t, `buffer-file-coding-system` of the buffer associated with process will
/// inherit the coding system used to decode the process output.
#[lisp_fn]
pub fn process_inherit_coding_system_flag(process: LispProcessRef) -> bool {
    process.inherit_coding_system_flag()
}

/// Return the exit status of PROCESS or the signal number that killed it.
/// If PROCESS has not yet exited or died, return 0.
#[lisp_fn]
pub fn process_exit_status(mut process: LispProcessRef) -> LispObject {
    if process.raw_status_new() {
        unsafe { update_status(process.as_mut()) };
    }
    let status = process.status;
    status
        .as_cons()
        .map_or_else(|| LispObject::from(0), |cons| car(cons.cdr()))
}

/// Return non-nil if PROCESS has given the terminal to a
/// child.  If the operating system does not make it possible to find out,
/// return t.  If we can find out, return the numeric ID of the foreground
/// process group.
#[lisp_fn(min = "0")]
pub fn process_running_child_p(mut process: LispObject) -> LispObject {
    // Initialize in case ioctl doesn't exist or gives an error,
    // in a way that will cause returning t.
    process = get_process(process);
    let mut proc_ref = process.as_process_or_error();

    if proc_ref.ptype().ne(Qreal) {
        error!(
            "Process {} is not a subprocess.",
            proc_ref.name.as_string_or_error()
        );
    }
    if proc_ref.infd < 0 {
        error!(
            "Process {} is not active.",
            proc_ref.name.as_string_or_error()
        );
    }

    let gid = unsafe { emacs_get_tty_pgrp(proc_ref.as_mut()) };

    if gid == proc_ref.pid {
        Qnil
    } else if gid != -1 {
        LispObject::from_fixnum(gid.into())
    } else {
        Qt
    }
}

include!(concat!(env!("OUT_DIR"), "/process_exports.rs"));
