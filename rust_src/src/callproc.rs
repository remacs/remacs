//! Synchronous subprocess invocation for GNU Emacs.

use libc::O_RDONLY;

use crate::{
    buffers,
    coding::encode_file_name,
    eval::{record_unwind_protect_int, unbind_to},
    fileio::expand_file_name,
    lisp::LispObject,
    remacs_macros::lisp_fn,
    remacs_sys::Fdelete_region,
    remacs_sys::Qnil,
    remacs_sys::NULL_DEVICE,
    remacs_sys::{
        build_string, call_process, close_file_unwind, create_temp_file, emacs_open,
        report_file_error,
    },
    threads::{c_specpdl_index, ThreadState},
};

/// Call PROGRAM synchronously in separate process.
/// The remaining arguments are optional.
/// The program's input comes from file INFILE (nil means `/dev/null').
/// Insert output in DESTINATION before point; t means current buffer; nil for DESTINATION
/// means discard it; 0 means discard and don't wait; and `(:file FILE)', where
/// FILE is a file name string, means that it should be written to that file
/// (if the file already exists it is overwritten).
/// DESTINATION can also have the form (REAL-BUFFER STDERR-FILE); in that case,
/// REAL-BUFFER says what to do with standard output, as above,
/// while STDERR-FILE says what to do with standard error in the child.
/// STDERR-FILE may be nil (discard standard error output),
/// t (mix it with ordinary output), or a file name string.
///
/// Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
/// Remaining arguments are strings passed as command arguments to PROGRAM.
///
/// If executable PROGRAM can't be found as an executable, `call-process'
/// signals a Lisp error.  `call-process' reports errors in execution of
/// the program only through its return and output.
///
/// If DESTINATION is 0, `call-process' returns immediately with value nil.
/// Otherwise it waits for PROGRAM to terminate
/// and returns a numeric exit status or a signal description string.
/// If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.
///
/// The process runs in `default-directory' if that is local (as
/// determined by `unhandled-file-name-directory'), or "~" otherwise.  If
/// you want to run a process in a remote directory use `process-file'.
///
/// usage: (call-process PROGRAM &optional INFILE DESTINATION DISPLAY &rest ARGS)
#[lisp_fn(min = "1", name = "call-process", c_name = "call_process")]
pub fn call_process_lisp(args: &mut [LispObject]) -> LispObject {
    let count = c_specpdl_index();

    let infile = if args.len() >= 2 && args[1].is_not_nil() {
        expand_file_name(
            args[1].into(),
            ThreadState::current_buffer_unchecked().directory_.into(),
        )
    } else {
        unsafe { build_string(NULL_DEVICE.as_ptr() as *const i8) }.into()
    };

    let encoded_file = encode_file_name(infile);

    let filefd = unsafe {
        emacs_open(
            encoded_file.const_data_ptr() as *const i8,
            libc::O_RDONLY,
            0,
        )
    };

    if filefd < 0 {
        unsafe {
            report_file_error(
                "Opening process input file".as_ptr() as *const i8,
                infile.into(),
            )
        };
    }

    unsafe { record_unwind_protect_int(Some(close_file_unwind), filefd) };

    unbind_to(count, unsafe {
        call_process(
            args.len() as isize,
            args.as_mut_ptr() as *mut LispObject,
            filefd,
            -1,
        )
    })
}

/// Send text from START to END to a synchronous process running PROGRAM.
///
/// START and END are normally buffer positions specifying the part of the
/// buffer to send to the process.
/// If START is nil, that means to use the entire buffer contents; END is
/// ignored.
/// If START is a string, then send that string to the process
/// instead of any buffer contents; END is ignored.
/// The remaining arguments are optional.
/// Delete the text if fourth arg DELETE is non-nil.
///
/// Insert output in BUFFER before point; t means current buffer; nil for
/// BUFFER means discard it; 0 means discard and don't wait; and `(:file
/// FILE)', where FILE is a file name string, means that it should be
/// written to that file (if the file already exists it is overwritten).
/// BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
/// REAL-BUFFER says what to do with standard output, as above,
/// while STDERR-FILE says what to do with standard error in the child.
/// STDERR-FILE may be nil (discard standard error output),
/// t (mix it with ordinary output), or a file name string.
///
/// Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
/// Remaining args are passed to PROGRAM at startup as command args.
///
/// If BUFFER is 0, `call-process-region' returns immediately with value nil.
/// Otherwise it waits for PROGRAM to terminate
/// and returns a numeric exit status or a signal description string.
/// If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.
///
/// usage: (call-process-region START END PROGRAM &optional DELETE BUFFER DISPLAY &rest ARGS)
#[lisp_fn(min = "3")]
pub fn call_process_region(args: &mut [LispObject]) -> LispObject {
    let mut start = args[0];
    let mut end = args[1];
    let mut infile = Qnil;
    let spec = c_specpdl_index();

    let empty_input = if let Some(string) = start.as_string() {
        string.is_empty()
    } else if start.is_nil() {
        let buffer = ThreadState::current_buffer_unchecked();
        buffer.beg() == buffer.z()
    } else {
        let (start_1, end_1) = buffers::validate_region_rust(args[0], args[1]);
        start = start_1.into();
        end = end_1.into();
        args[0] = start;
        args[1] = end;

        start_1 == end_1
    };

    let fd = unsafe {
        if !empty_input {
            create_temp_file(args.len() as isize, args.as_mut_ptr(), &mut infile)
        } else {
            let fd = emacs_open(NULL_DEVICE.as_ptr() as *const i8, O_RDONLY, 0);
            if fd < 0 {
                report_file_error("opening null device".as_ptr() as *const i8, Qnil);
            }
            record_unwind_protect_int(Some(close_file_unwind), fd);
            fd
        }
    };

    if args.len() > 3 && args[3].is_not_nil() {
        unsafe { Fdelete_region(start, end) };
    }

    let args = if args.len() > 3 {
        &mut args[2..]
    } else {
        args[0] = args[2];
        &mut args[..2]
    };
    args[1] = infile;

    let count = if empty_input { -1 } else { spec };
    let exit = unsafe { call_process(args.len() as isize, args.as_mut_ptr(), fd, count) };
    unbind_to(spec, exit)
}

include!(concat!(env!("OUT_DIR"), "/callproc_exports.rs"));
