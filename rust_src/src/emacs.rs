//! Emacs!

use cfg_if::cfg_if;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::{build_string, Fcopy_sequence},
    remacs_sys::{daemon_name, globals},
};

/// Replaces IS_DAEMON
cfg_if! {
    if #[cfg(windows)] {
        use crate::remacs_sys::w32_daemon_event;
        pub fn is_daemon() -> bool {
            unsafe { w32_daemon_event != std::ptr::null_mut() }
        }
    } else {
        use crate::remacs_sys::daemon_type;
        pub fn is_daemon() -> bool {
            unsafe { daemon_type != 0 }
        }
    }
}

/// Return the program name that was used to run Emacs.
/// Any directory names are omitted.
#[lisp_fn]
pub fn invocation_name() -> LispObject {
    unsafe { Fcopy_sequence(globals.Vinvocation_name) }
}

/// Return the directory name in which the Emacs executable was located.
#[lisp_fn]
pub fn invocation_directory() -> LispObject {
    unsafe { Fcopy_sequence(globals.Vinvocation_directory) }
}

/// Return non-nil if the current emacs process is a daemon.
/// If the daemon was given a name argument, return that name.
#[lisp_fn]
pub fn daemonp() -> LispObject {
    unsafe {
        if is_daemon() && !daemon_name.is_null() {
            return build_string(daemon_name);
        }

        is_daemon().into()
    }
}

include!(concat!(env!("OUT_DIR"), "/emacs_exports.rs"));
