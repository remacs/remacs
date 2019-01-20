//! Emacs!

use remacs_macros::lisp_fn;

use crate::{
    lisp::{defsubr, LispObject},
    remacs_sys::globals,
    remacs_sys::Fcopy_sequence,
};

#[cfg(not(feature = "windowsnt"))]
use crate::remacs_sys::daemon_type;
#[cfg(feature = "windowsnt")]
use crate::remacs_sys::w32_daemon_event;

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

pub fn is_daemon() -> bool {
    #[cfg(feature = "windowsnt")]
    return unsafe { w32_daemon_event } != std::ptr::null_mut();
    #[cfg(not(feature = "windowsnt"))]
    return unsafe { daemon_type } != 0;
}

include!(concat!(env!("OUT_DIR"), "/emacs_exports.rs"));
