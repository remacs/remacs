//! Emacs!

use lisp::{defsubr, LispObject};
use remacs_macros::lisp_fn;
use remacs_sys::globals;
use remacs_sys::Fcopy_sequence;

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

include!(concat!(env!("OUT_DIR"), "/emacs_exports.rs"));
