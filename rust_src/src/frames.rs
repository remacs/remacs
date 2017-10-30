//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::{selected_frame as current_frame, Lisp_Frame};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

pub type LispFrameRef = ExternalPtr<Lisp_Frame>;

/// Return the frame that is now selected.
#[lisp_fn]
pub fn selected_frame() -> LispObject {
    unsafe { LispObject::from(current_frame) }
}

pub extern "C" fn rust_init_syms() {
    unsafe {
        defsubr(&*Sselected_frame);
    }
}
