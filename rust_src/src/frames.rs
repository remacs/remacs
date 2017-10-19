//! Generic frame functions.

use remacs_macros::lisp_fn;
use remacs_sys::{Lisp_Frame, selected_frame as current_frame};
use lisp::{LispObject, ExternalPtr};

pub type LispFrameRef = ExternalPtr<Lisp_Frame>;


/// Return the frame that is now selected.
#[lisp_fn]
pub fn selected_frame() -> LispObject {
    unsafe { LispObject::from(current_frame) }
}
