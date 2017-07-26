//! Functions operating on windows.

use lisp::{LispObject, ExternalPtr};
use remacs_macros::lisp_fn;
use remacs_sys::window;

pub type LispWindowRef = ExternalPtr<window>;

impl LispWindowRef {
    // Check if window is live
    #[inline]
    pub fn is_live(self) -> bool {
        LispObject::from_raw(self.contents).is_buffer()
    }
}

/// Return t if OBJECT is a window and nil otherwise.
#[lisp_fn]
fn windowp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_window())
}

/// Return t if OBJECT is a live window and nil otherwise.
///
/// A live window is a window that displays a buffer.
/// Internal windows and deleted windows are not live.
#[lisp_fn]
pub fn window_live_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_window().map_or(false, |m| m.is_live()))
}
