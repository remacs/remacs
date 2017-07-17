use std::ptr;
use libc::ptrdiff_t;

use lisp::LispObject;
use remacs_sys::{error, Lisp_Marker};
use remacs_macros::lisp_fn;

/// Return the char position of marker MARKER, as a C integer.
pub fn marker_position(m_ptr: *const Lisp_Marker) -> ptrdiff_t {
    let m = unsafe { ptr::read(m_ptr) };

    let buf = m.buffer;
    if buf.is_null() {
        unsafe {
            error("Marker does not point anywhere\0".as_ptr());
        }
    }

    // TODO: add assertions that marker_position in marker.c has.

    m.charpos
}

/// Return t if OBJECT is a marker (editor pointer).
#[lisp_fn]
fn markerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker())
}
