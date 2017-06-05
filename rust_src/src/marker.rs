extern crate libc;

use std::ptr;
use lisp::{LispObject, LispMiscType};
use remacs_sys::error;
use remacs_macros::lisp_fn;

// TODO: write a docstring based on the docs in lisp.h.
#[repr(C)]
pub struct LispMarker {
    ty: LispMiscType,
    // GC mark bit, 13 bits spacer, needs_adjustment flag,
    // insertion_type flag.
    padding: u16,
    // TODO: define a proper buffer struct.
    buffer: *const libc::c_void,
    next: *const LispMarker,
    charpos: libc::ptrdiff_t,
    bytepos: libc::ptrdiff_t,
}

/// Return the char position of marker MARKER, as a C integer.
pub fn marker_position(m_ptr: *const LispMarker) -> libc::ptrdiff_t {
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
