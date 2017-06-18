use std::ptr;
use libc::ptrdiff_t;

use lisp::{LispObject, LispMiscType};
use buffers::Lisp_Buffer;
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
    buffer: *const Lisp_Buffer,
    next: *const LispMarker,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
}

/// Return the char position of marker MARKER, as a C integer.
pub fn marker_position(m_ptr: *const LispMarker) -> ptrdiff_t {
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
