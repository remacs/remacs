extern crate libc;

use std::ptr;
use lisp::{LispObject, LispMiscType, XMARKER, CHECK_TYPE};

extern "C" {
    // defined in eval.c, where it can actually take an arbitrary
    // number of arguments.
    // TODO: define a Rust version of this that uses Rust strings.
    fn error(m: *const u8, ...);
    pub static Qmarkerp: LispObject;
}

/// Raise an error if `x` is not marker.
#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn CHECK_MARKER(x: LispObject) {
    CHECK_TYPE(x.is_marker(), unsafe { Qmarkerp }, x)
}

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
pub fn marker_position(marker: LispObject) -> libc::ptrdiff_t {
    let m_ptr = XMARKER(marker);
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

fn markerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker())
}

defun!("markerp",
       Fmarkerp(object),
       Smarkerp,
       markerp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a marker (editor pointer).

(fn OBJECT)");
