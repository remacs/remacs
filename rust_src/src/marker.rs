use libc::{c_void, ptrdiff_t};
use std::mem;

use remacs_sys::make_lisp_ptr;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::{Lisp_Marker, EmacsInt, Lisp_Type};
use remacs_macros::lisp_fn;

use buffers::LispBufferRef;

pub type LispMarkerRef = ExternalPtr<Lisp_Marker>;

impl LispMarkerRef {
    pub fn charpos(self) -> Option<ptrdiff_t> {
        if self.buffer.is_null() {
            None
        } else {
            Some(self.charpos)
        }
    }

    pub fn charpos_or_error(self) -> ptrdiff_t {
        if self.buffer.is_null() {
            error!("Marker does not point anywhere");
        }

        // TODO: add assertions that marker_position in marker.c has.
        self.charpos
    }

    pub fn buffer(self) -> Option<LispBufferRef> {
        let buf = self.buffer;
        if buf.is_null() {
            None
        } else {
            Some(unsafe { mem::transmute(buf) })
        }

    }
}

/// Return t if OBJECT is a marker (editor pointer).
#[lisp_fn]
fn markerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker())
}

/// Return the position of MARKER, or nil if it points nowhere.
#[lisp_fn]
fn marker_position(object: LispObject) -> LispObject {
    let pos = object.as_marker_or_error().charpos();
    match pos {
        Some(p) => LispObject::from_natnum(p as EmacsInt),
        None => LispObject::constant_nil(),
    }
}

/// Return the buffer that MARKER points into, or nil if none.
/// Returns nil if MARKER points into a dead buffer.
#[lisp_fn]
fn marker_buffer(object: LispObject) -> LispObject {
    let buf = object.as_marker_or_error().buffer();
    match buf {
        Some(b) => unsafe {
            LispObject::from_raw(make_lisp_ptr(
                b.as_ptr() as *mut c_void,
                Lisp_Type::Lisp_Vectorlike,
            ))
        },
        None => LispObject::constant_nil(),
    }
}
