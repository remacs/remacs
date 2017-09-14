use libc::{c_void, ptrdiff_t};
use std::mem;

use remacs_sys::make_lisp_ptr;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::{Lisp_Misc_Type, Lisp_Marker, EmacsInt, Lisp_Type};
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

/// Return a newly allocated marker which points into BUF at character
/// position CHARPOS and byte position BYTEPOS.
pub fn build_marker(buf: LispBufferRef, charpos: ptrdiff_t, byte_pos: ptrdiff_t) -> Lisp_Marker {
    debug_assert!(buf.is_live());
    debug_assert!(charpos <= byte_pos);

    let m = Lisp_Marker{
        ty: Lisp_Misc_Type::Marker,
        padding: 0,
        buffer: buf,
        charpos: charpos,
        bytepos: byte_pos,
        next: unsafe{ *buf.text }.markers,
    };
    unsafe{ *buf.text }.markers = m;
    // (buf.text).markers = m;
    m
}

/// Return t if OBJECT is a marker (editor pointer).
#[lisp_fn]
fn markerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker())
}

/// Return the position of MARKER, or nil if it points nowhere.
#[lisp_fn]
pub fn marker_position(marker: LispObject) -> LispObject {
    let pos = marker.as_marker_or_error().charpos();
    match pos {
        Some(p) => LispObject::from_natnum(p as EmacsInt),
        None => LispObject::constant_nil(),
    }
}

/// Return the buffer that MARKER points into, or nil if none.
/// Returns nil if MARKER points into a dead buffer.
#[lisp_fn]
pub fn marker_buffer(marker: LispObject) -> LispObject {
    let buf = marker.as_marker_or_error().buffer();
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
