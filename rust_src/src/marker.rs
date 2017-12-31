//! marker support

use libc::ptrdiff_t;
use std::mem;

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Lisp_Marker};
use remacs_sys::{buf_charpos_to_bytepos, set_point_both};

use buffers::LispBufferRef;
use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use threads::ThreadState;
use util::clip_to_bounds;

pub type LispMarkerRef = ExternalPtr<Lisp_Marker>;

impl LispMarkerRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(self.as_ptr()) }
    }

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

    pub fn bytepos_or_error(self) -> ptrdiff_t {
        if self.buffer.is_null() {
            error!("Marker does not point anywhere");
        } else {
            self.bytepos
        }
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
pub fn markerp(object: LispObject) -> bool {
    object.is_marker()
}

/// Return the position of MARKER, or nil if it points nowhere.
#[lisp_fn]
pub fn marker_position(marker: LispMarkerRef) -> Option<EmacsInt> {
    if let Some(p) = marker.charpos() {
        Some(p as EmacsInt)
    } else {
        None
    }
}

/// Return the buffer that MARKER points into, or nil if none.
/// Returns nil if MARKER points into a dead buffer.
#[lisp_fn]
pub fn marker_buffer(marker: LispMarkerRef) -> Option<LispBufferRef> {
    marker.buffer()
}

/// Set PT from MARKER's clipped position.
pub fn set_point_from_marker(marker: LispMarkerRef) {
    let cur_buf = ThreadState::current_buffer();
    let charpos = clip_to_bounds(
        cur_buf.begv,
        marker.charpos_or_error() as EmacsInt,
        cur_buf.zv,
    );
    let mut bytepos = marker.bytepos_or_error();
    // Don't trust the byte position if the marker belongs to a
    // different buffer.
    if marker.buffer().map_or(false, |b| b != cur_buf) {
        bytepos = unsafe { buf_charpos_to_bytepos(cur_buf.as_ptr(), charpos) };
    } else {
        bytepos = clip_to_bounds(cur_buf.begv_byte, bytepos as EmacsInt, cur_buf.zv_byte);
    };
    unsafe { set_point_both(charpos, bytepos) };
}

include!(concat!(env!("OUT_DIR"), "/marker_exports.rs"));
