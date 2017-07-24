use libc::ptrdiff_t;

use lisp::{LispObject, ExternalPtr};
use remacs_sys::Lisp_Marker;
use remacs_macros::lisp_fn;

pub type LispMarkerRef = ExternalPtr<Lisp_Marker>;

impl LispMarkerRef {
    /// Return the char position of marker MARKER, as a C integer.
    pub fn position(self) -> ptrdiff_t {
        let buf = self.buffer;
        if buf.is_null() {
            error!("Marker does not point anywhere");
        }

        // TODO: add assertions that marker_position in marker.c has.
        self.charpos
    }
}

/// Return t if OBJECT is a marker (editor pointer).
#[lisp_fn]
fn markerp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_marker())
}
