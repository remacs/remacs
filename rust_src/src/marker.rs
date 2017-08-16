use libc::ptrdiff_t;

use lisp::{LispObject, ExternalPtr};
use remacs_sys::{Lisp_Marker, EmacsInt, Qmarkerp};
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

    pub fn charpos(self) -> Option<ptrdiff_t> {
        let buf = self.buffer;
        if buf.is_null() {
            None
        } else {
            Some(self.charpos)
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
    if object.is_marker() {
        let pos = object.as_marker().unwrap().charpos();
        match pos {
            None => LispObject::constant_nil(),
            _ => LispObject::from_natnum(pos.unwrap() as EmacsInt),
        }
    } else {
        wrong_type!(Qmarkerp, object)
    }
}
