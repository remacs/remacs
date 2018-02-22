//! marker support

use libc::{c_void, ptrdiff_t};
use std::mem;
use std::ptr;

use remacs_macros::lisp_fn;
use remacs_sys::{BoolBF, EmacsInt, Lisp_Buffer, Lisp_Marker, Lisp_Object};
use remacs_sys::{buf_charpos_to_bytepos, mget_buffer, mget_bytepos, mget_charpos,
                 mget_insertion_type, mget_next_marker, mset_buffer, mset_insertion_type,
                 mset_next_marker, set_marker_internal, set_point_both, unchain_marker,
                 Fmake_marker};

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

    pub fn from_ptr(ptr: *mut c_void) -> Option<LispMarkerRef> {
        unsafe { ptr.as_ref().map(|p| mem::transmute(p)) }
    }

    pub fn charpos(self) -> Option<ptrdiff_t> {
        match self.buffer() {
            None => None,
            Some(_) => unsafe { Some(mget_charpos(self.as_ptr())) },
        }
    }

    pub fn charpos_or_error(self) -> ptrdiff_t {
        match self.buffer() {
            None => error!("Marker does not point anywhere"),
            Some(_) => unsafe { mget_charpos(self.as_ptr()) },
        }
    }

    pub fn bytepos(self) -> Option<ptrdiff_t> {
        match self.buffer() {
            None => None,
            Some(_) => unsafe { Some(mget_bytepos(self.as_ptr())) },
        }
    }

    pub fn bytepos_or_error(self) -> ptrdiff_t {
        match self.buffer() {
            None => error!("Marker does not point anywhere"),
            Some(_) => unsafe { mget_bytepos(self.as_ptr()) },
        }
    }

    pub fn buffer(self) -> Option<LispBufferRef> {
        unsafe {
            let buffer = mget_buffer(self.as_ptr());
            buffer.as_ref().map(|b| mem::transmute(b))
        }
    }

    pub fn set_buffer(self, b: *mut Lisp_Buffer) -> () {
        unsafe { mset_buffer(self.as_ptr(), b) }
    }

    #[inline]
    pub fn insertion_type(self) -> bool {
        unsafe { mget_insertion_type(self.as_ptr()) as BoolBF }
    }

    pub fn iter(self) -> LispMarkerIter {
        LispMarkerIter {
            current: Some(self),
        }
    }

    pub fn next(self) -> Option<LispMarkerRef> {
        unsafe {
            let next = mget_next_marker(self.next);
            next.as_ref().map(|n| mem::transmute(n))
        }
    }

    pub fn set_next(self, m: *mut Lisp_Marker) -> () {
        unsafe { mset_next_marker(self.as_ptr(), m) }
    }
}

pub struct LispMarkerIter {
    current: Option<LispMarkerRef>,
}

impl Iterator for LispMarkerIter {
    type Item = LispMarkerRef;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.current;
        match c {
            None => None,
            Some(m) => {
                self.current = m.next();
                c
            }
        }
    }
}

/// Return t if OBJECT is a marker (editor pointer).
#[lisp_fn]
pub fn markerp(object: LispObject) -> bool {
    object.is_marker()
}

/// Return the position of MARKER, or nil if it points nowhere.
#[lisp_fn(name = "marker-position", c_name = "marker_position")]
pub fn marker_position_lisp(marker: LispMarkerRef) -> Option<EmacsInt> {
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
#[no_mangle]
pub extern "C" fn set_point_from_marker(marker: Lisp_Object) {
    let marker = LispObject::from_raw(marker).as_marker_or_error();
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

/// Return insertion type of MARKER: t if it stays after inserted text.
/// The value nil means the marker stays before text inserted there.
#[lisp_fn]
pub fn marker_insertion_type(marker: LispMarkerRef) -> bool {
    marker.insertion_type()
}

/// Set the insertion-type of MARKER to TYPE.
/// If ITYPE is non-nil, it means the marker advances when you insert text at it.
/// If ITYPE is nil, it means the marker stays behind when you insert text at it.
#[lisp_fn]
pub fn set_marker_insertion_type(mut marker: LispMarkerRef, itype: LispObject) -> LispObject {
    unsafe { mset_insertion_type(marker.as_mut(), itype.is_not_nil() as BoolBF) };
    itype
}

/// Position MARKER before character number POSITION in BUFFER.
/// If BUFFER is omitted or nil, it defaults to the current buffer.  If
/// POSITION is nil, makes marker point nowhere so it no longer slows down
/// editing in any buffer.  Returns MARKER.
#[lisp_fn(min = "2")]
pub fn set_marker(marker: LispObject, position: LispObject, buffer: LispObject) -> LispObject {
    unsafe {
        LispObject::from_raw(set_marker_internal(
            marker.to_raw(),
            position.to_raw(),
            buffer.to_raw(),
            false,
        ))
    }
}

/// Return a new marker pointing at the same place as MARKER.
/// If argument is a number, makes a new marker pointing
/// at that position in the current buffer.
/// If MARKER is not specified, the new marker does not point anywhere.
/// The optional argument ITYPE specifies the insertion type of the new marker;
/// see `marker-insertion-type'.
#[lisp_fn(min = "0")]
pub fn copy_marker(marker: LispObject, itype: LispObject) -> LispObject {
    if marker.is_not_nil() {
        marker.as_fixnum_coerce_marker_or_error();
    }
    let new = unsafe { LispObject::from_raw(Fmake_marker()) };
    let buffer_or_nil = marker
        .as_marker()
        .and_then(|m| m.buffer())
        .map_or(LispObject::constant_nil(), |b| b.as_lisp_obj());

    set_marker(new, marker, buffer_or_nil);
    unsafe {
        mset_insertion_type(
            new.as_marker().unwrap().as_mut(),
            itype.is_not_nil() as BoolBF,
        )
    };
    new
}

/// Return t if there are markers pointing at POSITION in the current buffer.
#[lisp_fn]
pub fn buffer_has_markers_at(position: EmacsInt) -> bool {
    let cur_buf = ThreadState::current_buffer();
    let position = clip_to_bounds(cur_buf.begv, position, cur_buf.zv);

    if let Some(marker) = cur_buf.markers() {
        for m in marker.iter() {
            if m.charpos().map_or(false, |p| p == position) {
                return true;
            }
        }
    }
    false
}

/// Change M so it points to B at CHARPOS and BYTEPOS.
#[no_mangle]
pub extern "C" fn attach_marker(
    marker: *mut Lisp_Marker,
    buffer: *mut Lisp_Buffer,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
) {
    unsafe {
        let mut buffer_ref = LispBufferRef::from_ptr(buffer as *mut c_void)
            .unwrap_or_else(|| panic!("Invalid buffer reference."));

        // In a single-byte buffer, two positions must be equal.
        // Otherwise, every character is at least one byte.
        if buffer_ref.z() == buffer_ref.z_byte() {
            assert!(charpos == bytepos);
        } else {
            assert!(charpos <= bytepos);
        }

        let mut marker_ref = LispMarkerRef::from_ptr(marker as *mut c_void)
            .unwrap_or_else(|| panic!("Invalid marker reference."));

        marker_ref.charpos = charpos;
        marker_ref.bytepos = bytepos;

        if marker_ref.buffer().map_or(true, |b| b != buffer_ref) {
            unchain_marker(marker);
            marker_ref.set_buffer(buffer);
            marker_ref.set_next(
                buffer_ref
                    .markers()
                    .map_or(ptr::null_mut(), |mut m| m.as_mut()),
            );
            (*buffer_ref.text).markers = marker;
        }
    }
}

/// Like set-marker, but won't let the position be outside the visible part.
#[no_mangle]
pub extern "C" fn set_marker_restricted(
    marker: Lisp_Object,
    position: Lisp_Object,
    buffer: Lisp_Object,
) -> Lisp_Object {
    unsafe { set_marker_internal(marker, position, buffer, true) }
}

/// Set the position of MARKER, specifying both the
/// character position and the corresponding byte position.
#[no_mangle]
pub extern "C" fn set_marker_both(
    marker: Lisp_Object,
    buffer: Lisp_Object,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
) -> Lisp_Object {
    let mut m = LispObject::from_raw(marker).as_marker_or_error();
    let b = live_buffer(buffer);
    if !b.is_null() {
        attach_marker(m.as_mut(), b, charpos, bytepos);
    } else {
        unsafe { unchain_marker(m.as_mut()) };
    }
    marker
}

/// Like set_marker_both, but won't let the position be outside the visible part.
#[no_mangle]
pub extern "C" fn set_marker_restricted_both(
    marker: Lisp_Object,
    buffer: Lisp_Object,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
) -> Lisp_Object {
    let mut m = LispObject::from_raw(marker).as_marker_or_error();
    let b = live_buffer(buffer);

    if !b.is_null() {
        let cur_buf = ThreadState::current_buffer();
        let clipped_charpos = clip_to_bounds(cur_buf.begv, charpos as EmacsInt, cur_buf.zv);
        let clipped_bytepos =
            clip_to_bounds(cur_buf.begv_byte, bytepos as EmacsInt, cur_buf.zv_byte);
        attach_marker(m.as_mut(), b, clipped_charpos, clipped_bytepos);
    } else {
        unsafe {
            unchain_marker(m.as_mut());
        }
    }
    marker
}

// marker_position and marker_byte_position are supposed to be used in c.
// use charpos_or_error and bytepos_or_error in rust.
/// Return the char position of marker MARKER, as a C integer.
#[no_mangle]
pub extern "C" fn marker_position(marker: Lisp_Object) -> ptrdiff_t {
    let m = LispObject::from_raw(marker).as_marker_or_error();
    m.charpos_or_error()
}

/// Return the byte position of marker MARKER, as a C integer.
#[no_mangle]
pub extern "C" fn marker_byte_position(marker: Lisp_Object) -> ptrdiff_t {
    let m = LispObject::from_raw(marker).as_marker_or_error();
    m.bytepos_or_error()
}

/// If BUFFER is nil, return current buffer pointer.  Next, check
/// whether BUFFER is a buffer object and return buffer pointer
/// corresponding to BUFFER if BUFFER is live, or NULL otherwise.
#[no_mangle]
pub extern "C" fn live_buffer(buffer: Lisp_Object) -> *mut Lisp_Buffer {
    let mut b = LispObject::from_raw(buffer).as_buffer_or_current_buffer();
    if b.is_live() {
        b.as_mut()
    } else {
        ptr::null_mut()
    }
}

include!(concat!(env!("OUT_DIR"), "/marker_exports.rs"));
