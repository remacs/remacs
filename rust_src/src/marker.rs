//! marker support

use libc::{c_void, ptrdiff_t};
use std::mem;
use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    buffers::{current_buffer, LispBufferRef},
    hashtable::LispHashTableRef,
    lisp::{ExternalPtr, LispMiscRef, LispObject, LispStructuralEqual},
    remacs_sys::{allocate_misc, set_point_both, Fmake_marker},
    remacs_sys::{equal_kind, EmacsInt, Lisp_Buffer, Lisp_Marker, Lisp_Misc_Type, Lisp_Type},
    remacs_sys::{Qinteger_or_marker_p, Qmarkerp},
    threads::ThreadState,
    util::clip_to_bounds,
};

pub type LispMarkerRef = ExternalPtr<Lisp_Marker>;

pub const MARKER_DEBUG: bool = cfg!(MARKER_DEBUG);

impl LispMarkerRef {
    pub fn charpos(self) -> Option<isize> {
        match self.buffer() {
            None => None,
            Some(_) => Some(self.charpos),
        }
    }

    pub fn charpos_or_error(self) -> isize {
        match self.buffer() {
            None => error!("Marker does not point anywhere"),
            Some(_) => self.charpos,
        }
    }

    pub fn set_charpos(&mut self, charpos: isize) {
        self.charpos = charpos;
    }

    pub fn bytepos(self) -> Option<isize> {
        match self.buffer() {
            None => None,
            Some(_) => Some(self.bytepos),
        }
    }

    pub fn bytepos_or_error(self) -> isize {
        match self.buffer() {
            None => error!("Marker does not point anywhere"),
            Some(_) => self.bytepos,
        }
    }

    pub fn set_bytepos(&mut self, bytepos: isize) {
        self.bytepos = bytepos;
    }

    pub fn buffer(self) -> Option<LispBufferRef> {
        unsafe { self.buffer.as_ref().map(|b| mem::transmute(b)) }
    }

    pub fn set_buffer(mut self, b: *mut Lisp_Buffer) {
        self.buffer = b;
    }

    pub fn iter(self) -> LispMarkerIter {
        LispMarkerIter {
            current: Some(self),
        }
    }

    pub fn next(self) -> Option<LispMarkerRef> {
        unsafe { self.next.as_ref().map(|n| mem::transmute(n)) }
    }

    pub fn set_next(mut self, m: *mut Lisp_Marker) {
        self.next = m;
    }
}

impl LispStructuralEqual for LispMarkerRef {
    fn equal(
        &self,
        other: Self,
        _kind: equal_kind::Type,
        _depth: i32,
        _ht: &mut LispHashTableRef,
    ) -> bool {
        if self.buffer != other.buffer {
            false
        } else if self.buffer.is_null() {
            true
        } else {
            self.bytepos == other.bytepos
        }
    }
}

impl From<LispObject> for LispMarkerRef {
    fn from(o: LispObject) -> Self {
        o.as_marker().unwrap_or_else(|| wrong_type!(Qmarkerp, o))
    }
}

impl From<LispMarkerRef> for LispObject {
    fn from(m: LispMarkerRef) -> Self {
        Self::tag_ptr(m, Lisp_Type::Lisp_Misc)
    }
}

impl From<LispObject> for Option<LispMarkerRef> {
    fn from(o: LispObject) -> Self {
        o.as_misc().and_then(LispMiscRef::as_marker)
    }
}

impl LispObject {
    pub fn is_marker(self) -> bool {
        self.as_misc()
            .map_or(false, |m| m.get_type() == Lisp_Misc_Type::Lisp_Misc_Marker)
    }

    pub fn as_marker(self) -> Option<LispMarkerRef> {
        self.into()
    }
}

impl LispMiscRef {
    pub fn as_marker(self) -> Option<LispMarkerRef> {
        if self.get_type() == Lisp_Misc_Type::Lisp_Misc_Marker {
            unsafe { Some(mem::transmute(self)) }
        } else {
            None
        }
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

/// Return a newly allocated marker which points into BUF
/// at character position CHARPOS and byte position BYTEPOS.
#[no_mangle]
pub extern "C" fn build_marker(
    buf: *mut Lisp_Buffer,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
) -> LispObject {
    let buffer = LispBufferRef::from_ptr(buf as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid buffer reference."));
    build_marker_rust(buffer, charpos as isize, bytepos as isize).into()
}

pub fn build_marker_rust(
    mut buffer: LispBufferRef,
    charpos: isize,
    bytepos: isize,
) -> LispMarkerRef {
    debug_assert!(buffer.name_.is_not_nil());
    debug_assert!(charpos <= bytepos);

    let mut marker: LispMarkerRef =
        unsafe { allocate_misc(Lisp_Misc_Type::Lisp_Misc_Marker) }.into();

    marker.set_buffer(buffer.as_mut());
    marker.set_charpos(charpos);
    marker.set_bytepos(bytepos);
    marker.set_insertion_type(false);
    marker.set_need_adjustment(false);

    unsafe {
        marker.set_next((*buffer.text).markers);
        (*buffer.text).markers = marker.as_mut();
    }

    marker
}

/// Return value of point, as a marker object.
#[lisp_fn]
pub fn point_marker() -> LispMarkerRef {
    let cur_buf = ThreadState::current_buffer_unchecked();
    build_marker_rust(cur_buf, cur_buf.pt, cur_buf.pt_byte)
}

/// Return a marker to the minimum permissible value of point in this buffer.
/// This is the beginning, unless narrowing (a buffer restriction) is in effect.
#[lisp_fn]
pub fn point_min_marker() -> LispMarkerRef {
    let cur_buf = ThreadState::current_buffer_unchecked();
    build_marker_rust(cur_buf, cur_buf.begv, cur_buf.begv_byte)
}

/// Return a marker to the maximum permissible value of point in this buffer.
/// This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
/// is in effect, in which case it is less.
#[lisp_fn]
pub fn point_max_marker() -> LispMarkerRef {
    let cur_buf = ThreadState::current_buffer_unchecked();
    build_marker_rust(cur_buf, cur_buf.zv, cur_buf.zv_byte)
}

/// Set PT from MARKER's clipped position.
#[no_mangle]
pub extern "C" fn set_point_from_marker(marker: LispObject) {
    let marker: LispMarkerRef = marker.into();
    let cur_buf = ThreadState::current_buffer_unchecked();
    let charpos = clip_to_bounds(
        cur_buf.begv,
        marker.charpos_or_error() as EmacsInt,
        cur_buf.zv,
    );

    // Don't trust the byte position if the marker belongs to a
    // different buffer.
    let bytepos = if marker.buffer().map_or(false, |b| b != cur_buf) {
        cur_buf.charpos_to_bytepos(charpos)
    } else {
        clip_to_bounds(
            cur_buf.begv_byte,
            marker.bytepos_or_error() as EmacsInt,
            cur_buf.zv_byte,
        )
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
    marker.set_insertion_type(itype.is_not_nil());
    itype
}

/// Position MARKER before character number POSITION in BUFFER.
/// If BUFFER is omitted or nil, it defaults to the current buffer.  If
/// POSITION is nil, makes marker point nowhere so it no longer slows down
/// editing in any buffer.  Returns MARKER.
#[lisp_fn(min = "2")]
pub fn set_marker(
    marker: LispMarkerRef,
    position: LispObject,
    buffer: LispObject,
) -> LispMarkerRef {
    set_marker_internal(marker, position, buffer, false)
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
    let new = unsafe { Fmake_marker() };
    let buffer_or_nil = marker.as_marker().and_then(LispMarkerRef::buffer);

    set_marker(new.into(), marker, buffer_or_nil.into());

    if let Some(mut m) = new.as_marker() {
        m.set_insertion_type(itype.is_not_nil())
    }

    new
}

/// Return t if there are markers pointing at POSITION in the current buffer.
#[lisp_fn]
pub fn buffer_has_markers_at(position: EmacsInt) -> bool {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let position = clip_to_bounds(cur_buf.begv, position, cur_buf.zv);

    cur_buf.markers().map_or(false, |marker| {
        marker
            .iter()
            .any(|m| m.charpos().map_or(false, |p| p == position))
    })
}

/// Change M so it points to B at CHARPOS and BYTEPOS.
pub fn attach_marker(
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

/// Remove MARKER from the chain of whatever buffer it is in,
/// leaving it points to nowhere.  This is called during garbage
/// collection, so we must be careful to ignore and preserve
/// mark bits, including those in chain fields of markers.
#[no_mangle]
pub extern "C" fn unchain_marker(marker: *mut Lisp_Marker) {
    unsafe {
        let marker_ref = LispMarkerRef::from_ptr(marker as *mut c_void)
            .unwrap_or_else(|| panic!("Invalid marker reference."));

        if let Some(mut buf) = marker_ref.buffer() {
            marker_ref.set_buffer(ptr::null_mut());
            if let Some(last) = buf.markers() {
                let mut tail: LispMarkerRef = last;

                for cur in last.iter() {
                    if cur == marker_ref {
                        if cur == last {
                            // Deleting first marker from the buffer's chain.  Crash
                            // if new first marker in chain does not say it belongs
                            // to the same buffer, or at least that they have the same
                            // base buffer.
                            if let Some(new) = tail.next() {
                                if new.buffer().map_or(true, |n| n.text != buf.text) {
                                    panic!("New first marker in chain does not belong to buffer!");
                                }
                            }
                            match cur.next() {
                                None => (*buf.text).markers = ptr::null_mut(),
                                Some(mut c) => (*buf.text).markers = c.as_mut(),
                            }
                        } else {
                            tail.set_next(cur.next().map_or(ptr::null_mut(), |mut m| m.as_mut()));
                        }
                        // We have removed the marker from the chain;
                        // no need to scan the rest of the chain.
                        return;
                    }
                    tail = cur;
                }
                panic!("Marker was not found in its chain.");
            } else {
                panic!("No markers were found in buffer.");
            }
        }
    }
}

/// Like set-marker, but won't let the position be outside the visible part.
#[no_mangle]
pub extern "C" fn set_marker_restricted(
    marker: LispObject,
    position: LispObject,
    buffer: LispObject,
) -> LispObject {
    set_marker_internal(marker.into(), position, buffer, true).into()
}

/// Set the position of MARKER, specifying both the
/// character position and the corresponding byte position.
#[no_mangle]
pub extern "C" fn set_marker_both(
    marker: LispObject,
    buffer: LispObject,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
) -> LispObject {
    let mut m: LispMarkerRef = marker.into();
    if let Some(mut b) = buffer
        .as_live_buffer()
        .or_else(|| current_buffer().as_live_buffer())
    {
        attach_marker(m.as_mut(), b.as_mut(), charpos, bytepos);
    } else {
        unchain_marker(m.as_mut());
    }
    marker
}

/// Like set_marker_both, but won't let the position be outside the visible part.
#[no_mangle]
pub extern "C" fn set_marker_restricted_both(
    marker: LispObject,
    buffer: LispObject,
    charpos: ptrdiff_t,
    bytepos: ptrdiff_t,
) -> LispObject {
    let mut m: LispMarkerRef = marker.into();

    if let Some(mut b) = buffer
        .as_live_buffer()
        .or_else(|| current_buffer().as_live_buffer())
    {
        let cur_buf = ThreadState::current_buffer_unchecked();
        let clipped_charpos = clip_to_bounds(cur_buf.begv, charpos as EmacsInt, cur_buf.zv);
        let clipped_bytepos =
            clip_to_bounds(cur_buf.begv_byte, bytepos as EmacsInt, cur_buf.zv_byte);
        attach_marker(m.as_mut(), b.as_mut(), clipped_charpos, clipped_bytepos);
    } else {
        unchain_marker(m.as_mut());
    }
    marker
}

// marker_position and marker_byte_position are supposed to be used in c.
// use charpos_or_error and bytepos_or_error in rust.
/// Return the char position of marker MARKER, as a C integer.
#[no_mangle]
pub extern "C" fn marker_position(marker: LispObject) -> ptrdiff_t {
    let m: LispMarkerRef = marker.into();
    m.charpos_or_error()
}

/// Return the byte position of marker MARKER, as a C integer.
#[no_mangle]
pub extern "C" fn marker_byte_position(marker: LispObject) -> ptrdiff_t {
    let m: LispMarkerRef = marker.into();
    m.bytepos_or_error()
}

impl LispObject {
    pub fn has_buffer(self) -> bool {
        self.as_marker().map_or(false, |m| m.buffer().is_some())
    }
}

/// Internal function to set MARKER in BUFFER at POSITION.  Non-zero
/// RESTRICTED means limit the POSITION by the visible part of BUFFER.
fn set_marker_internal(
    mut marker: LispMarkerRef,
    position: LispObject,
    buffer: LispObject,
    restricted: bool,
) -> LispMarkerRef {
    let buf = buffer
        .as_live_buffer()
        .or_else(|| current_buffer().as_live_buffer());
    // Set MARKER to point nowhere if BUFFER is dead, or
    // POSITION is nil or a marker points to nowhere.
    if position.is_nil() || (position.is_marker() && !position.has_buffer()) || buf.is_none() {
        unchain_marker(marker.as_mut());

    // Optimize the special case where we are copying the position of
    // an existing marker, and MARKER is already in the same buffer.
    } else if position.as_marker().map_or(false, |p| p.buffer() == buf) && marker.buffer() == buf {
        let pos: LispMarkerRef = position.into();
        marker.charpos = pos.charpos_or_error();
        marker.bytepos = pos.bytepos_or_error();
    } else {
        let b = buf.unwrap_or_else(|| panic!("Invalid buffer reference."));
        set_marker_internal_else(marker, position, restricted, b);
    }
    marker
}

fn set_marker_internal_else(
    mut marker: LispMarkerRef,
    position: LispObject,
    restricted: bool,
    mut buf: LispBufferRef,
) {
    let mut charpos: ptrdiff_t;
    let mut bytepos: ptrdiff_t;

    // Do not use CHECK_NUMBER_COERCE_MARKER because we
    // don't want to call buf_charpos_to_bytepos if POSITION
    // is a marker and so we know the bytepos already.
    if let Some(num) = position.as_fixnum() {
        charpos = num as ptrdiff_t;
        bytepos = -1;
    } else if let Some(m) = position.as_marker() {
        charpos = m.charpos_or_error();
        bytepos = m.bytepos_or_error();
    } else {
        wrong_type!(Qinteger_or_marker_p, position)
    }
    let beg = buf.buffer_beg(restricted);
    let end = buf.buffer_end(restricted);
    charpos = clip_to_bounds(beg, charpos as EmacsInt, end);

    // Don't believe BYTEPOS if it comes from a different buffer,
    // since that buffer might have a very different correspondence
    // between character and byte positions.
    if bytepos == -1
        || !position
            .as_marker()
            .map_or(false, |m| m.buffer() == Some(buf))
    {
        bytepos = buf.charpos_to_bytepos(charpos);
    } else {
        let beg = buf.buffer_beg_byte(restricted);
        let end = buf.buffer_end_byte(restricted);
        bytepos = clip_to_bounds(beg, bytepos as EmacsInt, end);
    }
    attach_marker(marker.as_mut(), buf.as_mut(), charpos, bytepos);
}

impl LispBufferRef {
    pub fn buffer_beg(self, visible: bool) -> ptrdiff_t {
        if visible {
            self.begv
        } else {
            self.beg()
        }
    }

    pub fn buffer_end(self, visible: bool) -> ptrdiff_t {
        if visible {
            self.zv
        } else {
            self.z()
        }
    }

    pub fn buffer_beg_byte(self, visible: bool) -> ptrdiff_t {
        if visible {
            self.begv_byte
        } else {
            self.beg_byte()
        }
    }

    pub fn buffer_end_byte(self, visible: bool) -> ptrdiff_t {
        if visible {
            self.zv_byte
        } else {
            self.z_byte()
        }
    }
}

// Converting between character positions and byte positions.

// There are several places in the buffer where we know
// the correspondence: BEG, BEGV, PT, GPT, ZV and Z,
// and everywhere there is a marker.  So we find the one of these places
// that is closest to the specified position, and scan from there.

/// Return the byte position corresponding to CHARPOS in B.
#[no_mangle]
pub extern "C" fn buf_charpos_to_bytepos(b: *mut Lisp_Buffer, charpos: isize) -> isize {
    let buffer = LispBufferRef::from_ptr(b as *mut c_void).unwrap();
    buffer.charpos_to_bytepos(charpos)
}

#[no_mangle]
pub unsafe extern "C" fn buf_bytepos_to_charpos(b: *mut Lisp_Buffer, bytepos: isize) -> isize {
    let buffer = LispBufferRef::from_ptr(b as *mut c_void).unwrap();
    buffer.bytepos_to_charpos(bytepos)
}

#[no_mangle]
pub extern "C" fn clear_charpos_cache(b: *mut Lisp_Buffer) {
    let mut buf_ref = LispBufferRef::from_ptr(b as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid buffer reference."));
    buf_ref.is_cached = false;
}

include!(concat!(env!("OUT_DIR"), "/marker_exports.rs"));
