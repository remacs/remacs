//! marker support

use libc::{c_void, ptrdiff_t};
use std::mem;
use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    buffers::{current_buffer, LispBufferRef},
    lisp::{defsubr, ExternalPtr, LispMiscRef, LispObject},
    multibyte::multibyte_chars_in_text,
    remacs_sys::{allocate_misc, set_point_both, Fmake_marker},
    remacs_sys::{equal_kind, EmacsInt, Lisp_Buffer, Lisp_Marker, Lisp_Misc_Type, Lisp_Type},
    remacs_sys::{Qinteger_or_marker_p, Qmarkerp, Qnil},
    threads::ThreadState,
    util::clip_to_bounds,
};

pub type LispMarkerRef = ExternalPtr<Lisp_Marker>;

#[cfg(MARKER_DEBUG)]
const MARKER_DEBUG: bool = true;

#[cfg(not(MARKER_DEBUG))]
const MARKER_DEBUG: bool = false;

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

    pub fn equal(
        self,
        other: LispMarkerRef,
        _kind: equal_kind::Type,
        _depth: i32,
        _ht: LispObject,
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
        o.as_marker_or_error()
    }
}

impl From<LispMarkerRef> for LispObject {
    fn from(m: LispMarkerRef) -> Self {
        LispObject::tag_ptr(m, Lisp_Type::Lisp_Misc)
    }
}

impl From<LispObject> for Option<LispMarkerRef> {
    fn from(o: LispObject) -> Self {
        o.as_marker()
    }
}

impl LispObject {
    pub fn is_marker(self) -> bool {
        self.as_misc()
            .map_or(false, |m| m.get_type() == Lisp_Misc_Type::Lisp_Misc_Marker)
    }

    pub fn as_marker(self) -> Option<LispMarkerRef> {
        self.as_misc().and_then(|m| m.as_marker())
    }

    pub fn as_marker_or_error(self) -> LispMarkerRef {
        self.as_marker()
            .unwrap_or_else(|| wrong_type!(Qmarkerp, self))
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
    debug_assert!(unsafe { (*buf).name_.is_not_nil() });
    debug_assert!(charpos <= bytepos);

    let obj = unsafe { allocate_misc(Lisp_Misc_Type::Lisp_Misc_Marker) };
    let mut m = obj.as_marker_or_error();

    m.set_buffer(buf);
    m.set_charpos(charpos);
    m.set_bytepos(bytepos);
    m.set_insertion_type(false);
    m.set_need_adjustment(false);

    let mut buffer_ref = LispBufferRef::from_ptr(buf as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid buffer reference."));

    unsafe {
        m.set_next((*buffer_ref.text).markers);
        (*buffer_ref.text).markers = m.as_mut();
    }

    obj
}

/// Return value of point, as a marker object.
#[lisp_fn]
pub fn point_marker() -> LispObject {
    let mut cur_buf = ThreadState::current_buffer();
    build_marker(cur_buf.as_mut(), cur_buf.pt, cur_buf.pt_byte)
}

/// Return a marker to the minimum permissible value of point in this buffer.
/// This is the beginning, unless narrowing (a buffer restriction) is in effect.
#[lisp_fn]
pub fn point_min_marker() -> LispObject {
    let mut cur_buf = ThreadState::current_buffer();
    build_marker(cur_buf.as_mut(), cur_buf.begv, cur_buf.begv_byte)
}

/// Return a marker to the maximum permissible value of point in this buffer.
/// This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
/// is in effect, in which case it is less.
#[lisp_fn]
pub fn point_max_marker() -> LispObject {
    let mut cur_buf = ThreadState::current_buffer();
    build_marker(cur_buf.as_mut(), cur_buf.zv, cur_buf.zv_byte)
}

/// Set PT from MARKER's clipped position.
#[no_mangle]
pub extern "C" fn set_point_from_marker(marker: LispObject) {
    let marker = marker.as_marker_or_error();
    let mut cur_buf = ThreadState::current_buffer();
    let charpos = clip_to_bounds(
        cur_buf.begv,
        marker.charpos_or_error() as EmacsInt,
        cur_buf.zv,
    );
    let mut bytepos = marker.bytepos_or_error();
    // Don't trust the byte position if the marker belongs to a
    // different buffer.
    if marker.buffer().map_or(false, |b| b != cur_buf) {
        bytepos = buf_charpos_to_bytepos(cur_buf.as_mut(), charpos);
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
    let buffer_or_nil = marker
        .as_marker()
        .and_then(|m| m.buffer())
        .map_or(Qnil, LispObject::from);

    set_marker(new.into(), marker, buffer_or_nil);

    if let Some(mut m) = new.as_marker() {
        m.set_insertion_type(itype.is_not_nil())
    }

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
    let mut m = marker.as_marker_or_error();
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
    let mut m = marker.as_marker_or_error();

    if let Some(mut b) = buffer
        .as_live_buffer()
        .or_else(|| current_buffer().as_live_buffer())
    {
        let cur_buf = ThreadState::current_buffer();
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
    let m = marker.as_marker_or_error();
    m.charpos_or_error()
}

/// Return the byte position of marker MARKER, as a C integer.
#[no_mangle]
pub extern "C" fn marker_byte_position(marker: LispObject) -> ptrdiff_t {
    let m = marker.as_marker_or_error();
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
        let pos = position.as_marker_or_error();
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
        bytepos = buf_charpos_to_bytepos(buf.as_mut(), charpos);
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
    let mut buffer_ref = LispBufferRef::from_ptr(b as *mut c_void).unwrap();

    assert!(buffer_ref.beg() <= charpos && charpos <= buffer_ref.z());

    let mut best_above = buffer_ref.z();
    let mut best_above_byte = buffer_ref.z_byte();

    // If this buffer has as many characters as bytes,
    // each character must be one byte.
    // This takes care of the case where enable-multibyte-characters is nil.
    if best_above == best_above_byte {
        return charpos;
    }

    let mut best_below = buffer_ref.beg();
    let mut best_below_byte = buffer_ref.beg_byte();

    // We find in best_above and best_above_byte
    // the closest known point above CHARPOS,
    // and in best_below and best_below_byte
    // the closest known point below CHARPOS,
    //
    // If at any point we can tell that the space between those
    // two best approximations is all single-byte,
    // we interpolate the result immediately.

    macro_rules! consider_known {
        ($cpos:expr, $bpos:expr) => {
            let mut changed = false;
            if $cpos == charpos {
                if MARKER_DEBUG {
                    byte_char_debug_check(buffer_ref, charpos, $bpos);
                }
                return $bpos;
            } else if $cpos > charpos {
                if $cpos < best_above {
                    best_above = $cpos;
                    best_above_byte = $bpos;
                    changed = true;
                }
            } else if $cpos > best_below {
                best_below = $cpos;
                best_below_byte = $bpos;
                changed = true;
            }
            if changed {
                if best_above - best_below == best_above_byte - best_below_byte {
                    return best_below_byte + (charpos - best_below);
                }
            }
        };
    }

    consider_known!(buffer_ref.pt, buffer_ref.pt_byte);
    consider_known!(buffer_ref.gpt(), buffer_ref.gpt_byte());
    consider_known!(buffer_ref.begv, buffer_ref.begv_byte);
    consider_known!(buffer_ref.zv, buffer_ref.zv_byte);

    if buffer_ref.is_cached && buffer_ref.modifications() == buffer_ref.cached_modiff {
        consider_known!(buffer_ref.cached_charpos, buffer_ref.cached_bytepos);
    }

    for m in buffer_ref.markers().iter() {
        consider_known!(m.charpos_or_error(), m.bytepos_or_error());
        // If we are down to a range of 50 chars,
        // don't bother checking any other markers;
        // scan the intervening chars directly now.
        if best_above - best_below < 50 {
            break;
        }
    }

    if charpos - best_below < best_above - charpos {
        let record = charpos - best_below > 5000;

        while best_below != charpos {
            best_below += 1;
            best_below_byte = buffer_ref.inc_pos(best_below_byte);
        }
        if record {
            build_marker(b, best_below, best_below_byte);
        }
        if MARKER_DEBUG {
            byte_char_debug_check(buffer_ref, best_below, best_below_byte);
        }

        buffer_ref.is_cached = true;
        buffer_ref.cached_modiff = buffer_ref.modifications();

        buffer_ref.cached_charpos = best_below;
        buffer_ref.cached_bytepos = best_below_byte;

        best_below_byte
    } else {
        let record = best_above - charpos > 5000;

        while best_above != charpos {
            best_above -= 1;
            best_above_byte = buffer_ref.dec_pos(best_above_byte);
        }

        if record {
            build_marker(b, best_above, best_above_byte);
        }
        if MARKER_DEBUG {
            byte_char_debug_check(buffer_ref, best_below, best_below_byte);
        }

        buffer_ref.is_cached = true;
        buffer_ref.cached_modiff = buffer_ref.modifications();

        buffer_ref.cached_charpos = best_above;
        buffer_ref.cached_bytepos = best_above_byte;

        best_above_byte
    }
}

#[no_mangle]
pub unsafe extern "C" fn buf_bytepos_to_charpos(b: *mut Lisp_Buffer, bytepos: isize) -> isize {
    let mut buffer_ref = LispBufferRef::from_ptr(b as *mut c_void).unwrap();

    assert!(buffer_ref.beg_byte() <= bytepos && bytepos <= buffer_ref.z_byte());

    let mut best_above = buffer_ref.z();
    let mut best_above_byte = buffer_ref.z_byte();

    // If this buffer has as many characters as bytes,
    // each character must be one byte.
    // This takes care of the case where enable-multibyte-characters is nil.
    if best_above == best_above_byte {
        return bytepos;
    }

    let mut best_below = buffer_ref.beg();
    let mut best_below_byte = buffer_ref.beg_byte();

    macro_rules! consider_known {
        ($bpos:expr, $cpos:expr) => {
            let mut changed = false;
            if $bpos == bytepos {
                if MARKER_DEBUG {
                    byte_char_debug_check(buffer_ref, $cpos, bytepos);
                }
                return $cpos;
            } else if $bpos > bytepos {
                if $bpos < best_above_byte {
                    best_above = $cpos;
                    best_above_byte = $bpos;
                    changed = true;
                }
            } else if $bpos > best_below_byte {
                best_below = $cpos;
                best_below_byte = $bpos;
                changed = true;
            }
            if changed {
                if best_above - best_below == best_above_byte - best_below_byte {
                    return best_below + (bytepos - best_below_byte);
                }
            }
        };
    }

    consider_known!(buffer_ref.pt_byte, buffer_ref.pt);
    consider_known!(buffer_ref.gpt_byte(), buffer_ref.gpt());
    consider_known!(buffer_ref.begv_byte, buffer_ref.begv);
    consider_known!(buffer_ref.zv_byte, buffer_ref.zv);

    if buffer_ref.is_cached && buffer_ref.modifications() == buffer_ref.cached_modiff {
        consider_known!(buffer_ref.cached_bytepos, buffer_ref.cached_charpos);
    }

    for m in buffer_ref.markers().iter() {
        consider_known!(m.bytepos_or_error(), m.charpos_or_error());
        // If we are down to a range of 50 chars,
        // don't bother checking any other markers;
        // scan the intervening chars directly now.
        if best_above - best_below < 50 {
            break;
        }
    }

    // We get here if we did not exactly hit one of the known places.
    // We have one known above and one known below.
    // Scan, counting characters, from whichever one is closer.

    if bytepos - best_below_byte < best_above_byte - bytepos {
        let record = bytepos - best_below_byte > 5000;

        while best_below_byte < bytepos {
            best_below += 1;
            best_below_byte = buffer_ref.inc_pos(best_below_byte);
        }

        // If this position is quite far from the nearest known position,
        // cache the correspondence by creating a marker here.
        // It will last until the next GC.
        // But don't do it if BUF_MARKERS is nil;
        // that is a signal from Fset_buffer_multibyte.
        if record && buffer_ref.markers().is_some() {
            build_marker(b, best_below, best_below_byte);
        }
        if MARKER_DEBUG {
            byte_char_debug_check(buffer_ref, best_below, best_below_byte);
        }

        buffer_ref.is_cached = true;
        buffer_ref.cached_modiff = buffer_ref.modifications();

        buffer_ref.cached_charpos = best_below;
        buffer_ref.cached_bytepos = best_below_byte;

        best_below
    } else {
        let record = best_above_byte - bytepos > 5000;

        while best_above_byte > bytepos {
            best_above -= 1;
            best_above_byte = buffer_ref.dec_pos(best_above_byte);
        }

        // If this position is quite far from the nearest known position,
        // cache the correspondence by creating a marker here.
        // It will last until the next GC.
        // But don't do it if BUF_MARKERS is nil;
        // that is a signal from Fset_buffer_multibyte.
        if record && buffer_ref.markers().is_some() {
            build_marker(b, best_below, best_below_byte);
        }
        if MARKER_DEBUG {
            byte_char_debug_check(buffer_ref, best_below, best_below_byte);
        }

        buffer_ref.is_cached = true;
        buffer_ref.cached_modiff = buffer_ref.modifications();

        buffer_ref.cached_charpos = best_above;
        buffer_ref.cached_bytepos = best_above_byte;

        best_above
    }
}

#[no_mangle]
pub extern "C" fn clear_charpos_cache(b: *mut Lisp_Buffer) {
    let mut buf_ref = LispBufferRef::from_ptr(b as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid buffer reference."));
    buf_ref.is_cached = false;
}

// Debugging

fn byte_char_debug_check(b: LispBufferRef, charpos: isize, bytepos: isize) {
    if !b.multibyte_characters_enabled() {
        return;
    }

    let nchars = unsafe {
        if bytepos > b.gpt_byte() {
            multibyte_chars_in_text(b.beg_addr(), b.gpt_byte() - b.beg_byte())
                + multibyte_chars_in_text(b.gap_end_addr(), bytepos - b.gpt_byte())
        } else {
            multibyte_chars_in_text(b.beg_addr(), bytepos - b.beg_byte())
        }
    };

    if charpos - 1 != nchars {
        panic!("byte_char_debug_check failed.")
    }
}

/// Count the markers in buffer BUF.
#[cfg(MARKER_DEBUG)]
fn count_markers(buf: LispBufferRef) -> u8 {
    let mut total = 0;
    for _m in buf.markers().iter() {
        total += 1;
    }
    total
}

/// Recompute the bytepos corresponding to CHARPOS in the simplest, most reliable way.
#[cfg(MARKER_DEBUG)]
fn verify_bytepos(charpos: isize) -> isize {
    let mut below: isize = 1;
    let mut below_byte: isize = 1;
    let cur_buf = ThreadState::current_buffer();

    while below != charpos {
        below += 1;
        below_byte = cur_buf.inc_pos(below_byte);
    }
    below_byte
}

include!(concat!(env!("OUT_DIR"), "/marker_exports.rs"));
