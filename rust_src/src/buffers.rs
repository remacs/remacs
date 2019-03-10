//! Functions operating on buffers.

use std::{self, mem, ptr};

use field_offset::FieldOffset;
use libc::{self, c_char, c_int, c_uchar, c_void, ptrdiff_t};

use rand::{thread_rng, Rng};

use remacs_macros::lisp_fn;

use crate::{
    character::char_head_p,
    chartable::LispCharTableRef,
    data::Lisp_Fwd,
    editfns::{point, widen},
    eval::unbind_to,
    fileio::{expand_file_name, find_file_name_handler},
    frames::LispFrameRef,
    hashtable::LispHashTableRef,
    lisp::{ExternalPtr, LispMiscRef, LispObject, LispStructuralEqual, LiveBufferIter},
    lists::{car, cdr, list, member, rassq, setcar},
    lists::{CarIter, LispConsCircularChecks, LispConsEndChecks},
    marker::{
        build_marker, build_marker_rust, marker_buffer, marker_position_lisp, set_marker_both,
        LispMarkerRef, MARKER_DEBUG,
    },
    multibyte::{multibyte_chars_in_text, multibyte_length_by_head, string_char},
    multibyte::{LispStringRef, LispSymbolOrString},
    numbers::MOST_POSITIVE_FIXNUM,
    obarray::intern,
    remacs_sys::{
        allocate_misc, bset_update_mode_line, buffer_local_flags, buffer_local_value,
        buffer_window_count, concat2, del_range, delete_all_overlays, globals, last_per_buffer_idx,
        lookup_char_property, make_timespec, marker_position, modify_overlay,
        set_buffer_internal_1, specbind, unchain_both, unchain_marker, update_mode_lines,
        windows_or_buffers_changed,
    },
    remacs_sys::{
        buffer_defaults, equal_kind, pvec_type, EmacsInt, Lisp_Buffer, Lisp_Buffer_Local_Value,
        Lisp_Misc_Type, Lisp_Overlay, Lisp_Type, Vbuffer_alist, Vrun_hooks,
    },
    remacs_sys::{Fcopy_sequence, Fget_text_property, Fnconc, Fnreverse},
    remacs_sys::{
        Qafter_string, Qbefore_string, Qbuffer_list_update_hook, Qbuffer_read_only, Qbufferp,
        Qget_file_buffer, Qinhibit_quit, Qinhibit_read_only, Qnil, Qoverlayp, Qt, Qunbound,
        UNKNOWN_MODTIME_NSECS,
    },
    strings::string_equal,
    threads::{c_specpdl_index, ThreadState},
    vectors::LispVectorlikeRef,
};

pub const BEG: ptrdiff_t = 1;
pub const BEG_BYTE: ptrdiff_t = 1;

/// Return value of point, in bytes, as an integer.
/// Beginning of buffer is position (point-min).
pub fn point_byte() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.pt_byte as EmacsInt
}

/// Return the minimum permissible byte_position in the current
/// buffer.  This is 1, unless narrowing (a buffer restriction) is in
/// effect.
pub fn point_min_byte() -> EmacsInt {
    ThreadState::current_buffer_unchecked().begv_byte as EmacsInt
}

/// Maximum number of bytes in a buffer.
/// A buffer cannot contain more bytes than a 1-origin fixnum can
/// represent, nor can it be so large that C pointer arithmetic stops
/// working. The `ptrdiff_t` cast ensures that this is signed, not unsigned.
//const fn buf_bytes_max() -> ptrdiff_t {
//    const mpf: ptrdiff_t = (MOST_POSITIVE_FIXNUM - 1) as ptrdiff_t;
//    const eimv: ptrdiff_t = EmacsInt::max_value() as ptrdiff_t;
//    const pdmv: ptrdiff_t = libc::ptrdiff_t::max_value();
//    const arith_max: ptrdiff_t = if eimv <= pdmv {
//        eimv
//    } else {
//        pdmv
//    };
//    if mpf as ptrdiff_t <= arith_max {
//        mpf
//    } else {
//        arith_max
//    }
//}
// TODO(db48x): use the nicer implementation above once const functions can have conditionals in them
// https://github.com/rust-lang/rust/issues/24111
const fn buf_bytes_max() -> ptrdiff_t {
    const p: [ptrdiff_t; 2] = [
        EmacsInt::max_value() as ptrdiff_t,
        libc::ptrdiff_t::max_value(),
    ];
    const arith_max: ptrdiff_t =
        p[((p[1] - p[0]) >> ((8 * std::mem::size_of::<ptrdiff_t>()) - 1)) as usize];
    const q: [ptrdiff_t; 2] = [(MOST_POSITIVE_FIXNUM - 1) as ptrdiff_t, arith_max];
    q[((q[1] - q[0]) >> ((8 * std::mem::size_of::<ptrdiff_t>()) - 1)) as usize]
}
pub const BUF_BYTES_MAX: ptrdiff_t = buf_bytes_max();

pub type LispBufferRef = ExternalPtr<Lisp_Buffer>;
pub type LispOverlayRef = ExternalPtr<Lisp_Overlay>;

impl LispBufferRef {
    pub fn is_read_only(self) -> bool {
        self.read_only_.into()
    }

    pub fn beg(self) -> ptrdiff_t {
        BEG
    }

    pub fn beg_byte(self) -> ptrdiff_t {
        BEG_BYTE
    }

    pub fn gap_start_addr(self) -> *mut c_uchar {
        unsafe { (*self.text).beg.offset((*self.text).gpt_byte - BEG_BYTE) }
    }

    pub fn gap_end_addr(self) -> *mut c_uchar {
        unsafe {
            (*self.text)
                .beg
                .offset((*self.text).gpt_byte + (*self.text).gap_size - BEG_BYTE)
        }
    }

    pub fn z_addr(self) -> *mut c_uchar {
        unsafe {
            (*self.text)
                .beg
                .offset((*self.text).gap_size + (*self.text).z_byte - BEG_BYTE)
        }
    }

    pub fn markers(self) -> Option<LispMarkerRef> {
        unsafe { (*self.text).markers.as_ref().map(|m| mem::transmute(m)) }
    }

    pub fn mark_active(self) -> LispObject {
        self.mark_active_
    }

    pub fn pt_marker(self) -> LispObject {
        self.pt_marker_
    }

    pub fn begv_marker(self) -> LispObject {
        self.begv_marker_
    }

    pub fn zv_marker(self) -> LispObject {
        self.zv_marker_
    }

    pub fn mark(self) -> LispObject {
        self.mark_
    }

    #[allow(dead_code)]
    pub fn name(self) -> LispObject {
        self.name_
    }

    pub fn filename(self) -> LispObject {
        self.filename_
    }

    pub fn base_buffer(self) -> Option<LispBufferRef> {
        Self::from_ptr(self.base_buffer as *mut c_void)
    }

    pub fn truename(self) -> LispObject {
        self.file_truename_
    }

    pub fn case_fold_search(self) -> LispObject {
        self.case_fold_search_
    }

    // Check if buffer is live
    pub fn is_live(self) -> bool {
        self.name_.is_not_nil()
    }

    pub fn set_pt_both(&mut self, charpos: ptrdiff_t, byte: ptrdiff_t) {
        self.pt = charpos;
        self.pt_byte = byte;
    }

    pub fn set_begv_both(&mut self, charpos: ptrdiff_t, byte: ptrdiff_t) {
        self.begv = charpos;
        self.begv_byte = byte;
    }

    pub fn set_zv_both(&mut self, charpos: ptrdiff_t, byte: ptrdiff_t) {
        self.zv = charpos;
        self.zv_byte = byte;
    }

    pub fn set_syntax_table(&mut self, table: LispCharTableRef) {
        self.syntax_table_ = table.into();
    }

    pub fn value_p(self, idx: isize) -> bool {
        if idx < 0 || idx >= (unsafe { last_per_buffer_idx } as isize) {
            panic!("buffer value_p called with an invalid index!");
        }
        self.local_flags[idx as usize] != 0
    }

    // Similar to SET_PER_BUFFER_VALUE_P macro in C
    /// Set whether per-buffer variable with index IDX has a buffer-local
    /// value in buffer.  VAL zero means it does't.
    pub fn set_per_buffer_value_p(&mut self, idx: usize, val: libc::c_char) {
        unsafe {
            if idx >= last_per_buffer_idx as usize {
                panic!(
                    "set_per_buffer_value_p called with index greater than {}",
                    last_per_buffer_idx
                );
            }
        }
        self.local_flags[idx] = val;
    }

    // Characters, positions and byte positions.

    /// Return the address of byte position N in current buffer.
    pub fn byte_pos_addr(self, n: ptrdiff_t) -> *mut c_uchar {
        let offset = if n >= self.gpt_byte() {
            self.gap_size()
        } else {
            0
        };

        unsafe { self.beg_addr().offset(offset + n - self.beg_byte()) }
    }

    /// Return the address of character at byte position BYTE_POS.
    pub fn buf_byte_address(self, byte_pos: isize) -> c_uchar {
        let gap = self.pos_within_range(byte_pos);
        unsafe { *(self.beg_addr().offset(byte_pos - BEG_BYTE + gap)) as c_uchar }
    }

    /// Return the byte at byte position N.
    pub fn fetch_byte(self, n: ptrdiff_t) -> u8 {
        unsafe { *self.byte_pos_addr(n) }
    }

    /// Return character at byte position POS.  See the caveat WARNING for
    /// FETCH_MULTIBYTE_CHAR below.
    pub fn fetch_char(self, n: ptrdiff_t) -> c_int {
        if self.multibyte_characters_enabled() {
            self.fetch_multibyte_char(n)
        } else {
            c_int::from(self.fetch_byte(n))
        }
    }

    /// Return character code of multi-byte form at byte position POS.  If POS
    /// doesn't point the head of valid multi-byte form, only the byte at
    /// POS is returned.  No range checking.
    pub fn fetch_multibyte_char(self, n: ptrdiff_t) -> c_int {
        let offset = if n >= self.gpt_byte() && n >= 0 {
            self.gap_size()
        } else {
            0
        };

        unsafe {
            string_char(
                self.beg_addr().offset(offset + n - self.beg_byte()),
                ptr::null_mut(),
                ptr::null_mut(),
            )
        }
    }

    pub fn multibyte_characters_enabled(self) -> bool {
        self.enable_multibyte_characters_.is_not_nil()
    }

    pub fn pos_within_range(self, pos: isize) -> isize {
        if pos >= self.gpt_byte() {
            self.gap_size()
        } else {
            0
        }
    }

    // Same as the BUF_INC_POS c macro
    /// Increment the buffer byte position POS_BYTE of the the buffer to
    /// the next character boundary.  This macro relies on the fact that
    /// *GPT_ADDR and *Z_ADDR are always accessible and the values are
    /// '\0'.  No range checking of POS_BYTE.
    pub fn inc_pos(self, pos_byte: isize) -> isize {
        let chp = self.buf_byte_address(pos_byte);
        pos_byte + multibyte_length_by_head(chp) as isize
    }

    // Same as the BUF_DEC_POS c macro
    /// Decrement the buffer byte position POS_BYTE of the buffer to
    /// the previous character boundary.  No range checking of POS_BYTE.
    pub fn dec_pos(self, pos_byte: isize) -> isize {
        let mut new_pos = pos_byte - 1;
        let mut offset = new_pos - self.beg_byte();
        offset += self.pos_within_range(new_pos);
        unsafe {
            let mut chp = self.beg_addr().offset(offset);

            while !char_head_p(*chp) {
                chp = chp.offset(-1);
                new_pos -= 1;
            }
        }
        new_pos
    }

    // Methods for accessing struct buffer_text fields

    pub fn beg_addr(self) -> *mut c_uchar {
        unsafe { (*self.text).beg }
    }

    pub fn gpt(self) -> ptrdiff_t {
        unsafe { (*self.text).gpt }
    }

    pub fn gpt_byte(self) -> ptrdiff_t {
        unsafe { (*self.text).gpt_byte }
    }

    pub fn gap_size(self) -> ptrdiff_t {
        unsafe { (*self.text).gap_size }
    }

    pub fn gap_position(self) -> ptrdiff_t {
        unsafe { (*self.text).gpt }
    }

    /// Number of modifications made to the buffer.
    pub fn modifications(self) -> EmacsInt {
        unsafe { (*self.text).modiff }
    }

    /// Value of `modiff` last time the buffer was saved.
    pub fn modifications_since_save(self) -> EmacsInt {
        unsafe { (*self.text).save_modiff }
    }

    /// Number of modifications to the buffer's characters.
    pub fn char_modifications(self) -> EmacsInt {
        unsafe { (*self.text).chars_modiff }
    }

    pub fn overlay_modifications(self) -> EmacsInt {
        unsafe { (*self.text).overlay_modiff }
    }

    pub fn z_byte(self) -> ptrdiff_t {
        unsafe { (*self.text).z_byte }
    }

    pub fn z(self) -> ptrdiff_t {
        unsafe { (*self.text).z }
    }

    pub fn bytepos_to_charpos(mut self, bytepos: isize) -> isize {
        assert!(self.beg_byte() <= bytepos && bytepos <= self.z_byte());

        let mut best_above = self.z();
        let mut best_above_byte = self.z_byte();

        // If this buffer has as many characters as bytes,
        // each character must be one byte.
        // This takes care of the case where enable-multibyte-characters is nil.
        if best_above == best_above_byte {
            return bytepos;
        }

        let mut best_below = self.beg();
        let mut best_below_byte = self.beg_byte();

        macro_rules! consider_known {
            ($bpos:expr, $cpos:expr) => {
                let mut changed = false;
                if $bpos == bytepos {
                    if MARKER_DEBUG {
                        byte_char_debug_check(self, $cpos, bytepos);
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

        consider_known!(self.pt_byte, self.pt);
        consider_known!(self.gpt_byte(), self.gpt());
        consider_known!(self.begv_byte, self.begv);
        consider_known!(self.zv_byte, self.zv);

        if self.is_cached && self.modifications() == self.cached_modiff {
            consider_known!(self.cached_bytepos, self.cached_charpos);
        }

        for m in self.markers().iter() {
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
                best_below_byte = self.inc_pos(best_below_byte);
            }

            // If this position is quite far from the nearest known position,
            // cache the correspondence by creating a marker here.
            // It will last until the next GC.
            // But don't do it if BUF_MARKERS is nil;
            // that is a signal from Fset_buffer_multibyte.
            if record && self.markers().is_some() {
                build_marker_rust(self, best_below, best_below_byte);
            }
            if MARKER_DEBUG {
                byte_char_debug_check(self, best_below, best_below_byte);
            }

            self.is_cached = true;
            self.cached_modiff = self.modifications();

            self.cached_charpos = best_below;
            self.cached_bytepos = best_below_byte;

            best_below
        } else {
            let record = best_above_byte - bytepos > 5000;

            while best_above_byte > bytepos {
                best_above -= 1;
                best_above_byte = self.dec_pos(best_above_byte);
            }

            // If this position is quite far from the nearest known position,
            // cache the correspondence by creating a marker here.
            // It will last until the next GC.
            // But don't do it if BUF_MARKERS is nil;
            // that is a signal from Fset_buffer_multibyte.
            if record && self.markers().is_some() {
                build_marker_rust(self, best_below, best_below_byte);
            }
            if MARKER_DEBUG {
                byte_char_debug_check(self, best_below, best_below_byte);
            }

            self.is_cached = true;
            self.cached_modiff = self.modifications();

            self.cached_charpos = best_above;
            self.cached_bytepos = best_above_byte;

            best_above
        }
    }

    pub fn charpos_to_bytepos(mut self, charpos: isize) -> isize {
        assert!(self.beg() <= charpos && charpos <= self.z());

        let mut best_above = self.z();
        let mut best_above_byte = self.z_byte();

        // If this buffer has as many characters as bytes,
        // each character must be one byte.
        // This takes care of the case where enable-multibyte-characters is nil.
        if best_above == best_above_byte {
            return charpos;
        }

        let mut best_below = self.beg();
        let mut best_below_byte = self.beg_byte();

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
                        byte_char_debug_check(self, charpos, $bpos);
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

        consider_known!(self.pt, self.pt_byte);
        consider_known!(self.gpt(), self.gpt_byte());
        consider_known!(self.begv, self.begv_byte);
        consider_known!(self.zv, self.zv_byte);

        if self.is_cached && self.modifications() == self.cached_modiff {
            consider_known!(self.cached_charpos, self.cached_bytepos);
        }

        for m in self.markers().iter() {
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
                best_below_byte = self.inc_pos(best_below_byte);
            }
            if record {
                build_marker_rust(self, best_below, best_below_byte);
            }
            if MARKER_DEBUG {
                byte_char_debug_check(self, best_below, best_below_byte);
            }

            self.is_cached = true;
            self.cached_modiff = self.modifications();

            self.cached_charpos = best_below;
            self.cached_bytepos = best_below_byte;

            best_below_byte
        } else {
            let record = best_above - charpos > 5000;

            while best_above != charpos {
                best_above -= 1;
                best_above_byte = self.dec_pos(best_above_byte);
            }

            if record {
                build_marker_rust(self, best_above, best_above_byte);
            }
            if MARKER_DEBUG {
                byte_char_debug_check(self, best_below, best_below_byte);
            }

            self.is_cached = true;
            self.cached_modiff = self.modifications();

            self.cached_charpos = best_above;
            self.cached_bytepos = best_above_byte;

            best_above_byte
        }
    }

    pub fn local_vars_iter(self) -> CarIter {
        let vars = self.local_var_alist_;
        vars.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off)
    }

    pub fn overlays_before(self) -> Option<LispOverlayRef> {
        unsafe { self.overlays_before.as_ref().map(|m| mem::transmute(m)) }
    }

    pub fn overlays_after(self) -> Option<LispOverlayRef> {
        unsafe { self.overlays_after.as_ref().map(|m| mem::transmute(m)) }
    }

    pub fn as_live(self) -> Option<LispBufferRef> {
        if self.is_live() {
            Some(self)
        } else {
            None
        }
    }

    #[allow(clippy::cast_ptr_alignment)]
    pub unsafe fn set_value(&mut self, offset: usize, value: LispObject) {
        let buffer_bytes = self.as_mut() as *mut c_char;
        let pos = buffer_bytes.add(offset) as *mut LispObject;
        *pos = value;
    }

    // Reinitialize everything about a buffer except its name and contents
    // and local variables.
    // If called on an already-initialized buffer, the list of overlays
    // should be deleted before calling this function, otherwise we end up
    // with overlays that claim to belong to the buffer but the buffer
    // claims it doesn't belong to it.
    pub fn reset(&mut self) {
        self.filename_ = Qnil;
        self.file_truename_ = Qnil;
        self.directory_ = match ThreadState::current_buffer() {
            Some(current_buff) => current_buff.directory_,
            None => Qnil,
        };
        self.modtime = unsafe { make_timespec(0, UNKNOWN_MODTIME_NSECS.into()) };
        self.modtime_size = -1;
        self.save_length_ = 0.into();
        self.last_window_start = 1;
        // It is more conservative to start out "changed" than "unchanged".
        self.set_clip_changed(false);
        self.set_prevent_redisplay_optimizations_p(true);
        self.backed_up_ = Qnil;
        self.auto_save_modified = 0;
        self.auto_save_failure_time = 0;
        self.auto_save_file_name_ = Qnil;
        self.read_only_ = Qnil;
        self.overlays_before = ptr::null_mut();
        self.overlays_after = ptr::null_mut();
        self.overlay_center = BEG;
        self.mark_active_ = Qnil;
        self.point_before_scroll_ = Qnil;
        self.file_format_ = Qnil;
        self.auto_save_file_format_ = Qt;
        self.last_selected_window_ = Qnil;
        self.display_count_ = 0.into();
        self.display_time_ = Qnil;
        self.enable_multibyte_characters_ = unsafe { buffer_defaults.enable_multibyte_characters_ };
        self.cursor_type_ = unsafe { buffer_defaults.cursor_type_ };
        self.extra_line_spacing_ = unsafe { buffer_defaults.extra_line_spacing_ };
        self.display_error_modiff = 0;
    }
}

impl LispObject {
    pub fn is_buffer(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_BUFFER))
    }

    pub fn as_buffer(self) -> Option<LispBufferRef> {
        self.into()
    }

    pub fn as_live_buffer(self) -> Option<LispBufferRef> {
        self.as_buffer().and_then(LispBufferRef::as_live)
    }
}

impl From<LispObject> for LispBufferRef {
    fn from(o: LispObject) -> Self {
        o.as_buffer().unwrap_or_else(|| wrong_type!(Qbufferp, o))
    }
}

impl From<LispBufferRef> for LispObject {
    fn from(b: LispBufferRef) -> Self {
        Self::tag_ptr(b, Lisp_Type::Lisp_Vectorlike)
    }
}

impl From<LispObject> for Option<LispBufferRef> {
    fn from(o: LispObject) -> Self {
        o.as_vectorlike().and_then(LispVectorlikeRef::as_buffer)
    }
}

impl LispObject {
    pub fn is_overlay(self) -> bool {
        self.as_misc()
            .map_or(false, |m| m.get_type() == Lisp_Misc_Type::Lisp_Misc_Overlay)
    }

    pub fn as_overlay(self) -> Option<LispOverlayRef> {
        self.into()
    }
}

impl From<LispObject> for LispOverlayRef {
    fn from(o: LispObject) -> Self {
        o.as_overlay().unwrap_or_else(|| wrong_type!(Qoverlayp, o))
    }
}

impl From<LispOverlayRef> for LispObject {
    fn from(o: LispOverlayRef) -> Self {
        Self::tag_ptr(o, Lisp_Type::Lisp_Misc)
    }
}

impl From<LispObject> for Option<LispOverlayRef> {
    fn from(o: LispObject) -> Self {
        o.as_misc().and_then(LispMiscRef::as_overlay)
    }
}

impl LispMiscRef {
    pub fn as_overlay(self) -> Option<LispOverlayRef> {
        if self.get_type() == Lisp_Misc_Type::Lisp_Misc_Overlay {
            unsafe { Some(mem::transmute(self)) }
        } else {
            None
        }
    }
}

impl LispOverlayRef {
    pub fn iter(self) -> LispOverlayIter {
        LispOverlayIter {
            current: Some(self),
        }
    }
}

impl LispStructuralEqual for LispOverlayRef {
    fn equal(
        &self,
        other: Self,
        kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool {
        let overlays_equal = self.start.equal_internal(other.start, kind, depth + 1, ht)
            && self.end.equal_internal(other.end, kind, depth + 1, ht);
        overlays_equal && self.plist.equal_internal(other.plist, kind, depth + 1, ht)
    }
}

pub struct LispOverlayIter {
    current: Option<LispOverlayRef>,
}

impl Iterator for LispOverlayIter {
    type Item = LispOverlayRef;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.current;
        match c {
            None => None,
            Some(o) => {
                self.current = LispOverlayRef::from_ptr(o.next as *mut c_void);
                c
            }
        }
    }
}

pub type LispBufferLocalValueRef = ExternalPtr<Lisp_Buffer_Local_Value>;

impl LispBufferLocalValueRef {
    pub fn get_fwd(self) -> *const Lisp_Fwd {
        self.fwd
    }

    pub fn get_value(self) -> LispObject {
        let (_, d) = self.valcell.into();
        d
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum LispBufferOrName {
    Buffer(LispBufferRef),
    Name(LispStringRef),
}

impl LispBufferOrName {
    pub fn as_buffer_or_current_buffer(self) -> Option<LispBufferRef> {
        let obj = LispObject::from(self);
        obj.map_or_else(
            || Some(ThreadState::current_buffer_unchecked()),
            LispObject::as_buffer,
        )
    }
}

impl From<LispBufferOrName> for LispObject {
    fn from(buffer_or_name: LispBufferOrName) -> Self {
        match buffer_or_name {
            LispBufferOrName::Buffer(b) => b.into(),
            LispBufferOrName::Name(n) => n.into(),
        }
    }
}

impl From<LispObject> for LispBufferOrName {
    fn from(v: LispObject) -> Self {
        if let Some(s) = v.as_string() {
            LispBufferOrName::Name(s)
        } else if let Some(b) = v.as_buffer() {
            LispBufferOrName::Buffer(b)
        } else {
            wrong_type!(Qbufferp, v);
        }
    }
}

impl From<LispBufferRef> for LispBufferOrName {
    #[inline(always)]
    fn from(b: LispBufferRef) -> Self {
        LispBufferOrName::Buffer(b)
    }
}

impl From<LispStringRef> for LispBufferOrName {
    #[inline(always)]
    fn from(n: LispStringRef) -> Self {
        LispBufferOrName::Name(n)
    }
}

impl From<LispObject> for Option<LispBufferOrName> {
    fn from(v: LispObject) -> Self {
        if v.is_nil() {
            None
        } else if let Some(s) = v.as_string() {
            Some(LispBufferOrName::Name(s))
        } else if let Some(b) = v.as_buffer() {
            Some(LispBufferOrName::Buffer(b))
        } else {
            None
        }
    }
}

impl From<LispBufferOrName> for Option<LispBufferRef> {
    fn from(v: LispBufferOrName) -> Self {
        match v {
            LispBufferOrName::Buffer(b) => Some(b),
            LispBufferOrName::Name(name) => {
                let tem = unsafe { Vbuffer_alist }
                    .iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off)
                    .find(|&item| string_equal(car(item), name));

                cdr(tem.into()).as_buffer()
            }
        }
    }
}

impl From<LispBufferOrName> for LispBufferRef {
    fn from(v: LispBufferOrName) -> Self {
        Option::<LispBufferRef>::from(v).unwrap_or_else(|| nsberror(v.into()))
    }
}

pub enum LispBufferOrCurrent {
    Buffer(LispBufferRef),
    Current,
}

impl From<LispObject> for LispBufferOrCurrent {
    fn from(obj: LispObject) -> Self {
        match obj.as_buffer() {
            None => LispBufferOrCurrent::Current,
            Some(buf) => LispBufferOrCurrent::Buffer(buf),
        }
    }
}

impl From<LispBufferOrCurrent> for LispObject {
    fn from(buffer: LispBufferOrCurrent) -> Self {
        match buffer {
            LispBufferOrCurrent::Current => Qnil,
            LispBufferOrCurrent::Buffer(buf) => buf.into(),
        }
    }
}

impl From<LispBufferOrCurrent> for LispBufferRef {
    fn from(buffer: LispBufferOrCurrent) -> Self {
        match buffer {
            LispBufferOrCurrent::Buffer(buf) => buf,
            LispBufferOrCurrent::Current => ThreadState::current_buffer_unchecked(),
        }
    }
}

/// Return false.
/// If the optional arg BUFFER is provided and not nil, enable undoes in that
/// buffer, otherwise run on the current buffer.
#[lisp_fn(min = "0", intspec = "")]
pub fn buffer_enable_undo(buffer: LispBufferOrCurrent) {
    let mut buf: LispBufferRef = buffer.into();
    if buf.undo_list_.eq(Qt) {
        buf.undo_list_ = Qnil;
    }
}

/// Return a list of all live buffers.
/// If the optional arg FRAME is a frame, return the buffer list in the
/// proper order for that frame: the buffers shown in FRAME come first,
/// followed by the rest of the buffers.
#[lisp_fn(min = "0")]
pub fn buffer_list(frame: Option<LispFrameRef>) -> LispObject {
    let mut buffers: Vec<LispObject> = unsafe { Vbuffer_alist }
        .iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off)
        .map(cdr)
        .collect();

    match frame {
        None => list(&buffers),

        Some(frame) => {
            let framelist = unsafe { Fcopy_sequence(frame.buffer_list) };
            let prevlist = unsafe { Fnreverse(Fcopy_sequence(frame.buried_buffer_list)) };

            // Remove any buffer that duplicates one in FRAMELIST or PREVLIST.
            buffers.retain(|e| member(*e, framelist).is_nil() && member(*e, prevlist).is_nil());

            callN_raw!(Fnconc, framelist, list(&buffers), prevlist)
        }
    }
}

/// Return t if OBJECT is an overlay.
#[lisp_fn]
pub fn overlayp(object: LispObject) -> bool {
    object.is_overlay()
}

/// Return non-nil if OBJECT is a buffer which has not been killed.
/// Value is nil if OBJECT is not a buffer or if it has been killed.
#[lisp_fn]
pub fn buffer_live_p(object: Option<LispBufferRef>) -> bool {
    object.map_or(false, LispBufferRef::is_live)
}

/// Return the buffer named BUFFER-OR-NAME.
/// BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
/// is a string and there is no buffer with that name, return nil.  If
/// BUFFER-OR-NAME is a buffer, return it as given.
#[lisp_fn]
pub fn get_buffer(buffer_or_name: LispBufferOrName) -> Option<LispBufferRef> {
    buffer_or_name.into()
}

/// Return the current buffer as a Lisp object.
#[lisp_fn]
pub fn current_buffer() -> LispObject {
    ThreadState::current_buffer().into()
}

/// Return name of file BUFFER is visiting, or nil if none.
/// No argument or nil as argument means use the current buffer.
#[lisp_fn(min = "0")]
pub fn buffer_file_name(buffer: LispBufferOrCurrent) -> LispObject {
    let buf: LispBufferRef = buffer.into();

    buf.filename_
}

/// Return t if BUFFER was modified since its file was last read or saved.
/// No argument or nil as argument means use current buffer as BUFFER.
#[lisp_fn(min = "0")]
pub fn buffer_modified_p(buffer: LispBufferOrCurrent) -> bool {
    let buf: LispBufferRef = buffer.into();
    buf.modifications_since_save() < buf.modifications()
}

/// Return the name of BUFFER, as a string.
/// BUFFER defaults to the current buffer.
/// Return nil if BUFFER has been killed.
#[lisp_fn(min = "0")]
pub fn buffer_name(buffer: LispBufferOrCurrent) -> LispObject {
    let buf: LispBufferRef = buffer.into();
    buf.name_
}

/// Return BUFFER's tick counter, incremented for each change in text.
/// Each buffer has a tick counter which is incremented each time the
/// text in that buffer is changed.  It wraps around occasionally.
/// No argument or nil as argument means use current buffer as BUFFER.
#[lisp_fn(min = "0")]
pub fn buffer_modified_tick(buffer: LispBufferOrCurrent) -> EmacsInt {
    let buf: LispBufferRef = buffer.into();
    buf.modifications()
}

/// Return BUFFER's character-change tick counter.
/// Each buffer has a character-change tick counter, which is set to the
/// value of the buffer's tick counter (see `buffer-modified-tick'), each
/// time text in that buffer is inserted or deleted.  By comparing the
/// values returned by two individual calls of `buffer-chars-modified-tick',
/// you can tell whether a character change occurred in that buffer in
/// between these calls.  No argument or nil as argument means use current
/// buffer as BUFFER.
#[lisp_fn(min = "0")]
pub fn buffer_chars_modified_tick(buffer: LispBufferOrCurrent) -> EmacsInt {
    let buf: LispBufferRef = buffer.into();
    buf.char_modifications()
}

/// Return the position at which OVERLAY starts.
#[lisp_fn]
pub fn overlay_start(overlay: LispOverlayRef) -> Option<EmacsInt> {
    marker_position_lisp(overlay.start.into())
}

/// Return the position at which OVERLAY ends.
#[lisp_fn]
pub fn overlay_end(overlay: LispOverlayRef) -> Option<EmacsInt> {
    marker_position_lisp(overlay.end.into())
}

/// Return the buffer OVERLAY belongs to.
/// Return nil if OVERLAY has been deleted.
#[lisp_fn]
pub fn overlay_buffer(overlay: LispOverlayRef) -> Option<LispBufferRef> {
    marker_buffer(overlay.start.into())
}

/// Return a list of the properties on OVERLAY.
/// This is a copy of OVERLAY's plist; modifying its conses has no
/// effect on OVERLAY.
#[lisp_fn]
pub fn overlay_properties(overlay: LispOverlayRef) -> LispObject {
    unsafe { Fcopy_sequence(overlay.plist) }
}

#[no_mangle]
pub unsafe extern "C" fn validate_region(b: *mut LispObject, e: *mut LispObject) {
    let (begin, end) = validate_region_rust(*b, *e);
    *b = begin.into();
    *e = end.into();
}

pub fn validate_region_rust(start: LispObject, stop: LispObject) -> (isize, isize) {
    let mut begin = start.as_fixnum_coerce_marker_or_error();
    let mut end = stop.as_fixnum_coerce_marker_or_error();

    if begin > end {
        mem::swap(&mut begin, &mut end);
    }

    let buf = ThreadState::current_buffer_unchecked();
    let begv = buf.begv as EmacsInt;
    let zv = buf.zv as EmacsInt;

    if !(begv <= begin && end <= zv) {
        args_out_of_range!(current_buffer(), start, stop);
    }

    (begin as isize, end as isize)
}

/// Make buffer BUFFER-OR-NAME current for editing operations.
/// BUFFER-OR-NAME may be a buffer or the name of an existing buffer.
/// See also `with-current-buffer' when you want to make a buffer current
/// temporarily.  This function does not display the buffer, so its effect
/// ends when the current command terminates.  Use `switch-to-buffer' or
/// `pop-to-buffer' to switch buffers permanently.
/// The return value is the buffer made current.
#[lisp_fn]
pub fn set_buffer(buffer_or_name: LispBufferOrName) -> LispBufferRef {
    let mut buffer: LispBufferRef = buffer_or_name.into();
    if !buffer.is_live() {
        error!("Selecting deleted buffer");
    };
    unsafe { set_buffer_internal_1(buffer.as_mut()) };
    buffer
}

/// Signal a `buffer-read-only' error if the current buffer is read-only.
/// If the text under POSITION (which defaults to point) has the
/// `inhibit-read-only' text property set, the error will not be raised.
#[lisp_fn(min = "0")]
pub fn barf_if_buffer_read_only(position: Option<EmacsInt>) {
    let pos = position.unwrap_or_else(point);

    let inhibit_read_only: bool = unsafe { globals.Vinhibit_read_only.into() };
    let prop = unsafe { Fget_text_property(pos.into(), Qinhibit_read_only, Qnil) };

    if ThreadState::current_buffer_unchecked().is_read_only() && !inhibit_read_only && prop.is_nil()
    {
        xsignal!(Qbuffer_read_only, current_buffer())
    }
}

/// No such buffer error.
#[no_mangle]
pub extern "C" fn nsberror(spec: LispObject) -> ! {
    match spec.as_string() {
        Some(s) => error!("No buffer named {}", s),
        None => error!("Invalid buffer argument"),
    }
}

/// These functions are for debugging overlays.

/// Return a pair of lists giving all the overlays of the current buffer.
/// The car has all the overlays before the overlay center;
/// the cdr has all the overlays after the overlay center.
/// Recentering overlays moves overlays between these lists.
/// The lists you get are copies, so that changing them has no effect.
/// However, the overlays you get are the real objects that the buffer uses.
#[lisp_fn]
pub fn overlay_lists() -> LispObject {
    let list_overlays =
        |ol: LispOverlayRef| -> LispObject { ol.iter().fold(Qnil, |accum, n| (n, accum).into()) };

    let cur_buf = ThreadState::current_buffer_unchecked();
    let before = cur_buf.overlays_before().map_or(Qnil, &list_overlays);
    let after = cur_buf.overlays_after().map_or(Qnil, &list_overlays);
    unsafe { (Fnreverse(before), Fnreverse(after)).into() }
}

fn get_truename_buffer_1(filename: LispSymbolOrString) -> LispObject {
    LiveBufferIter::new()
        .find(|buf| {
            let buf_truename = buf.truename();
            buf_truename.is_string() && string_equal(buf_truename, filename)
        })
        .into()
}

#[no_mangle]
pub extern "C" fn get_truename_buffer(filename: LispObject) -> LispObject {
    get_truename_buffer_1(filename.into())
}

/// If buffer B has markers to record PT, BEGV and ZV when it is not
/// current, update these markers.
#[no_mangle]
pub extern "C" fn record_buffer_markers(buffer: *mut Lisp_Buffer) {
    let buffer_ref = LispBufferRef::from_ptr(buffer as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid buffer reference."));
    let pt_marker = buffer_ref.pt_marker();

    if pt_marker.is_not_nil() {
        let begv_marker = buffer_ref.begv_marker();
        let zv_marker = buffer_ref.zv_marker();

        assert!(begv_marker.is_not_nil());
        assert!(zv_marker.is_not_nil());

        let buffer = LispObject::from(buffer_ref);
        set_marker_both(pt_marker, buffer, buffer_ref.pt, buffer_ref.pt_byte);
        set_marker_both(begv_marker, buffer, buffer_ref.begv, buffer_ref.begv_byte);
        set_marker_both(zv_marker, buffer, buffer_ref.zv, buffer_ref.zv_byte);
    }
}

/// If buffer B has markers to record PT, BEGV and ZV when it is not
/// current, fetch these values into B->begv etc.
#[no_mangle]
pub extern "C" fn fetch_buffer_markers(buffer: *mut Lisp_Buffer) {
    let mut buffer_ref = LispBufferRef::from_ptr(buffer as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid buffer reference."));

    if buffer_ref.pt_marker().is_not_nil() {
        assert!(buffer_ref.begv_marker().is_not_nil());
        assert!(buffer_ref.zv_marker().is_not_nil());

        let pt_marker: LispMarkerRef = buffer_ref.pt_marker().into();
        let begv_marker: LispMarkerRef = buffer_ref.begv_marker().into();
        let zv_marker: LispMarkerRef = buffer_ref.zv_marker().into();

        buffer_ref.set_pt_both(pt_marker.charpos_or_error(), pt_marker.bytepos_or_error());
        buffer_ref.set_begv_both(
            begv_marker.charpos_or_error(),
            begv_marker.bytepos_or_error(),
        );
        buffer_ref.set_zv_both(zv_marker.charpos_or_error(), zv_marker.bytepos_or_error());
    }
}

/// Return the buffer visiting file FILENAME (a string).
/// The buffer's `buffer-file-name' must match exactly the expansion of FILENAME.
/// If there is no such live buffer, return nil.
/// See also `find-buffer-visiting'.
#[lisp_fn]
pub fn get_file_buffer(filename: LispStringRef) -> Option<LispBufferRef> {
    let filename = expand_file_name(filename, None);

    // If the file name has special constructs in it,
    // call the corresponding file handler.
    let handler = find_file_name_handler(filename, Qget_file_buffer);

    if handler.is_not_nil() {
        let handled_buf = call!(handler, Qget_file_buffer, filename.into());
        handled_buf.as_buffer()
    } else {
        LiveBufferIter::new().find(|buf| {
            let buf_filename = buf.filename();
            buf_filename.is_string() && string_equal(buf_filename, filename)
        })
    }
}

/// Return the value of VARIABLE in BUFFER.
/// If VARIABLE does not have a buffer-local binding in BUFFER, the value
/// is the default binding of the variable.
#[lisp_fn(name = "buffer-local-value", c_name = "buffer_local_value")]
pub fn buffer_local_value_lisp(variable: LispObject, buffer: LispObject) -> LispObject {
    let result = unsafe { buffer_local_value(variable, buffer) };

    if result.eq(Qunbound) {
        void_variable!(variable);
    }

    result
}

/// Return the base buffer of indirect buffer BUFFER.
/// If BUFFER is not indirect, return nil.
/// BUFFER defaults to the current buffer.
#[lisp_fn(min = "0")]
pub fn buffer_base_buffer(buffer: LispBufferOrCurrent) -> Option<LispBufferRef> {
    let buf: LispBufferRef = buffer.into();
    buf.base_buffer()
}

/// Force redisplay of the current buffer's mode line and header line.
/// With optional non-nil ALL, force redisplay of all mode lines and
/// header lines.  This function also forces recomputation of the
/// menu bar menus and the frame title.
#[lisp_fn(min = "0")]
pub fn force_mode_line_update(all: bool) -> bool {
    let mut current_buffer = ThreadState::current_buffer_unchecked();
    if all {
        unsafe {
            update_mode_lines = 10;
        }
    // FIXME: This can't be right.
    // current_buffer.set_prevent_redisplay_optimizations_p(true);
    } else if 0 < unsafe { buffer_window_count(current_buffer.as_mut()) } {
        unsafe {
            bset_update_mode_line(current_buffer.as_mut());
        }
        current_buffer.set_prevent_redisplay_optimizations_p(true);
    }
    all
}

/// Return a Lisp_Misc_Overlay object with specified START, END and PLIST.
#[no_mangle]
pub extern "C" fn build_overlay(
    start: LispObject,
    end: LispObject,
    plist: LispObject,
) -> LispObject {
    unsafe {
        let obj = allocate_misc(Lisp_Misc_Type::Lisp_Misc_Overlay);
        let mut overlay: LispOverlayRef = obj.into();
        overlay.start = start;
        overlay.end = end;
        overlay.plist = plist;
        overlay.next = ptr::null_mut();

        overlay.into()
    }
}

/// Get the property of overlay OVERLAY with property name PROP.
#[lisp_fn]
pub fn overlay_get(overlay: LispOverlayRef, prop: LispObject) -> LispObject {
    unsafe { lookup_char_property(overlay.plist, prop, false) }
}

// Mark OV as no longer associated with BUF.
#[no_mangle]
pub extern "C" fn drop_overlay(mut buf: LispBufferRef, ov: LispOverlayRef) {
    let mut start: LispMarkerRef = ov.start.into();
    let mut end: LispMarkerRef = ov.end.into();
    assert!(buf == marker_buffer(start).unwrap());
    unsafe {
        modify_overlay(
            buf.as_mut(),
            marker_position(ov.start),
            marker_position(ov.end),
        );
        unchain_marker(start.as_mut());
        unchain_marker(end.as_mut());
    }
}

/// Delete the overlay OVERLAY from its buffer.
#[lisp_fn]
pub fn delete_overlay(overlay: LispOverlayRef) {
    let mut buf_ref = match marker_buffer(LispMarkerRef::from(overlay.start)) {
        Some(b) => b,
        None => return,
    };
    let count = c_specpdl_index();

    unsafe {
        specbind(Qinhibit_quit, Qt);
        unchain_both(buf_ref.as_mut(), overlay.into());
        drop_overlay(buf_ref, overlay);

        // When deleting an overlay with before or after strings, turn off
        // display optimizations for the affected buffer, on the basis that
        // these strings may contain newlines.  This is easier to do than to
        // check for that situation during redisplay.
        if windows_or_buffers_changed != 0 && overlay_get(overlay, Qbefore_string).is_not_nil()
            || overlay_get(overlay, Qafter_string).is_not_nil()
        {
            buf_ref.set_prevent_redisplay_optimizations_p(true);
        }
    }

    unbind_to(count, Qnil);
}

/// Delete all overlays of BUFFER.
/// BUFFER omitted or nil means delete all overlays of the current buffer.
#[lisp_fn(
    min = "0",
    name = "delete-all-overlays",
    c_name = "delete_all_overlays"
)]
pub fn delete_all_overlays_lisp(buffer: LispBufferOrCurrent) {
    let mut buf: LispBufferRef = buffer.into();
    unsafe { delete_all_overlays(buf.as_mut()) };
}

/// Delete the entire contents of the current buffer.
/// Any narrowing restriction in effect (see `narrow-to-region') is removed,
/// so the buffer is truly empty after this.
#[lisp_fn(intspec = "*")]
pub fn erase_buffer() {
    widen();
    unsafe {
        let mut cur_buf = ThreadState::current_buffer_unchecked();
        del_range(cur_buf.beg(), cur_buf.z());

        cur_buf.last_window_start = 1;

        // Prevent warnings, or suspension of auto saving, that would happen
        // if future size is less than past size.  Use of erase-buffer
        // implies that the future text is not really related to the past text.
        cur_buf.save_length_ = 0.into();
    }
}

// We split this away from generate-new-buffer, because rename-buffer
// and set-visited-file-name ought to be able to use this to really
// rename the buffer properly.

/// Return a string that is the name of no existing buffer based on NAME.
/// If there is no live buffer named NAME, then return NAME.
/// Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
/// (starting at 2) until an unused name is found, and then return that name.
/// Optional second argument IGNORE specifies a name that is okay to use (if
/// it is in the sequence to be tried) even if a buffer with that name exists.
///
/// If NAME begins with a space (i.e., a buffer that is not normally
/// visible to users), then if buffer NAME already exists a random number
/// is first appended to NAME, to speed up finding a non-existent buffer.
#[lisp_fn(min = "1")]
pub fn generate_new_buffer_name(name: LispStringRef, ignore: LispObject) -> LispStringRef {
    if (ignore.is_not_nil() && string_equal(name, ignore)) || get_buffer(name.into()).is_none() {
        return name;
    }

    let basename = if name.byte_at(0) == b' ' {
        let mut s = format!("-{}", thread_rng().gen_range(0, 1_000_000));
        local_unibyte_string!(suffix, s);
        let genname = unsafe { concat2(name.into(), suffix) };
        if get_buffer(genname.into()).is_none() {
            return genname.into();
        }
        genname
    } else {
        name.into()
    };

    let mut suffix_count = 2;
    loop {
        let mut s = format!("<{}>", suffix_count);
        local_unibyte_string!(suffix, s);
        let candidate = unsafe { concat2(basename, suffix) }.force_string();
        if string_equal(candidate, ignore) || get_buffer(candidate.into()).is_none() {
            return candidate;
        }
        suffix_count += 1;
    }
}

pub unsafe fn per_buffer_idx_from_field_offset(
    offset: FieldOffset<Lisp_Buffer, LispObject>,
) -> isize {
    let obj = *offset.apply_ptr_mut(&mut buffer_local_flags);
    obj.to_fixnum_unchecked() as isize
}

pub unsafe fn per_buffer_idx(count: isize) -> isize {
    let flags = &mut buffer_local_flags as *mut Lisp_Buffer as *mut LispObject;
    let obj = flags.offset(count);
    (*obj).to_fixnum_unchecked() as isize
}

/// Return a list of overlays which is a copy of the overlay list
/// LIST, but for buffer BUFFER.
#[no_mangle]
pub unsafe extern "C" fn copy_overlays(
    buffer: *mut Lisp_Buffer,
    list: *mut Lisp_Overlay,
) -> *mut Lisp_Overlay {
    if list.is_null() {
        return list;
    }

    let mut result = ptr::null_mut();

    let overlays_iter = LispOverlayRef::from_ptr(list as *mut c_void)
        .unwrap_or_else(|| panic!("Invalid overlay reference."))
        .iter();

    let duplicate_marker = |marker_obj: LispObject| -> LispObject {
        let mkr: LispMarkerRef = marker_obj.into();
        let new_mkr = build_marker(buffer, mkr.charpos_or_error(), mkr.bytepos_or_error());
        LispMarkerRef::from(new_mkr).set_insertion_type(mkr.insertion_type());
        new_mkr
    };

    let _ = overlays_iter.fold(None, |tail: Option<LispOverlayRef>, overlay| {
        let start = duplicate_marker(overlay.start);
        let end = duplicate_marker(overlay.end);

        let mut overlay_new: LispOverlayRef =
            build_overlay(start, end, Fcopy_sequence(overlay.plist)).into();

        match tail {
            Some(mut tail_ref) => tail_ref.next = overlay_new.as_mut(),
            None => result = overlay_new.as_mut(),
        }

        Some(overlay_new)
    });

    result
}

#[no_mangle]
pub extern "C" fn reset_buffer(mut buffer: LispBufferRef) {
    buffer.reset();
}

#[no_mangle]
pub extern "C" fn rust_syms_of_buffer() {
    def_lisp_sym!(Qget_file_buffer, "get-file-buffer");

    /// Analogous to `mode-line-format', but controls the header line.
    /// The header line appears, optionally, at the top of a window;
    /// the mode line appears at the bottom.
    defvar_per_buffer!(header_line_format_, "header-line-format", Qnil);
}

/// Change current buffer's name to NEWNAME (a string).  If second arg
/// UNIQUE is nil or omitted, it is an error if a buffer named NEWNAME
/// already exists.  If UNIQUE is non-nil, come up with a new name
/// using `generate-new-buffer-name'.  Interactively, you can set
/// UNIQUE with a prefix argument.  We return the name we actually
/// gave the buffer.  This does not change the name of the visited
/// file (if any).
#[lisp_fn(
    min = "1",
    intspec = "(list (read-string \"Rename buffer (to new name): \" nil 'buffer-name-history (buffer-name (current-buffer))) current-prefix-arg)"
)]
pub fn rename_buffer(newname: LispStringRef, unique: LispObject) -> LispStringRef {
    if newname.is_empty() {
        error!("Empty string is invalid as a buffer name")
    }

    let mut current_buffer = ThreadState::current_buffer_unchecked();

    let newname = get_buffer(newname.into()).map_or(newname, |tem| {
        // Don't short-circuit if UNIQUE is t.  That is a useful
        // way to rename the buffer automatically so you can
        // create another with the original name.  It makes UNIQUE
        // equivalent to
        // (rename-buffer (generate-new-buffer-name NEWNAME)).
        if unique.is_nil() {
            if tem == current_buffer {
                return current_buffer.name_.into();
            }
            error!("Buffer name `{}' is in use", newname)
        } else {
            generate_new_buffer_name(newname, current_buffer.name_)
        }
    });

    current_buffer.name_ = newname.into();

    // Catch redisplay's attention.  Unless we do this, the mode lines
    // for any windows displaying current_buffer will stay unchanged.
    unsafe {
        update_mode_lines = 11;
    }

    let buf: LispBufferRef = current_buffer;
    unsafe {
        setcar(rassq(buf.into(), Vbuffer_alist).into(), newname.into());
    }
    if current_buffer.filename_.is_nil() && current_buffer.auto_save_file_name_.is_not_nil() {
        call!(intern("rename-auto-save-file").into());
    }

    unsafe {
        if Vrun_hooks.is_not_nil() {
            call!(Vrun_hooks, Qbuffer_list_update_hook);
        }
    }

    // Refetch since that last call may have done GC.
    ThreadState::current_buffer_unchecked().name_.into()
}

// Debugging

pub fn byte_char_debug_check(b: LispBufferRef, charpos: isize, bytepos: isize) {
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

include!(concat!(env!("OUT_DIR"), "/buffers_exports.rs"));
