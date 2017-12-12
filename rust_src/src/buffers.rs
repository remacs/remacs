//! Functions operating on buffers.

use libc::{c_int, c_uchar, c_void, ptrdiff_t};
use std::{mem, ptr};

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Lisp_Buffer, Lisp_Object, Lisp_Overlay, Lisp_Type, Vbuffer_alist};
use remacs_sys::{make_lisp_ptr, nsberror, set_buffer_internal};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use lists::{car, cdr};
use marker::{marker_buffer, marker_position};
use multibyte::string_char;
use strings::string_equal;
use threads::ThreadState;
use util::clip_to_bounds;

pub const BEG: ptrdiff_t = 1;
pub const BEG_BYTE: ptrdiff_t = 1;

pub type LispBufferRef = ExternalPtr<Lisp_Buffer>;
pub type LispOverlayRef = ExternalPtr<Lisp_Overlay>;

impl LispBufferRef {
    #[inline]
    pub fn zv(self) -> ptrdiff_t {
        self.zv
    }

    #[inline]
    pub fn pt(self) -> ptrdiff_t {
        self.pt
    }

    #[inline]
    pub fn beg_addr(self) -> *mut c_uchar {
        unsafe { (*self.text).beg }
    }

    #[inline]
    pub fn beg(self) -> ptrdiff_t {
        BEG
    }

    #[inline]
    pub fn beg_byte(self) -> ptrdiff_t {
        BEG_BYTE
    }

    #[inline]
    pub fn gpt_byte(self) -> ptrdiff_t {
        unsafe { (*self.text).gpt_byte }
    }

    #[inline]
    pub fn gap_size(self) -> ptrdiff_t {
        unsafe { (*self.text).gap_size }
    }

    #[inline]
    pub fn gap_position(self) -> ptrdiff_t {
        unsafe { (*self.text).gpt }
    }

    #[inline]
    pub fn gap_end_addr(self) -> *mut c_uchar {
        unsafe {
            (*self.text)
                .beg
                .offset((*self.text).gpt_byte + (*self.text).gap_size - BEG_BYTE)
        }
    }

    #[inline]
    pub fn z_addr(self) -> *mut c_uchar {
        unsafe {
            (*self.text)
                .beg
                .offset((*self.text).gap_size + (*self.text).z_byte - BEG_BYTE)
        }
    }

    #[inline]
    pub fn z_byte(self) -> ptrdiff_t {
        unsafe { (*self.text).z_byte }
    }

    #[inline]
    pub fn z(self) -> ptrdiff_t {
        unsafe { (*self.text).z }
    }

    /// Number of modifications made to the buffer.
    #[inline]
    pub fn modifications(self) -> EmacsInt {
        unsafe { (*self.text).modiff }
    }

    /// Value of `modiff` last time the buffer was saved.
    #[inline]
    pub fn modifications_since_save(self) -> EmacsInt {
        unsafe { (*self.text).save_modiff }
    }

    /// Number of modifications to the buffer's characters.
    #[inline]
    pub fn char_modifications(self) -> EmacsInt {
        unsafe { (*self.text).chars_modiff }
    }

    #[inline]
    pub fn mark_active(self) -> LispObject {
        LispObject::from(self.mark_active)
    }

    #[inline]
    pub fn mark(self) -> LispObject {
        LispObject::from(self.mark)
    }

    #[allow(dead_code)]
    #[inline]
    pub fn name(self) -> LispObject {
        LispObject::from(self.name)
    }

    // Check if buffer is live
    #[inline]
    pub fn is_live(self) -> bool {
        LispObject::from(self.name).is_not_nil()
    }

    #[inline]
    pub fn fetch_byte(self, n: ptrdiff_t) -> u8 {
        let offset = if n >= self.gpt_byte() {
            self.gap_size()
        } else {
            0
        };

        unsafe { *(self.beg_addr().offset(offset + n - self.beg_byte())) as u8 }
    }

    #[inline]
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

    #[inline]
    pub fn fetch_char(self, n: ptrdiff_t) -> c_int {
        if LispObject::from(self.enable_multibyte_characters).is_not_nil() {
            self.fetch_multibyte_char(n)
        } else {
            c_int::from(self.fetch_byte(n))
        }
    }
}

impl LispOverlayRef {
    #[inline]
    pub fn start(self) -> LispObject {
        LispObject::from(self.start)
    }

    #[inline]
    pub fn end(self) -> LispObject {
        LispObject::from(self.end)
    }
}

impl LispObject {
    /// Return SELF as a struct buffer pointer, defaulting to the current buffer.
    /// Same as the decode_buffer function in buffer.h
    #[inline]
    pub fn as_buffer_or_current_buffer(self) -> LispBufferRef {
        if self.is_nil() {
            ThreadState::current_buffer()
        } else {
            self.as_buffer_or_error()
        }
    }
}

/// Return t if OBJECT is an overlay.
#[lisp_fn]
pub fn overlayp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_overlay())
}

/// Return non-nil if OBJECT is a buffer which has not been killed.
/// Value is nil if OBJECT is not a buffer or if it has been killed.
#[lisp_fn]
pub fn buffer_live_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_buffer().map_or(false, |m| m.is_live()))
}

/// Like Fassoc, but use `Fstring_equal` to compare
/// (which ignores text properties), and don't ever quit.
fn assoc_ignore_text_properties(key: LispObject, list: LispObject) -> LispObject {
    let result = list.iter_tails_safe()
        .find(|&item| string_equal(car(item.car()), key).is_not_nil());
    if let Some(elt) = result {
        elt.car()
    } else {
        LispObject::constant_nil()
    }
}

/// Return the buffer named BUFFER-OR-NAME.
/// BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
/// is a string and there is no buffer with that name, return nil.  If
/// BUFFER-OR-NAME is a buffer, return it as given.
#[lisp_fn]
pub fn get_buffer(buffer_or_name: LispObject) -> LispObject {
    if buffer_or_name.is_buffer() {
        buffer_or_name
    } else {
        buffer_or_name.as_string_or_error();
        cdr(assoc_ignore_text_properties(
            buffer_or_name,
            LispObject::from(unsafe { Vbuffer_alist }),
        ))
    }
}

/// Return the current buffer as a Lisp object.
#[lisp_fn]
pub fn current_buffer() -> LispObject {
    let buffer_ref = ThreadState::current_buffer();
    unsafe {
        LispObject::from(make_lisp_ptr(
            buffer_ref.as_ptr() as *mut c_void,
            Lisp_Type::Lisp_Vectorlike,
        ))
    }
}

/// Return name of file BUFFER is visiting, or nil if none.
/// No argument or nil as argument means use the current buffer.
#[lisp_fn(min = "0")]
pub fn buffer_file_name(buffer: LispObject) -> LispObject {
    let buf = if buffer.is_nil() {
        ThreadState::current_buffer()
    } else {
        buffer.as_buffer_or_error()
    };

    LispObject::from(buf.filename)
}

/// Return t if BUFFER was modified since its file was last read or saved.
/// No argument or nil as argument means use current buffer as BUFFER.
#[lisp_fn(min = "0")]
pub fn buffer_modified_p(buffer: LispObject) -> LispObject {
    let buf = buffer.as_buffer_or_current_buffer();
    LispObject::from_bool(buf.modifications_since_save() < buf.modifications())
}

/// Return the name of BUFFER, as a string.
/// BUFFER defaults to the current buffer.
/// Return nil if BUFFER has been killed.
#[lisp_fn(min = "0")]
pub fn buffer_name(buffer: LispObject) -> LispObject {
    LispObject::from(buffer.as_buffer_or_current_buffer().name)
}

/// Return BUFFER's tick counter, incremented for each change in text.
/// Each buffer has a tick counter which is incremented each time the
/// text in that buffer is changed.  It wraps around occasionally.
/// No argument or nil as argument means use current buffer as BUFFER.
#[lisp_fn(min = "0")]
pub fn buffer_modified_tick(buffer: LispObject) -> LispObject {
    LispObject::from_fixnum(buffer.as_buffer_or_current_buffer().modifications())
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
pub fn buffer_chars_modified_tick(buffer: LispObject) -> LispObject {
    LispObject::from_fixnum(buffer.as_buffer_or_current_buffer().char_modifications())
}

/// Return the position at which OVERLAY starts.
#[lisp_fn]
pub fn overlay_start(overlay: LispObject) -> LispObject {
    let marker = overlay.as_overlay_or_error().start();
    marker_position(marker)
}

/// Return the position at which OVERLAY ends.
#[lisp_fn]
pub fn overlay_end(overlay: LispObject) -> LispObject {
    let marker = overlay.as_overlay_or_error().end();
    marker_position(marker)
}

/// Recenter the overlays of the current buffer around position POS.
/// That makes overlay lookup faster for positions near POS (but perhaps slower
/// for positions far away from POS).
#[lisp_fn]
pub fn overlay_recenter(pos: LispObject) -> LispObject {
    let p: ptrdiff_t = clip_to_bounds(
        ptrdiff_t::min_value(),
        pos.as_fixnum_coerce_marker_or_error(),
        ptrdiff_t::max_value(),
    );
    unsafe { recenter_overlay_lists(ThreadState::current_buffer(), p) };
    LispObject::constant_nil()
}

/// Return the buffer OVERLAY belongs to.
/// Return nil if OVERLAY has been deleted.
#[lisp_fn]
pub fn overlay_buffer(overlay: LispObject) -> LispObject {
    let marker = overlay.as_overlay_or_error().start();
    marker_buffer(marker)
}

pub fn recenter_overlay_lists(mut buf: LispBufferRef, pos: ptrdiff_t) {
    // let mut overlay: LispObject;
    let (mut prev, mut tail, mut next): (
        Option<LispOverlayRef>,
        LispOverlayRef,
        Option<LispOverlayRef>,
    ) = (
        None,
        LispOverlayRef::new((*buf).overlays_before as *mut Lisp_Overlay),
        None,
    );
    loop {
        if tail.as_ptr() as ptrdiff_t == 0 {
            break;
        }
        let (beg, end, curNext) = (tail.start(), tail.end(), tail.next);
        if end.as_fixnum_coerce_marker_or_error() as ptrdiff_t > pos {
            let begPtr = beg.as_fixnum_coerce_marker_or_error() as ptrdiff_t;

            match prev {
                Some(mut tailPtr) => tailPtr.next = curNext,
                None => set_buffer_overlays_before(buf, curNext),
            }

            loop {
                let (mut other_prev, mut other): (
                    Option<LispOverlayRef>,
                    LispOverlayRef,
                ) = (
                    None,
                    LispOverlayRef::new((*buf).overlays_after as *mut Lisp_Overlay),
                );
            }
        }
        // TODO: check loop changes
        prev = Some(tail);
        tail = next.unwrap()
    }
    (*buf).overlay_center = pos
}

#[inline]
fn set_buffer_overlays_before(mut buf: LispBufferRef, overlay: *const Lisp_Overlay) {
    (*buf).overlays_before = overlay as *mut c_void
}

#[inline]
fn set_buffer_overlays_after(mut buf: LispBufferRef, overlay: *const Lisp_Overlay) {
    (*buf).overlays_after = overlay as *mut c_void
}

#[no_mangle]
pub extern "C" fn validate_region(b: *mut Lisp_Object, e: *mut Lisp_Object) {
    let start = LispObject::from(unsafe { *b });
    let stop = LispObject::from(unsafe { *e });

    let mut beg = start.as_fixnum_coerce_marker_or_error();
    let mut end = stop.as_fixnum_coerce_marker_or_error();

    if beg > end {
        mem::swap(&mut beg, &mut end);
    }

    unsafe {
        *b = LispObject::from_fixnum(beg).to_raw();
        *e = LispObject::from_fixnum(end).to_raw();
    }

    let buf = ThreadState::current_buffer();
    let begv = buf.begv as EmacsInt;
    let zv = buf.zv as EmacsInt;

    if !(begv <= beg && end <= zv) {
        args_out_of_range!(current_buffer(), start, stop);
    }
}

/// Make buffer BUFFER-OR-NAME current for editing operations.
/// BUFFER-OR-NAME may be a buffer or the name of an existing buffer.
/// See also `with-current-buffer' when you want to make a buffer current
/// temporarily.  This function does not display the buffer, so its effect
/// ends when the current command terminates.  Use `switch-to-buffer' or
/// `pop-to-buffer' to switch buffers permanently.
/// The return value is the buffer made current.
#[lisp_fn]
pub fn set_buffer(buffer_or_name: LispObject) -> LispObject {
    let buffer = get_buffer(buffer_or_name);
    if buffer.is_nil() {
        unsafe { nsberror(buffer_or_name.to_raw()) }
    };
    let mut buf = buffer.as_buffer_or_error();
    if !buf.is_live() {
        error!("Selecting deleted buffer");
    };
    unsafe { set_buffer_internal(buf.as_mut()) };
    buffer
}

include!(concat!(env!("OUT_DIR"), "/buffers_exports.rs"));
