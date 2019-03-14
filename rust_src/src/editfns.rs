//! Lisp functions pertaining to editing.

use std;
use std::cmp::max;
use std::ops::{Add, Sub};
use std::ptr;

use libc;
use libc::{c_char, c_int, c_uchar, ptrdiff_t};

use remacs_macros::lisp_fn;

use crate::{
    buffers::{current_buffer, validate_region_rust},
    buffers::{LispBufferOrCurrent, LispBufferOrName, LispBufferRef, BUF_BYTES_MAX},
    character::{char_head_p, dec_pos},
    eval::{progn, record_unwind_protect, unbind_to},
    indent::invalidate_current_column,
    lisp::LispObject,
    marker::{marker_position_lisp, point_marker, set_point_from_marker},
    multibyte::{
        is_single_byte_char, multibyte_char_at, raw_byte_codepoint, raw_byte_from_codepoint,
        unibyte_to_char, write_codepoint, MAX_MULTIBYTE_LENGTH,
    },
    multibyte::{Codepoint, LispStringRef},
    numbers::LispNumber,
    remacs_sys::EmacsInt,
    remacs_sys::{
        buffer_overflow, build_string, chars_in_text, current_message, del_range, del_range_1,
        downcase, find_before_next_newline, find_newline, get_char_property_and_overlay, globals,
        insert_1_both, insert_from_buffer, insert_from_string_1, make_buffer_string,
        make_buffer_string_both, make_save_obj_obj_obj_obj, make_string_from_bytes, maybe_quit,
        message1, message3, record_unwind_current_buffer, save_excursion_restore,
        save_restriction_restore, save_restriction_save, scan_newline_from_point,
        set_buffer_internal_1, set_point, set_point_both, signal_after_change, styled_format,
        update_buffer_properties, update_compositions, CHECK_BORDER, STRING_BYTES,
    },
    remacs_sys::{
        Fadd_text_properties, Fcopy_sequence, Fget_pos_property, Fnext_single_char_property_change,
        Fprevious_single_char_property_change, Fx_popup_dialog,
    },
    remacs_sys::{
        Qboundary, Qchar_or_string_p, Qfield, Qinteger_or_marker_p, Qmark_inactive, Qnil, Qt,
    },
    textprop::get_char_property,
    threads::{c_specpdl_index, ThreadState},
    time::{lisp_time_struct, time_overflow, LispTime},
    util::clip_to_bounds,
    windows::{selected_window, LispWindowRef},
};

/// Return value of point, as an integer.
/// Beginning of buffer is position (point-min).
#[lisp_fn]
pub fn point() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.pt as EmacsInt
}

/// Return the number of characters in the current buffer.
/// If BUFFER is not nil, return the number of characters in that buffer
/// instead.
///
/// This does not take narrowing into account; to count the number of
/// characters in the accessible portion of the current buffer, use
/// `(- (point-max) (point-min))', and to count the number of characters
/// in some other BUFFER, use
/// `(with-current-buffer BUFFER (- (point-max) (point-min)))'.
#[lisp_fn(min = "0")]
pub fn buffer_size(buffer: LispBufferOrCurrent) -> EmacsInt {
    let buffer_ref: LispBufferRef = buffer.into();
    (buffer_ref.z() - buffer_ref.beg()) as EmacsInt
}

/// Return t if point is at the end of the buffer.
/// If the buffer is narrowed, this means the end of the narrowed part.
#[lisp_fn]
pub fn eobp() -> bool {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.zv == buffer_ref.pt
}

/// Return t if point is at the beginning of the buffer.  If the
/// buffer is narrowed, this means the beginning of the narrowed part.
#[lisp_fn]
pub fn bobp() -> bool {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.pt == buffer_ref.begv
}

/// Return t if point is at the beginning of a line.
#[lisp_fn]
pub fn bolp() -> bool {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.pt == buffer_ref.begv || buffer_ref.fetch_byte(buffer_ref.pt_byte - 1) == b'\n'
}

/// Return t if point is at the end of a line.
/// `End of a line' includes point being at the end of the buffer.
#[lisp_fn]
pub fn eolp() -> bool {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.pt == buffer_ref.zv || buffer_ref.fetch_byte(buffer_ref.pt_byte) == b'\n'
}

/// Return the position of the gap, in the current buffer.
/// See also `gap-size'.
#[lisp_fn]
pub fn gap_position() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.gap_position() as EmacsInt
}

/// Return the size of the current buffer's gap.
/// See also `gap-position'.
#[lisp_fn]
pub fn gap_size() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    buffer_ref.gap_size() as EmacsInt
}

/// Return the start or end position of the region.
/// BEGINNINGP means return the start.
/// If there is no region active, signal an error.
fn region_limit(beginningp: bool) -> EmacsInt {
    let current_buf = ThreadState::current_buffer_unchecked();
    if unsafe { globals.Vtransient_mark_mode }.is_not_nil()
        && unsafe { globals.Vmark_even_if_inactive }.is_nil()
        && current_buf.mark_active().is_nil()
    {
        xsignal!(Qmark_inactive);
    }

    let m = marker_position_lisp(current_buf.mark().into());
    let num = m.unwrap_or_else(|| error!("The mark is not set now, so there is no region"));

    // Clip to the current narrowing (bug#11770)
    if ((current_buf.pt as EmacsInt) < num) == beginningp {
        current_buf.pt as EmacsInt
    } else {
        clip_to_bounds(current_buf.begv, num, current_buf.zv) as EmacsInt
    }
}

/// Return the integer value of point or mark, whichever is smaller.
#[lisp_fn]
pub fn region_beginning() -> EmacsInt {
    region_limit(true)
}

/// Return the integer value of point or mark, whichever is larger.
#[lisp_fn]
pub fn region_end() -> EmacsInt {
    region_limit(false)
}

/// Return this buffer's mark, as a marker object.
/// Watch out!  Moving this marker changes the mark position.
/// If you set the marker not to point anywhere, the buffer will have no mark.
#[lisp_fn]
pub fn mark_marker() -> LispObject {
    ThreadState::current_buffer_unchecked().mark()
}

/// Return the minimum permissible value of point in the current
/// buffer.  This is 1, unless narrowing (a buffer restriction) is in
/// effect.
#[lisp_fn]
pub fn point_min() -> EmacsInt {
    ThreadState::current_buffer_unchecked().begv as EmacsInt
}

/// Return the maximum permissible value of point in the current
/// buffer.  This is (1+ (buffer-size)), unless narrowing (a buffer
/// restriction) is in effect, in which case it is less.
#[lisp_fn]
pub fn point_max() -> EmacsInt {
    ThreadState::current_buffer_unchecked().zv as EmacsInt
}

/// Set point to POSITION, a number or marker.
/// Beginning of buffer is position (point-min), end is (point-max).
///
/// The return value is POSITION.
#[lisp_fn(intspec = "NGoto char: ")]
pub fn goto_char(position: LispObject) -> LispObject {
    if position.is_marker() {
        set_point_from_marker(position);
    } else if let Some(num) = position.as_fixnum() {
        let cur_buf = ThreadState::current_buffer_unchecked();
        let pos = clip_to_bounds(cur_buf.begv, num, cur_buf.zv);
        let bytepos = cur_buf.charpos_to_bytepos(pos);
        unsafe { set_point_both(pos, bytepos) };
    } else {
        wrong_type!(Qinteger_or_marker_p, position)
    };
    position
}

/// Return the byte position for character position POSITION.
/// If POSITION is out of range, the value is nil.
#[lisp_fn]
pub fn position_bytes(position: LispNumber) -> Option<EmacsInt> {
    let pos = position.to_fixnum() as ptrdiff_t;
    let cur_buf = ThreadState::current_buffer_unchecked();

    if pos >= cur_buf.begv && pos <= cur_buf.zv {
        let bytepos = cur_buf.charpos_to_bytepos(pos);
        Some(bytepos as EmacsInt)
    } else {
        None
    }
}

/// TODO: Write better docstring
/// Insert COUNT (second arg) copies of BYTE (first arg).
/// Both arguments are required.
/// BYTE is a number of the range 0..255.
///
/// If BYTE is 128..255 and the current buffer is multibyte, the
/// corresponding eight-bit character is inserted.
///
/// Point, and before-insertion markers, are relocated as in the function `insert'.
/// The optional third arg INHERIT, if non-nil, says to inherit text properties
/// from adjoining text, if those properties are sticky.
#[lisp_fn(min = "2")]
pub fn insert_byte(byte: EmacsInt, count: Option<EmacsInt>, inherit: bool) {
    if byte < 0 || byte > 255 {
        args_out_of_range!(byte, 0, 255)
    }
    let buf = ThreadState::current_buffer_unchecked();
    let toinsert = if byte >= 128 && buf.multibyte_characters_enabled() {
        EmacsInt::from(raw_byte_codepoint(byte as c_uchar))
    } else {
        byte
    };
    insert_char(toinsert as Codepoint, count, inherit);
}

/// Insert COUNT copies of CHARACTER.
/// Interactively, prompt for CHARACTER.  You can specify CHARACTER in one
/// of these ways:
///
///  - As its Unicode character name, e.g. \"LATIN SMALL LETTER A\".
///    Completion is available; if you type a substring of the name
///    preceded by an asterisk `*', Emacs shows all names which include
///    that substring, not necessarily at the beginning of the name.
///
///  - As a hexadecimal code point, e.g. 263A.  Note that code points in
///    Emacs are equivalent to Unicode up to 10FFFF (which is the limit of
///    the Unicode code space).
///
///  - As a code point with a radix specified with #, e.g. #o21430
///    (octal), #x2318 (hex), or #10r8984 (decimal).
///
/// If called interactively, COUNT is given by the prefix argument.  If
/// omitted or nil, it defaults to 1.
///
/// Inserting the character(s) relocates point and before-insertion
/// markers in the same ways as the function `insert'.
///
/// The optional third argument INHERIT, if non-nil, says to inherit text
/// properties from adjoining text, if those properties are sticky.  If
/// called interactively, INHERIT is t.
#[lisp_fn(
    min = "1",
    intspec = "(list (read-char-by-name \"Insert character (Unicode name or hex): \") (prefix-numeric-value current-prefix-arg) t))"
)]
pub fn insert_char(character: Codepoint, count: Option<EmacsInt>, inherit: bool) {
    // 4000 bytes is a magic number chosen deep in the past for unknown reasons
    const BUFSIZE: usize = 4000;

    let count = count.unwrap_or(1);

    if count <= 0 {
        return;
    }

    let mut str = [0_u8; MAX_MULTIBYTE_LENGTH];
    let len = write_codepoint(&mut str[..], character);

    if BUF_BYTES_MAX / (len as isize) < (count as isize) {
        unsafe { buffer_overflow() };
    }
    let mut n = (count as usize) * len;
    let mut buffer = [0_u8; BUFSIZE];
    // bufferlen is the number of bytes used when filling the buffer
    // with as many copies of str as possible, without overflowing it.
    let bufferlen = std::cmp::min(n, BUFSIZE - (BUFSIZE % len));
    for i in 0..bufferlen {
        buffer[i] = str[i % len];
    }
    while n > bufferlen {
        unsafe { maybe_quit() };
        if inherit {
            insert_and_inherit(&buffer[..bufferlen]);
        } else {
            insert_slice(&buffer[..bufferlen]);
        }
        n -= bufferlen;
    }
    if inherit {
        insert_and_inherit(&buffer[..n]);
    } else {
        insert_slice(&buffer[..n]);
    }
}

/// Return the character following point, as a number. At the end of
/// the buffer or accessible region, return 0.
#[lisp_fn]
pub fn following_char() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer_unchecked();

    if buffer_ref.pt >= buffer_ref.zv {
        0
    } else {
        EmacsInt::from(buffer_ref.fetch_char(buffer_ref.pt_byte))
    }
}

/// Return the character preceding point, as a number. At the
/// beginning of the buffer or accessible region, return 0.
#[lisp_fn(c_name = "previous_char")]
pub fn preceding_char() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer_unchecked();

    if buffer_ref.pt <= buffer_ref.begv {
        return EmacsInt::from(0);
    }

    let pos = if buffer_ref.multibyte_characters_enabled() {
        unsafe { dec_pos(buffer_ref.pt_byte) }
    } else {
        buffer_ref.pt_byte - 1
    };
    EmacsInt::from(buffer_ref.fetch_char(pos))
}

/// Return character in current buffer preceding position POS.
/// POS is an integer or a marker and defaults to point.
/// If POS is out of range, the value is nil.
#[lisp_fn(min = "0")]
pub fn char_before(pos: LispObject) -> Option<EmacsInt> {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    let pos_byte: isize;

    if pos.is_nil() {
        pos_byte = buffer_ref.pt_byte;
        // In case point is point_min
        if pos_byte == 1 {
            return None;
        }
    } else if let Some(m) = pos.as_marker() {
        pos_byte = m.bytepos_or_error();
        if pos_byte <= buffer_ref.begv_byte || pos_byte > buffer_ref.zv_byte {
            return None;
        }
    } else {
        let p = pos
            .as_fixnum()
            .unwrap_or_else(|| wrong_type!(Qinteger_or_marker_p, pos)) as isize;
        if p <= buffer_ref.begv || p > buffer_ref.zv {
            return None;
        }
        pos_byte = buffer_ref.charpos_to_bytepos(p);
    }

    let pos_before = if buffer_ref.multibyte_characters_enabled() {
        EmacsInt::from(buffer_ref.fetch_char(unsafe { dec_pos(pos_byte) }))
    } else {
        EmacsInt::from(buffer_ref.fetch_byte(pos_byte - 1))
    };
    Some(pos_before)
}

/// Return character in current buffer at position POS.
/// POS is an integer or a marker and defaults to point.
/// If POS is out of range, the value is nil.
#[lisp_fn(min = "0")]
pub fn char_after(mut pos: LispObject) -> Option<EmacsInt> {
    let buffer_ref = ThreadState::current_buffer_unchecked();
    if pos.is_nil() {
        pos = point().into();
    }
    if let Some(m) = pos.as_marker() {
        let pos_byte = m.bytepos_or_error();
        // Note that this considers the position in the current buffer,
        // even if the marker is from another buffer.
        if pos_byte < buffer_ref.begv_byte || pos_byte >= buffer_ref.zv_byte {
            None
        } else {
            Some(EmacsInt::from(buffer_ref.fetch_char(pos_byte)))
        }
    } else {
        let p = pos.as_fixnum_coerce_marker_or_error() as ptrdiff_t;
        if p < buffer_ref.begv || p >= buffer_ref.zv {
            None
        } else {
            let pos_byte = buffer_ref.charpos_to_bytepos(p);
            Some(EmacsInt::from(buffer_ref.fetch_char(pos_byte)))
        }
    }
}

/// Return a copy of STRING with text properties added.
/// First argument is the string to copy.
/// Remaining arguments form a sequence of PROPERTY VALUE pairs for text
/// properties to add to the result.
/// usage: (fn STRING &rest PROPERTIES)
#[lisp_fn(min = "1")]
pub fn propertize(args: &[LispObject]) -> LispObject {
    // Number of args must be odd.
    if args.len() & 1 == 0 {
        error!("Wrong number of arguments");
    }

    let mut it = args.iter();

    // the unwrap call is safe, the number of args has already been checked
    let first = it.next().unwrap();
    let orig_string = LispStringRef::from(*first);

    let copy = unsafe { Fcopy_sequence(*first) };

    let mut properties = Qnil;

    while let Some(a) = it.next() {
        let b = it.next().unwrap(); // safe due to the odd check at the beginning
        properties = (*a, (*b, properties)).into();
    }

    unsafe {
        Fadd_text_properties(0.into(), orig_string.len_chars().into(), properties, copy);
    };

    copy
}

/// Convert arg CHAR to a string containing that character.
/// usage: (char-to-string CHAR)
#[lisp_fn]
pub fn char_to_string(character: LispObject) -> LispObject {
    let c = character.as_character_or_error();
    let mut buffer = [0_u8; MAX_MULTIBYTE_LENGTH];
    let len = write_codepoint(&mut buffer[..], c);

    unsafe { make_string_from_bytes(buffer.as_ptr() as *const i8, 1, len as isize) }
}

/// Convert arg BYTE to a unibyte string containing that byte.
#[lisp_fn]
pub fn byte_to_string(byte: EmacsInt) -> LispObject {
    if byte < 0 || byte > 255 {
        error!("Invalid byte");
    }
    let byte = byte as i8;

    unsafe { make_string_from_bytes(&byte as *const i8, 1, 1) }
}

/// Return the first character in STRING.
#[lisp_fn]
pub fn string_to_char(string: LispStringRef) -> EmacsInt {
    if !string.is_empty() {
        if string.is_multibyte() {
            let (cp, _) = multibyte_char_at(string.as_slice());
            EmacsInt::from(cp)
        } else {
            EmacsInt::from(string.byte_at(0))
        }
    } else {
        0
    }
}

/// Return the character position of the first character on the current line.
/// With optional argument N, scan forward N - 1 lines first.
/// If the scan reaches the end of the buffer, return that position.
///
/// This function ignores text display directionality; it returns the
/// position of the first character in logical order, i.e. the smallest
/// character position on the line.
///
/// This function constrains the returned position to the current field
/// unless that position would be on a different line than the original,
/// unconstrained result.  If N is nil or 1, and a front-sticky field
/// starts at point, the scan stops as soon as it starts.  To ignore field
/// boundaries, bind `inhibit-field-text-motion' to t.
///
/// This function does not move point.
#[lisp_fn(min = "0")]
pub fn line_beginning_position(n: Option<EmacsInt>) -> EmacsInt {
    let mut charpos: isize = 0;

    let n = n.unwrap_or(1) as isize;

    unsafe { scan_newline_from_point(n - 1, &mut charpos, ptr::null_mut()) };

    // Return END constrained to the current input field.
    constrain_to_field(
        Some(LispNumber::Fixnum(charpos as EmacsInt)),
        LispNumber::Fixnum(point() as EmacsInt),
        n != 1,
        true,
        Qnil,
    )
}

/// Return the character position of the last character on the current line.
/// With argument N not nil or 1, move forward N - 1 lines first.
/// If scan reaches end of buffer, return that position.
///
/// This function ignores text display directionality; it returns the
/// position of the last character in logical order, i.e. the largest
/// character position on the line.
///
/// This function constrains the returned position to the current field
/// unless that would be on a different line than the original,
/// unconstrained result.  If N is nil or 1, and a rear-sticky field ends
/// at point, the scan stops as soon as it starts.  To ignore field
/// boundaries bind `inhibit-field-text-motion' to t.
///
/// This function does not move point.
#[lisp_fn(min = "0")]
pub fn line_end_position(n: Option<EmacsInt>) -> EmacsInt {
    let orig = point();

    let n = n.unwrap_or(1);

    let clipped_n = clip_to_bounds(ptrdiff_t::min_value() + 1, n, ptrdiff_t::max_value());
    let end_pos = unsafe {
        find_before_next_newline(
            orig as isize,
            0,
            clipped_n - (if clipped_n <= 0 { 1 } else { 0 }),
            ptr::null_mut(),
        )
    };

    // Return END constrained to the current input field.
    constrain_to_field(
        Some(LispNumber::Fixnum(end_pos as EmacsInt)),
        LispNumber::Fixnum(orig),
        n != 1,
        true,
        Qnil,
    )
}

/// Return the beginning of the field surrounding POS.
/// A field is a region of text with the same `field' property.
/// If POS is nil, the value of point is used for POS.
/// If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
/// field, then the beginning of the *previous* field is returned.
/// If LIMIT is non-nil, it is a buffer position; if the beginning of the field
/// is before LIMIT, then LIMIT will be returned instead.
#[lisp_fn(min = "0")]
pub fn field_beginning(
    pos: Option<LispNumber>,
    escape_from_edge: bool,
    limit: Option<EmacsInt>,
) -> EmacsInt {
    let (beg, _) = find_field(pos, escape_from_edge, limit, None);

    beg as EmacsInt
}

/// Return the end of the field surrounding POS.
/// A field is a region of text with the same `field' property.
/// If POS is nil, the value of point is used for POS.
/// If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
/// then the end of the *following* field is returned.
/// If LIMIT is non-nil, it is a buffer position; if the end of the field
/// is after LIMIT, then LIMIT will be returned instead.
#[lisp_fn(min = "0")]
pub fn field_end(
    pos: Option<LispNumber>,
    escape_from_edge: bool,
    limit: Option<EmacsInt>,
) -> EmacsInt {
    let (_, end) = find_field(pos, escape_from_edge, None, limit);

    end as EmacsInt
}

/// Return the position closest to NEW-POS that is in the same field as OLD-POS.
/// A field is a region of text with the same `field' property.
///
/// If NEW-POS is nil, then use the current point instead, and move point
/// to the resulting constrained position, in addition to returning that
/// position.
///
/// If OLD-POS is at the boundary of two fields, then the allowable
/// positions for NEW-POS depends on the value of the optional argument
/// ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
/// constrained to the field that has the same `field' char-property
/// as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
/// is non-nil, NEW-POS is constrained to the union of the two adjacent
/// fields.  Additionally, if two fields are separated by another field with
/// the special value `boundary', then any point within this special field is
/// also considered to be `on the boundary'.
///
/// If the optional argument ONLY-IN-LINE is non-nil and constraining
/// NEW-POS would move it to a different line, NEW-POS is returned
/// unconstrained.  This is useful for commands that move by line, like
/// \\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
/// only in the case where they can still move to the right line.
///
/// If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
/// a non-nil property of that name, then any field boundaries are ignored.
///
/// Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil.
#[lisp_fn(min = "2")]
pub fn constrain_to_field(
    new_pos: Option<LispNumber>,
    old_pos: LispNumber,
    escape_from_edge: bool,
    only_in_line: bool,
    inhibit_capture_property: LispObject,
) -> EmacsInt {
    let (mut new_pos, orig_point) = match new_pos {
        None => {
            let tmp = point();
            (tmp, tmp)
        }
        Some(pos) => (pos.to_fixnum(), 0),
    };

    let old_pos = old_pos.to_fixnum();

    let fwd = new_pos > old_pos;

    let prev_old = old_pos - 1;
    let prev_new = new_pos - 1;
    let begv = ThreadState::current_buffer_unchecked().begv as EmacsInt;

    if unsafe { globals.Vinhibit_field_text_motion.is_nil() }
        && new_pos != old_pos
        && (get_char_property(
            new_pos,
            Qfield,
            Qnil).is_not_nil()
            || get_char_property(
                old_pos,
                Qfield,
                Qnil).is_not_nil()
            // To recognize field boundaries, we must also look at the
            // previous positions; we could use `Fget_pos_property'
            // instead, but in itself that would fail inside non-sticky
            // fields (like comint prompts).
            || (new_pos > begv
                && get_char_property(
                    prev_new,
                    Qfield,
                    Qnil).is_not_nil())
            || (old_pos > begv
                && get_char_property(prev_old, Qfield, Qnil).is_not_nil()))
        && (inhibit_capture_property.is_nil()
            // Field boundaries are again a problem; but now we must
            // decide the case exactly, so we need to call
            // `get_pos_property' as well.
            || (unsafe {
                Fget_pos_property(
                    LispObject::from(old_pos),
                    inhibit_capture_property,
                    Qnil).is_nil()
            }
                && (old_pos <= begv
                    || get_char_property(
                        old_pos,
                        inhibit_capture_property,
                        Qnil).is_nil()
                    || get_char_property(
                        prev_old,
                        inhibit_capture_property,
                        Qnil).is_nil())))
    // It is possible that NEW_POS is not within the same field as
    // OLD_POS; try to move NEW_POS so that it is.
    {
        let tmp: LispNumber = old_pos.into();
        let field_bound = if fwd {
            field_end(Some(tmp), escape_from_edge, Some(new_pos))
        } else {
            field_beginning(Some(tmp), escape_from_edge, Some(new_pos))
        };

        let should_constrain = if field_bound < new_pos { fwd } else { !fwd };

        // See if ESCAPE_FROM_EDGE caused FIELD_BOUND to jump to the
        // other side of NEW_POS, which would mean that NEW_POS is
        // already acceptable, and it's not necessary to constrain it
        // to FIELD_BOUND.
        if should_constrain {
            // NEW_POS should be constrained, but only if either
            // ONLY_IN_LINE is nil (in which case any constraint is OK),
            // or NEW_POS and FIELD_BOUND are on the same line (in which
            // case the constraint is OK even if ONLY_IN_LINE is non-nil).
            if !only_in_line {
                // Constrain NEW_POS to FIELD_BOUND.
                new_pos = field_bound;
            } else {
                // This is the ONLY_IN_LINE case, check that NEW_POS and
                // FIELD_BOUND are on the same line by seeing whether
                // there's an intervening newline or not.
                let mut shortage = 0;
                unsafe {
                    find_newline(
                        new_pos as isize,
                        -1,
                        field_bound as isize,
                        -1,
                        if fwd { -1 } else { 1 },
                        &mut shortage,
                        ptr::null_mut(),
                        true,
                    );
                }
                if shortage != 0 {
                    // Constrain NEW_POS to FIELD_BOUND.
                    new_pos = field_bound;
                }
            }
        }

        if orig_point != 0 && new_pos != orig_point {
            // The NEW_POS argument was originally nil, so automatically set PT.
            unsafe {
                set_point(new_pos as isize);
            }
        }
    }

    new_pos
}

/// Return the character position for byte position BYTEPOS.
/// If BYTEPOS is out of range, the value is nil.
#[lisp_fn]
pub fn byte_to_position(bytepos: EmacsInt) -> Option<EmacsInt> {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let mut pos_byte = bytepos as isize;
    if pos_byte < cur_buf.beg_byte() || pos_byte > cur_buf.z_byte() {
        return None;
    }
    if cur_buf.z() != cur_buf.z_byte() {
        // There are multibyte characters in the buffer.
        // The argument of BYTE_TO_CHAR must be a byte position at
        // a character boundary, so search for the start of the current
        // character.
        while !char_head_p(cur_buf.fetch_byte(pos_byte)) {
            pos_byte -= 1;
        }
    }

    Some(cur_buf.bytepos_to_charpos(pos_byte) as EmacsInt)
}

/// Return t if two characters match, optionally ignoring case.
/// Both arguments must be characters (i.e. integers).
/// Case is ignored if `case-fold-search' is non-nil in the current buffer.
#[lisp_fn]
pub fn char_equal(c1: LispObject, c2: LispObject) -> bool {
    // Check they're chars, not just integers, otherwise we could get array
    // bounds violations in downcase.
    let mut c1 = c1.as_character_or_error();
    let mut c2 = c2.as_character_or_error();

    if c1 == c2 {
        return true;
    }

    let cur_buf = ThreadState::current_buffer_unchecked();
    if cur_buf.case_fold_search().is_nil() {
        return false;
    }

    // FIXME: It is possible to compare multibyte characters even when
    // the current buffer is unibyte.  Unfortunately this is ambiguous
    // for characters between 128 and 255, as they could be either
    // eight-bit raw bytes or Latin-1 characters.  Assume the former for
    // now.  See Bug#17011, and also see casefiddle.c's casify_object,
    // which has a similar problem.
    if cur_buf.multibyte_characters_enabled() {
        if is_single_byte_char(c1) {
            c1 = unibyte_to_char(c1);
        }
        if is_single_byte_char(c2) {
            c2 = unibyte_to_char(c2);
        }
    }

    unsafe { downcase(c1 as c_int) == downcase(c2 as c_int) }
}

/// Return the effective uid of Emacs.
/// Value is an integer or a float, depending on the value.
#[lisp_fn]
pub fn user_uid() -> LispObject {
    let id = unsafe { libc::geteuid() };
    LispObject::int_or_float_from_fixnum(EmacsInt::from(id))
}

/// Return the real uid of Emacs.
/// Value is an integer or a float, depending on the value.
#[lisp_fn]
pub fn user_real_uid() -> LispObject {
    let id = unsafe { libc::getuid() };
    LispObject::int_or_float_from_fixnum(EmacsInt::from(id))
}

/// Return the effective gid of Emacs.
/// Value is an integer or a float, depending on the value.
#[lisp_fn]
pub fn group_gid() -> LispObject {
    let id = unsafe { libc::getegid() };
    LispObject::int_or_float_from_fixnum(EmacsInt::from(id))
}

/// Return the real gid of Emacs.
/// Value is an integer or a float, depending on the value.
#[lisp_fn]
pub fn group_real_gid() -> LispObject {
    let id = unsafe { libc::getgid() };
    LispObject::int_or_float_from_fixnum(EmacsInt::from(id))
}

/// Return the process ID of Emacs, as a number.
#[lisp_fn]
pub fn emacs_pid() -> LispObject {
    let id = unsafe { libc::getpid() };
    LispObject::int_or_float_from_fixnum(EmacsInt::from(id))
}

/// Insert before point a substring of the contents of BUFFER.
/// BUFFER may be a buffer or a buffer name.
/// Arguments START and END are character positions specifying the substring.
/// They default to the values of (point-min) and (point-max) in BUFFER.
///
/// Point and before-insertion markers move forward to end up after the
/// inserted text.
/// Any other markers at the point of insertion remain before the text.
///
/// If the current buffer is multibyte and BUFFER is unibyte, or vice
/// versa, strings are converted from unibyte to multibyte or vice versa
/// using `string-make-multibyte' or `string-make-unibyte', which see.
#[lisp_fn(min = "1")]
pub fn insert_buffer_substring(
    buffer: LispBufferOrName,
    beg: Option<LispNumber>,
    end: Option<LispNumber>,
) {
    let mut buf_ref = LispBufferRef::from(buffer)
        .as_live()
        .unwrap_or_else(|| error!("Selecting deleted buffer"));

    let mut b = beg.map_or(buf_ref.begv, |n| n.to_fixnum() as isize);
    let mut e = end.map_or(buf_ref.zv, |n| n.to_fixnum() as isize);

    if b > e {
        std::mem::swap(&mut b, &mut e);
    }

    if !(buf_ref.begv <= b && e <= buf_ref.zv) {
        args_out_of_range!(beg, end);
    }

    let mut cur_buf = ThreadState::current_buffer_unchecked();
    unsafe {
        set_buffer_internal_1(buf_ref.as_mut());
        update_buffer_properties(b, e);
        set_buffer_internal_1(cur_buf.as_mut());
        insert_from_buffer(buf_ref.as_mut(), b, e - b, false)
    };
}

/// Display a message at the bottom of the screen.
/// The message also goes into the `*Messages*' buffer, if `message-log-max'
/// is non-nil.  (In keyboard macros, that's all it does.)
/// Return the message.
///
/// In batch mode, the message is printed to the standard error stream,
/// followed by a newline.
///
/// The first argument is a format control string, and the rest are data
/// to be formatted under control of the string.  Percent sign (%), grave
/// accent (\\=`) and apostrophe (\\=') are special in the format; see
/// `format-message' for details.  To display STRING without special
/// treatment, use (message "%s" STRING).
///
/// If the first argument is nil or the empty string, the function clears
/// any existing message; this lets the minibuffer contents show.  See
/// also `current-message'.
///
/// usage: (message FORMAT-STRING &rest ARGS)
#[lisp_fn(min = "1")]
pub fn message(args: &mut [LispObject]) -> LispObject {
    let format_string = args[0];

    if format_string.is_nil()
        || format_string
            .as_string()
            .map_or(false, |mut s| unsafe { STRING_BYTES(s.as_mut()) } == 0)
    {
        unsafe { message1(ptr::null_mut()) };
        format_string
    } else {
        let val = format_message(args);
        unsafe { message3(val) };
        val
    }
}

/// Display a message, in a dialog box if possible.
/// If a dialog box is not available, use the echo area.
/// The first argument is a format control string, and the rest are data
/// to be formatted under control of the string.  See `format-message' for
/// details.
///
/// If the first argument is nil or the empty string, clear any existing
/// message; let the minibuffer contents show.
///
/// usage: (message-box FORMAT-STRING &rest ARGS)
#[lisp_fn(min = "1")]
pub fn message_box(args: &mut [LispObject]) -> LispObject {
    unsafe {
        if args[0].is_nil() {
            message1("".as_ptr() as *const ::libc::c_char);
            Qnil
        } else {
            let val = format_message(args);
            let pane = list!((build_string("OK".as_ptr() as *const ::libc::c_char), true));
            Fx_popup_dialog(Qt, (val, pane).into(), Qt);
            val
        }
    }
}

/// Display a message in a dialog box or in the echo area.
/// If this command was invoked with the mouse, use a dialog box if
/// `use-dialog-box' is non-nil.
/// Otherwise, use the echo area.
/// The first argument is a format control string, and the rest are data
/// to be formatted under control of the string.  See `format-message' for
/// details.
///
/// If the first argument is nil or the empty string, clear any existing
/// message; let the minibuffer contents show.
///
/// usage: (message-or-box FORMAT-STRING &rest ARGS)
#[lisp_fn(min = "1")]
pub fn message_or_box(args: &mut [LispObject]) -> LispObject {
    if unsafe {
        (globals.last_nonmenu_event.is_nil() || globals.last_nonmenu_event.is_cons())
            && globals.use_dialog_box
    } {
        message_box(args)
    } else {
        message(args)
    }
}

/// Return the string currently displayed in the echo area, or nil if none.
#[lisp_fn(name = "current-message", c_name = "current_message")]
pub fn lisp_current_message() -> LispObject {
    unsafe { current_message() }
}

/// Format a string out of a format-string and arguments.
/// The first argument is a format control string.
/// The other arguments are substituted into it to make the result, a string.
///
/// The format control string may contain %-sequences meaning to substitute
/// the next available argument, or the argument explicitly specified:
///
/// %s means print a string argument.  Actually, prints any object, with `princ'.
/// %d means print as signed number in decimal.
/// %o means print as unsigned number in octal, %x as unsigned number in hex.
/// %X is like %x, but uses upper case.
/// %e means print a number in exponential notation.
/// %f means print a number in decimal-point notation.
/// %g means print a number in exponential notation if the exponent would be
///    less than -4 or greater than or equal to the precision (default: 6);
///    otherwise it prints in decimal-point notation.
/// %c means print a number as a single character.
/// %S means print any object as an s-expression (using `prin1').
///
/// The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
/// Use %% to put a single % into the output.
///
/// A %-sequence other than %% may contain optional field number, flag,
/// width, and precision specifiers, as follows:
///
///   %<field><flags><width><precision>character
///
/// where field is [0-9]+ followed by a literal dollar "$", flags is
/// [+ #-0]+, width is [0-9]+, and precision is a literal period "."
/// followed by [0-9]+.
///
/// If a %-sequence is numbered with a field with positive value N, the
/// Nth argument is substituted instead of the next one.  A format can
/// contain either numbered or unnumbered %-sequences but not both, except
/// that %% can be mixed with numbered %-sequences.
///
/// The + flag character inserts a + before any positive number, while a
/// space inserts a space before any positive number; these flags only
/// affect %d, %e, %f, and %g sequences, and the + flag takes precedence.
/// The - and 0 flags affect the width specifier, as described below.
///
/// The # flag means to use an alternate display form for %o, %x, %X, %e,
/// %f, and %g sequences: for %o, it ensures that the result begins with
/// \"0\"; for %x and %X, it prefixes the result with \"0x\" or \"0X\";
/// for %e and %f, it causes a decimal point to be included even if the
/// precision is zero; for %g, it causes a decimal point to be
/// included even if the precision is zero, and also forces trailing
/// zeros after the decimal point to be left in place.
///
/// The width specifier supplies a lower limit for the length of the
/// printed representation.  The padding, if any, normally goes on the
/// left, but it goes on the right if the - flag is present.  The padding
/// character is normally a space, but it is 0 if the 0 flag is present.
/// The 0 flag is ignored if the - flag is present, or the format sequence
/// is something other than %d, %e, %f, and %g.
///
/// For %e and %f sequences, the number after the "." in the precision
/// specifier says how many decimal places to show; if zero, the decimal
/// point itself is omitted.  For %g, the precision specifies how many
/// significant digits to print; zero or omitted are treated as 1.
/// For %s and %S, the precision specifier truncates the string to the
/// given width.
///
/// Text properties, if any, are copied from the format-string to the
/// produced text.
///
/// usage: (format STRING &rest OBJECTS)
#[lisp_fn(min = "1")]
pub fn format(args: &mut [LispObject]) -> LispObject {
    unsafe {
        styled_format(
            args.len() as isize,
            args.as_mut_ptr() as *mut LispObject,
            false,
        )
    }
}

/// Format a string out of a format-string and arguments.
/// The first argument is a format control string.
/// The other arguments are substituted into it to make the result, a string.
///
/// This acts like `format', except it also replaces each grave accent (\\=`)
/// by a left quote, and each apostrophe (\\=') by a right quote.  The left
/// and right quote replacement characters are specified by
/// `text-quoting-style'.
///
/// usage: (format-message STRING &rest OBJECTS)
#[lisp_fn(min = "1")]
pub fn format_message(args: &mut [LispObject]) -> LispObject {
    unsafe {
        styled_format(
            args.len() as isize,
            args.as_mut_ptr() as *mut LispObject,
            true,
        )
    }
}

/// Return the contents of the current buffer as a string.
/// If narrowing is in effect, this function returns only the visible part
/// of the buffer.
#[lisp_fn]
pub fn buffer_string() -> LispObject {
    let cur_buf = ThreadState::current_buffer_unchecked();

    let begv = cur_buf.begv;
    let begv_byte = cur_buf.begv_byte;

    let zv = cur_buf.zv;
    let zv_byte = cur_buf.zv_byte;

    unsafe { make_buffer_string_both(begv, begv_byte, zv, zv_byte, true) }
}

/// Return the contents of part of the current buffer as a string.
/// The two arguments START and END are character positions;
/// they can be in either order.
/// The string returned is multibyte if the buffer is multibyte.
///
/// This function copies the text properties of that part of the buffer
/// into the result string; if you don't want the text properties,
/// use `buffer-substring-no-properties' instead.
#[lisp_fn]
pub fn buffer_substring(beg: LispObject, end: LispObject) -> LispObject {
    let (beg, end) = validate_region_rust(beg, end);
    unsafe { make_buffer_string(beg, end, true) }
}

/// Return the characters of part of the buffer, without the text properties.
/// The two arguments START and END are character positions;
/// they can be in either order.
#[lisp_fn]
pub fn buffer_substring_no_properties(beg: LispObject, end: LispObject) -> LispObject {
    let (beg, end) = validate_region_rust(beg, end);
    unsafe { make_buffer_string(beg, end, false) }
}

// Save current buffer state for `save-excursion' special form.
// We (ab)use Lisp_Misc_Save_Value to allow explicit free and so
// offload some work from GC.
#[no_mangle]
pub extern "C" fn save_excursion_save() -> LispObject {
    let window: LispWindowRef = selected_window().into();

    unsafe {
        make_save_obj_obj_obj_obj(
            point_marker().into(),
            Qnil,
            // Selected window if current buffer is shown in it, nil otherwise.
            if window.contents.eq(current_buffer()) {
                window.into()
            } else {
                Qnil
            },
            Qnil,
        )
    }
}

/// Save point, and current buffer; execute BODY; restore those things.
/// Executes BODY just like `progn'.
/// The values of point and the current buffer are restored
/// even in case of abnormal exit (throw or error).
///
/// If you only want to save the current buffer but not point,
/// then just use `save-current-buffer', or even `with-current-buffer'.
///
/// Before Emacs 25.1, `save-excursion' used to save the mark state.
/// To save the mark state as well as point and the current buffer, use
/// `save-mark-and-excursion'.
///
/// usage: (save-excursion &rest BODY)
#[lisp_fn(unevalled = "true")]
pub fn save_excursion(args: LispObject) -> LispObject {
    let count = c_specpdl_index();

    unsafe { record_unwind_protect(Some(save_excursion_restore), save_excursion_save()) };

    unbind_to(count, progn(args))
}

/// Record which buffer is current; execute BODY; make that buffer current.
/// BODY is executed just like `progn'.
/// usage: (save-current-buffer &rest BODY)
#[lisp_fn(unevalled = "true")]
pub fn save_current_buffer(args: LispObject) -> LispObject {
    let count = c_specpdl_index();

    unsafe { record_unwind_current_buffer() };

    unbind_to(count, progn(args))
}

/// Execute BODY, saving and restoring current buffer's restrictions.
/// The buffer's restrictions make parts of the beginning and end invisible.
/// \(They are set up with `narrow-to-region' and eliminated with `widen'.)
/// This special form, `save-restriction', saves the current buffer's restrictions
/// when it is entered, and restores them when it is exited.
/// So any `narrow-to-region' within BODY lasts only until the end of the form.
/// The old restrictions settings are restored
/// even in case of abnormal exit (throw or error).
///
/// The value returned is the value of the last form in BODY.
///
/// Note: if you are using both `save-excursion' and `save-restriction',
/// use `save-excursion' outermost:
/// (save-excursion (save-restriction ...))
///
/// usage: (save-restriction &rest BODY)
#[lisp_fn(unevalled = "true")]
pub fn save_restriction(body: LispObject) -> LispObject {
    let count = c_specpdl_index();

    unsafe { record_unwind_protect(Some(save_restriction_restore), save_restriction_save()) };

    unbind_to(count, progn(body))
}

// Find the field surrounding POS in BEG and END.  If POS is nil,
// the value of point is used instead.
//
// BEG_LIMIT and END_LIMIT serve to limit the ranged of the returned
// results; they do not effect boundary behavior.
//
// If MERGE_AT_BOUNDARY is non-nil, then if POS is at the very first
// position of a field, then the beginning of the previous field is
// returned instead of the beginning of POS's field (since the end of a
// field is actually also the beginning of the next input field, this
// behavior is sometimes useful).  Additionally in the MERGE_AT_BOUNDARY
// non-nil case, if two fields are separated by a field with the special
// value `boundary', and POS lies within it, then the two separated
// fields are considered to be adjacent, and POS between them, when
// finding the beginning and ending of the "merged" field.

pub fn find_field(
    pos: Option<LispNumber>,
    merge_at_boundary: bool,
    beg_limit: Option<EmacsInt>,
    end_limit: Option<EmacsInt>,
) -> (ptrdiff_t, ptrdiff_t) {
    let current_buffer = ThreadState::current_buffer_unchecked();
    let pos = pos.map_or(current_buffer.pt, |p| p.to_fixnum() as ptrdiff_t);

    // Fields right before and after the point.
    let after_field =
        unsafe { get_char_property_and_overlay(pos.into(), Qfield, Qnil, ptr::null_mut()) };
    let before_field = if pos > current_buffer.begv {
        unsafe { get_char_property_and_overlay((pos - 1).into(), Qfield, Qnil, ptr::null_mut()) }
    } else {
        // Using nil here would be a more obvious choice, but it would
        // fail when the buffer starts with a non-sticky field.
        after_field
    };

    // True if POS counts as the start of a field.
    let mut at_field_start = false;
    // True if POS counts as the end of a field.
    let mut at_field_end = false;

    // See if we need to handle the case where MERGE_AT_BOUNDARY is nil
    // and POS is at beginning of a field, which can also be interpreted
    // as the end of the previous field.  Note that the case where if
    // MERGE_AT_BOUNDARY is non-nil (see function comment) is actually the
    // more natural one; then we avoid treating the beginning of a field
    // specially.
    if !merge_at_boundary {
        let field = unsafe { Fget_pos_property(pos.into(), Qfield, Qnil) };
        if !field.eq(after_field) {
            at_field_end = true;
        }
        if !field.eq(before_field) {
            at_field_start = true;
        }
        if field.is_nil() && at_field_start && at_field_end {
            // If an inserted char would have a nil field while the surrounding
            // text is non-nil, we're probably not looking at a
            // zero-length field, but instead at a non-nil field that's
            // not intended for editing (such as comint's prompts).
            at_field_start = false;
            at_field_end = false;
        }
    }

    // Note about special `boundary' fields:
    //
    // Consider the case where the point (`.') is between the fields `x' and `y':
    //
    //    xxxx.yyyy
    //
    // In this situation, if merge_at_boundary is non-nil, consider the
    // `x' and `y' fields as forming one big merged field, and so the end
    // of the field is the end of `y'.
    //
    // However, if `x' and `y' are separated by a special `boundary' field
    // (a field with a `field' char-property of 'boundary), then ignore
    // this special field when merging adjacent fields.  Here's the same
    // situation, but with a `boundary' field between the `x' and `y' fields:
    //
    //    xxx.BBBByyyy
    //
    // Here, if point is at the end of `x', the beginning of `y', or
    // anywhere in-between (within the `boundary' field), merge all
    // three fields and consider the beginning as being the beginning of
    // the `x' field, and the end as being the end of the `y' field.

    unsafe {
        let beg = if at_field_start {
            // POS is at the edge of a field, and we should consider it as
            // the beginning of the following field.
            pos
        } else {
            let mut p = pos.into();
            let limit = beg_limit.into();
            // Find the previous field boundary.
            if merge_at_boundary && before_field.eq(Qboundary) {
                // Skip a `boundary' field.
                p = Fprevious_single_char_property_change(p, Qfield, Qnil, limit);
            }
            p = Fprevious_single_char_property_change(p, Qfield, Qnil, limit);
            p.as_fixnum().unwrap_or(current_buffer.begv as EmacsInt) as isize
        };

        let end = if at_field_end {
            // POS is at the edge of a field, and we should consider it as
            // the end of the previous field.
            pos
        } else {
            let mut p = pos.into();
            let limit = end_limit.into();
            // Find the next field boundary.
            if merge_at_boundary && after_field.eq(Qboundary) {
                // Skip a `boundary' field.
                p = Fnext_single_char_property_change(p, Qfield, Qnil, limit);
            }
            p = Fnext_single_char_property_change(p, Qfield, Qnil, limit);
            p.as_fixnum().unwrap_or(current_buffer.zv as EmacsInt) as isize
        };

        (beg, end)
    }
}

/// Delete the field surrounding POS.
/// A field is a region of text with the same `field' property.
/// If POS is nil, the value of point is used for POS.
#[lisp_fn(min = "0")]
pub fn delete_field(pos: Option<LispNumber>) {
    let (beg, end) = find_field(pos, false, None, None);
    if beg != end {
        unsafe { del_range(beg, end) };
    }
}

/// Return the contents of the field surrounding POS as a string.
/// A field is a region of text with the same `field' property.
/// If POS is nil, the value of point is used for POS.
#[lisp_fn(min = "0")]
pub fn field_string(pos: Option<LispNumber>) -> LispObject {
    let (beg, end) = find_field(pos, false, None, None);
    unsafe { make_buffer_string(beg, end, true) }
}

/// Return the contents of the field around POS, without text properties.
/// A field is a region of text with the same `field' property.
/// If POS is nil, the value of point is used for POS.
#[lisp_fn(min = "0")]
pub fn field_string_no_properties(pos: Option<LispNumber>) -> LispObject {
    let (beg, end) = find_field(pos, false, None, None);
    unsafe { make_buffer_string(beg, end, false) }
}

/// Delete the text between START and END, including START but excluding END. If
/// called interactively, delete the region between point and mark. This command
/// deletes buffer text without modifying the kill ring.
#[lisp_fn(intspec = "r")]
pub fn delete_region(start: LispObject, end: LispObject) {
    // Don't just call delete_and_extract_region and throw away the return value
    // to avoid doing the extra, unnecessary work of copying the region to
    // return it.
    let (start, end) = validate_region_rust(start, end);
    unsafe { del_range(start as ptrdiff_t, end as ptrdiff_t) };
}

/// Delete the text between START and END, including START but excluding END, and
/// return it.
#[lisp_fn]
pub fn delete_and_extract_region(start: LispObject, end: LispObject) -> Option<LispStringRef> {
    let (start, end) = validate_region_rust(start, end);
    if start == end {
        Some(LispObject::empty_unibyte_string())
    } else {
        unsafe { del_range_1(start as ptrdiff_t, end as ptrdiff_t, true, true) }.as_string()
    }
}

fn time_arith(
    a: LispObject,
    b: LispObject,
    op: impl FnOnce(LispTime, LispTime) -> LispTime,
) -> Vec<EmacsInt> {
    let mut alen: c_int = 0;
    let mut blen: c_int = 0;
    let ta = unsafe { lisp_time_struct(a, &mut alen) };
    let tb = unsafe { lisp_time_struct(b, &mut blen) };
    let t = op(ta, tb);
    if LispObject::fixnum_overflow(t.hi) {
        time_overflow();
    }

    let maxlen = max(alen, blen) as usize;

    t.into_vec(maxlen)
}

/// Return the sum of two time values A and B, as a time value. A nil value for either argument
/// stands for the current time. See `current-time-string' for the various forms of a time value.
#[lisp_fn(name = "time-add", c_name = "time_add")]
pub fn time_add_lisp(a: LispObject, b: LispObject) -> Vec<EmacsInt> {
    time_arith(a, b, LispTime::add)
}

/// Return the difference between two time values A and B, as a time value. Use `float-time' to
/// convert the difference into elapsed seconds.  A nil value for either argument stands for the
/// current time.  See `current-time-string' for the various forms of a time value.
#[lisp_fn(name = "time-subtract", c_name = "time_subtract")]
pub fn time_subtract_lisp(a: LispObject, b: LispObject) -> Vec<EmacsInt> {
    time_arith(a, b, LispTime::sub)
}

/// Return non-nil if time value T1 is earlier than time value T2.  A nil value for either
/// argument stands for the current time.  See `current-time-string' for the various forms of a
/// time value.
#[lisp_fn]
pub fn time_less_p(t1: LispObject, t2: LispObject) -> bool {
    let mut t1len: c_int = 0;
    let mut t2len: c_int = 0;
    let a = unsafe { lisp_time_struct(t1, &mut t1len) };
    let b = unsafe { lisp_time_struct(t2, &mut t2len) };

    a < b
}

/// Remove restrictions (narrowing) from current buffer.
/// This allows the buffer's full text to be seen and edited.
#[lisp_fn(intspec = "")]
pub fn widen() {
    let mut buffer_ref = ThreadState::current_buffer_unchecked();

    if buffer_ref.beg() != buffer_ref.begv || buffer_ref.z() != buffer_ref.zv {
        buffer_ref.set_clip_changed(true);
    }

    buffer_ref.set_begv_both(buffer_ref.beg(), buffer_ref.beg_byte());
    buffer_ref.set_zv_both(buffer_ref.z(), buffer_ref.z_byte());

    // Changing the buffer bounds invalidates any recorded current column.
    invalidate_current_column();
}

/// Insert the arguments, either strings or characters, at point.
/// Point and after-insertion markers move forward to end up after the inserted text.
/// Any other markers at the point of insertion remain before the text.
///
/// If the current buffer is multibyte, unibyte strings are converted to multibyte for insertion
/// (see `string-make-multibyte').  If the current buffer is unibyte, multibyte strings are
/// converted to unibyte for insertion (see `string-make-unibyte').
///
/// When operating on binary data, it may be necessary to preserve the original bytes of a unibyte
/// string when inserting it into a multibyte buffer; to accomplish this, apply
/// `string-as-multibyte' to the string and insert the result.
///
/// usage: (insert &rest ARGS)
#[lisp_fn(name = "insert", c_name = "insert")]
pub fn insert_lisp(args: &[LispObject]) {
    general_insert_function(insert_slice, insert_from_string_safe, false, args);
}

/// Insert the arguments at point, inheriting properties from adjoining text.  Point and
/// after-insertion markers move forward to end up after the inserted text.  Any other markers at
/// the point of insertion remain before the text.
///
/// If the current buffer is multibyte, unibyte strings are converted to multibyte for insertion
/// (see `unibyte-char-to-multibyte').  If the current buffer is unibyte, multibyte strings are
/// converted to unibyte for insertion.
///
/// usage: (insert-and-inherit &rest ARGS)
#[lisp_fn(name = "insert-and-inherit", c_name = "insert_and_inherit")]
pub fn insert_and_inherit_lisp(args: &[LispObject]) {
    general_insert_function(insert_and_inherit, insert_from_string_safe, true, args);
}

/// Insert strings or characters at point, relocating markers after the text.  Point and markers
/// move forward to end up after the inserted text.
///
/// If the current buffer is multibyte, unibyte strings are converted to multibyte for insertion
/// (see `unibyte-char-to-multibyte').  If the current buffer is unibyte, multibyte strings are
/// converted to unibyte for insertion.
///
/// If an overlay begins at the insertion point, the inserted text falls outside the overlay; if a
/// nonempty overlay ends at the insertion point, the inserted text falls inside that overlay.
///
/// usage: (insert-before-markers &rest ARGS)
#[lisp_fn(name = "insert-before-markers", c_name = "insert_before_markers")]
pub fn insert_before_markers_lisp(args: &[LispObject]) {
    general_insert_function(
        insert_before_markers,
        insert_from_string_before_markers_safe,
        false,
        args,
    );
}

/// Insert text at point, relocating markers and inheriting properties.  Point and markers move
/// forward to end up after the inserted text.
///
/// If the current buffer is multibyte, unibyte strings are converted to multibyte for insertion
/// (see `unibyte-char-to-multibyte').  If the current buffer is unibyte, multibyte strings are
/// converted to unibyte for insertion.
///
/// usage: (insert-before-markers-and-inherit &rest ARGS)
#[lisp_fn(
    name = "insert-before-markers-and-inherit",
    c_name = "insert_and_inherit_before_markers"
)]
pub fn insert_and_inherit_before_markers_lisp(args: &[LispObject]) {
    general_insert_function(
        insert_before_markers_and_inherit,
        insert_from_string_before_markers_safe,
        true,
        args,
    );
}

/// Insert the part of the text of STRING, a Lisp object assumed to be of type string, consisting
/// of the LENGTH characters (LENGTH_BYTE bytes) starting at position POS / POS_BYTE.  If the text
/// of STRING has properties,
///copy them into the buffer.
///
/// It does not work to use `insert' for this, because a GC could happen before we copy the stuff
/// into the buffer, and relocate the string without insert noticing.
#[no_mangle]
pub unsafe extern "C" fn insert_from_string(
    string: LispObject,
    pos: ptrdiff_t,
    pos_byte: ptrdiff_t,
    length: ptrdiff_t,
    length_byte: ptrdiff_t,
    inherit: bool,
) {
    let current_buffer = ThreadState::current_buffer_unchecked();
    let opoint = current_buffer.pt;

    let s: LispStringRef = string.into();
    if s.len_chars() == 0 {
        return;
    }

    insert_from_string_1(string, pos, pos_byte, length, length_byte, inherit, false);
    signal_after_change(opoint, 0, current_buffer.pt - opoint);
    update_compositions(opoint, current_buffer.pt, CHECK_BORDER as i32);
}

/// A safe-marked version of insert_from_string
fn insert_from_string_safe(
    string: LispObject,
    pos: ptrdiff_t,
    pos_byte: ptrdiff_t,
    length: ptrdiff_t,
    length_byte: ptrdiff_t,
    inherit: bool,
) {
    unsafe { insert_from_string(string, pos, pos_byte, length, length_byte, inherit) };
}

/// Like `insert_from_string' except that all markers pointing at the place where the insertion
/// happens are adjusted to point after it.
#[no_mangle]
pub unsafe extern "C" fn insert_from_string_before_markers(
    string: LispObject,
    pos: ptrdiff_t,
    pos_byte: ptrdiff_t,
    length: ptrdiff_t,
    length_byte: ptrdiff_t,
    inherit: bool,
) {
    let current_buffer = ThreadState::current_buffer_unchecked();
    let opoint = current_buffer.pt;

    let s: LispStringRef = string.into();
    if s.len_chars() == 0 {
        return;
    }

    insert_from_string_1(string, pos, pos_byte, length, length_byte, inherit, true);
    signal_after_change(opoint, 0, current_buffer.pt - opoint);
    update_compositions(opoint, current_buffer.pt, CHECK_BORDER as i32);
}

/// A safe-marked version of insert_from_string_before_markers
fn insert_from_string_before_markers_safe(
    string: LispObject,
    pos: ptrdiff_t,
    pos_byte: ptrdiff_t,
    length: ptrdiff_t,
    length_byte: ptrdiff_t,
    inherit: bool,
) {
    unsafe {
        insert_from_string_before_markers(string, pos, pos_byte, length, length_byte, inherit)
    };
}

/// Insert NARGS Lisp objects in the array ARGS by calling INSERT_FUNC (if a type of object is
/// Lisp_Int) or INSERT_FROM_STRING_FUNC (if a type of object is Lisp_String).  INHERIT is passed
/// to INSERT_FROM_STRING_FUNC as the last argument
fn general_insert_function<IF, IFSF>(
    insert_func: IF,
    insert_from_string_func: IFSF,
    inherit: bool,
    args: &[LispObject],
) where
    IF: Fn(&[u8]),
    IFSF: Fn(LispObject, ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t, bool),
{
    for &val in args {
        if val.is_character() {
            let c = val.as_natnum_or_error() as Codepoint;
            let mut s = [0 as c_uchar; MAX_MULTIBYTE_LENGTH];
            let multibyte = ThreadState::current_buffer_unchecked().multibyte_characters_enabled();
            let len = if multibyte {
                write_codepoint(&mut s, c)
            } else {
                s[0] = raw_byte_from_codepoint(c);
                1
            };
            insert_func(&s[..len]);
        } else if let Some(string) = val.as_string() {
            insert_from_string_func(val, 0, 0, string.len_chars(), string.len_bytes(), inherit);
        } else {
            wrong_type!(Qchar_or_string_p, val);
        }
    }
}

/// Insert a string of specified length before point.  This function judges multibyteness based on
/// enable_multibyte_characters in the current buffer; it never converts between single-byte and
/// multibyte.
///
/// DO NOT use this for the contents of a Lisp string or a Lisp buffer!  prepare_to_modify_buffer
/// could relocate the text.
#[no_mangle]
pub unsafe extern "C" fn insert(string: *const c_char, nbytes: ptrdiff_t) {
    if nbytes > 0 {
        let len = chars_in_text(string as *const c_uchar, nbytes);
        insert_1_both(string, len, nbytes, false, true, false);

        let pt = point() as isize;
        let opoint = pt - len;
        signal_after_change(opoint, 0, len);
        update_compositions(opoint, pt, CHECK_BORDER as i32);
    }
}

/// A version of inset that takes a slice instead of pointer and length
pub fn insert_slice(string: &[u8]) {
    unsafe { insert(string.as_ptr() as *const c_char, string.len() as isize) }
}

/// Likewise, but inherit text properties from neighboring characters.
pub fn insert_and_inherit(string: &[u8]) {
    let nbytes = string.len() as isize;
    if nbytes > 0 {
        unsafe {
            let len = chars_in_text(string.as_ptr(), nbytes);
            insert_1_both(
                string.as_ptr() as *const c_char,
                len,
                nbytes,
                true,
                true,
                false,
            );

            let pt = point() as isize;
            let opoint = pt - len;
            signal_after_change(opoint, 0, len);
            update_compositions(opoint, pt, CHECK_BORDER as i32);
        }
    }
}

/// Like `insert' except that all markers pointing at the place where the insertion happens are
/// adjusted to point after it.  Don't use this function to insert part of a Lisp string, since gc
/// could happen and relocate it.
pub fn insert_before_markers(string: &[u8]) {
    let nbytes = string.len() as isize;
    if nbytes > 0 {
        unsafe {
            let len = chars_in_text(string.as_ptr(), nbytes);
            insert_1_both(
                string.as_ptr() as *const c_char,
                len,
                nbytes,
                false,
                true,
                true,
            );

            let pt = point() as isize;
            let opoint = pt - len;
            signal_after_change(opoint, 0, len);
            update_compositions(opoint, pt, CHECK_BORDER as i32);
        }
    }
}

/// Likewise, but inherit text properties from neighboring characters.
pub fn insert_before_markers_and_inherit(string: &[u8]) {
    let nbytes = string.len() as isize;
    if nbytes > 0 {
        unsafe {
            let len = chars_in_text(string.as_ptr(), nbytes);
            insert_1_both(
                string.as_ptr() as *const c_char,
                len,
                nbytes,
                true,
                true,
                true,
            );

            let pt = point() as isize;
            let opoint = pt - len;
            signal_after_change(opoint, 0, len);
            update_compositions(opoint, pt, CHECK_BORDER as i32);
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/editfns_exports.rs"));
