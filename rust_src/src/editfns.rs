//! Lisp functions pertaining to editing.

use std::ptr;

use libc::{c_uchar, ptrdiff_t};
use std;

use remacs_macros::lisp_fn;
use remacs_sys::{Fadd_text_properties, Fcons, Fcopy_sequence, Fget_pos_property};
use remacs_sys::{Qfield, Qinteger_or_marker_p, Qmark_inactive, Qnil};
use remacs_sys::{buf_charpos_to_bytepos, buffer_overflow, find_before_next_newline, find_field,
                 find_newline, globals, insert, insert_and_inherit, make_string_from_bytes,
                 maybe_quit, scan_newline_from_point, set_point, set_point_both};
use remacs_sys::EmacsInt;

use buffers::{get_buffer, BUF_BYTES_MAX};
use lisp::{LispNumber, LispObject};
use lisp::defsubr;
use marker::{marker_position, set_point_from_marker};
use multibyte::{multibyte_char_at, raw_byte_codepoint, write_codepoint, Codepoint, LispStringRef,
                MAX_MULTIBYTE_LENGTH};
use textprop::get_char_property;
use threads::ThreadState;
use util::clip_to_bounds;

/// Return value of point, as an integer.
/// Beginning of buffer is position (point-min).
#[lisp_fn]
pub fn point() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer();
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
pub fn buffer_size(buffer: LispObject) -> EmacsInt {
    let buffer_ref = if buffer.is_not_nil() {
        get_buffer(buffer).as_buffer_or_error()
    } else {
        ThreadState::current_buffer()
    };
    (buffer_ref.z() - buffer_ref.beg()) as EmacsInt
}

/// Return t if point is at the end of the buffer.
/// If the buffer is narrowed, this means the end of the narrowed part.
#[lisp_fn]
pub fn eobp() -> bool {
    let buffer_ref = ThreadState::current_buffer();
    buffer_ref.zv() == buffer_ref.pt
}

/// Return t if point is at the beginning of the buffer.  If the
/// buffer is narrowed, this means the beginning of the narrowed part.
#[lisp_fn]
pub fn bobp() -> bool {
    let buffer_ref = ThreadState::current_buffer();
    buffer_ref.pt == buffer_ref.begv
}

/// Return t if point is at the beginning of a line.
#[lisp_fn]
pub fn bolp() -> bool {
    let buffer_ref = ThreadState::current_buffer();
    buffer_ref.pt == buffer_ref.begv || buffer_ref.fetch_byte(buffer_ref.pt_byte - 1) == b'\n'
}

/// Return t if point is at the end of a line.
/// `End of a line' includes point being at the end of the buffer.
#[lisp_fn]
pub fn eolp() -> bool {
    let buffer_ref = ThreadState::current_buffer();
    buffer_ref.pt == buffer_ref.zv() || buffer_ref.fetch_byte(buffer_ref.pt_byte) == b'\n'
}

/// Return the position of the gap, in the current buffer.
/// See also `gap-size'.
#[lisp_fn]
pub fn gap_position() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer();
    buffer_ref.gap_position() as EmacsInt
}

/// Return the size of the current buffer's gap.
/// See also `gap-position'.
#[lisp_fn]
pub fn gap_size() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer();
    buffer_ref.gap_size() as EmacsInt
}

/// Return the start or end position of the region.
/// BEGINNINGP means return the start.
/// If there is no region active, signal an error.
fn region_limit(beginningp: bool) -> EmacsInt {
    let current_buf = ThreadState::current_buffer();
    if LispObject::from_raw(unsafe { globals.f_Vtransient_mark_mode }).is_not_nil()
        && LispObject::from_raw(unsafe { globals.f_Vmark_even_if_inactive }).is_nil()
        && current_buf.mark_active().is_nil()
    {
        xsignal!(Qmark_inactive);
    }

    let m = marker_position(current_buf.mark().into());
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
    ThreadState::current_buffer().mark()
}

/// Return the minimum permissible value of point in the current
/// buffer.  This is 1, unless narrowing (a buffer restriction) is in
/// effect.
#[lisp_fn]
pub fn point_min() -> EmacsInt {
    ThreadState::current_buffer().begv as EmacsInt
}

/// Return the maximum permissible value of point in the current
/// buffer.  This is (1+ (buffer-size)), unless narrowing (a buffer
/// restriction) is in effect, in which case it is less.
#[lisp_fn]
pub fn point_max() -> EmacsInt {
    ThreadState::current_buffer().zv() as EmacsInt
}

/// Set point to POSITION, a number or marker.
/// Beginning of buffer is position (point-min), end is (point-max).
///
/// The return value is POSITION.
#[lisp_fn(intspec = "NGoto char: ")]
pub fn goto_char(position: LispObject) -> LispObject {
    if let Some(marker) = position.as_marker() {
        set_point_from_marker(marker);
    } else if let Some(num) = position.as_fixnum() {
        let cur_buf = ThreadState::current_buffer();
        let pos = clip_to_bounds(cur_buf.begv, num, cur_buf.zv);
        let bytepos = unsafe { buf_charpos_to_bytepos(cur_buf.as_ptr(), pos) };
        unsafe { set_point_both(pos, bytepos) };
    } else {
        wrong_type!(Qinteger_or_marker_p, position)
    };
    position
}

/// Return the byte position for character position POSITION.
/// If POSITION is out of range, the value is nil.
#[lisp_fn]
pub fn position_bytes(position: LispObject) -> Option<EmacsInt> {
    let pos = position.as_fixnum_coerce_marker_or_error() as ptrdiff_t;
    let cur_buf = ThreadState::current_buffer();

    if pos >= cur_buf.begv && pos <= cur_buf.zv {
        let bytepos = unsafe { buf_charpos_to_bytepos(cur_buf.as_ptr(), pos) };
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
        args_out_of_range!(
            LispObject::from_fixnum(byte),
            LispObject::from_fixnum(0),
            LispObject::from_fixnum(255)
        )
    }
    let buf = ThreadState::current_buffer();
    let toinsert =
        if byte >= 128 && LispObject::from_raw(buf.enable_multibyte_characters).is_not_nil() {
            raw_byte_codepoint(byte as c_uchar) as EmacsInt
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
#[lisp_fn(min = "1", intspec = "(list (read-char-by-name \"Insert character (Unicode name or hex): \") (prefix-numeric-value current-prefix-arg) t))")]
pub fn insert_char(character: Codepoint, count: Option<EmacsInt>, inherit: bool) {
    let count = count.unwrap_or(1);
    if count <= 0 {
        return;
    }

    let mut str = [0_u8; MAX_MULTIBYTE_LENGTH];
    let len = write_codepoint(&mut str[..], character);

    if BUF_BYTES_MAX / (len as isize) < (count as isize) {
        unsafe { buffer_overflow() };
    }
    let mut n: ptrdiff_t = (count * (len as EmacsInt)) as ptrdiff_t;
    // 4000 bytes is a magic number chosen deep in the past for unknown reasons
    const BUFSIZE: usize = 4000;
    let mut buffer = [0_i8; BUFSIZE];
    // bufferlen is the number of bytes used when filling the buffer
    // with as many copies of str as possible, without overflowing it.
    let bufferlen: ptrdiff_t = std::cmp::min(n, (BUFSIZE - (BUFSIZE % len)) as isize);
    for i in 0..bufferlen {
        buffer[i as usize] = str[(i % len as isize) as usize] as i8;
    }
    while n > bufferlen {
        unsafe { maybe_quit() };
        if inherit {
            unsafe { insert_and_inherit(buffer.as_ptr(), bufferlen) };
        } else {
            unsafe { insert(buffer.as_ptr(), bufferlen) };
        }
        n -= bufferlen;
    }
    if inherit {
        unsafe { insert_and_inherit(buffer.as_ptr(), n) };
    } else {
        unsafe { insert(buffer.as_ptr(), n) };
    }
}

/// Return the character following point, as a number. At the end of
/// the buffer or accessible region, return 0.
#[lisp_fn]
pub fn following_char() -> EmacsInt {
    let buffer_ref = ThreadState::current_buffer();

    if buffer_ref.pt >= buffer_ref.zv {
        0
    } else {
        buffer_ref.fetch_char(buffer_ref.pt_byte) as EmacsInt
    }
}

/// Return character in current buffer at position POS.
/// POS is an integer or a marker and defaults to point.
/// If POS is out of range, the value is nil.
#[lisp_fn(min = "0")]
pub fn char_after(mut pos: LispObject) -> Option<EmacsInt> {
    let buffer_ref = ThreadState::current_buffer();
    if pos.is_nil() {
        pos = LispObject::from(point());
    }
    if pos.is_marker() {
        let pos_byte = pos.as_marker().unwrap().bytepos_or_error();
        // Note that this considers the position in the current buffer,
        // even if the marker is from another buffer.
        if pos_byte < buffer_ref.begv_byte || pos_byte >= buffer_ref.zv_byte {
            None
        } else {
            Some(buffer_ref.fetch_char(pos_byte) as EmacsInt)
        }
    } else {
        let p = pos.as_fixnum_coerce_marker_or_error() as ptrdiff_t;
        if p < buffer_ref.begv || p >= buffer_ref.zv() {
            None
        } else {
            let pos_byte = unsafe { buf_charpos_to_bytepos(buffer_ref.as_ptr(), p) };
            Some(buffer_ref.fetch_char(pos_byte) as EmacsInt)
        }
    }
}

/// Return a copy of STRING with text properties added.
/// First argument is the string to copy.
/// Remaining arguments form a sequence of PROPERTY VALUE pairs for text
/// properties to add to the result.
/// usage: (fn STRING &rest PROPERTIES)
#[lisp_fn(min = "1")]
pub fn propertize(args: &mut [LispObject]) -> LispObject {
    /* Number of args must be odd. */
    if args.len() & 1 == 0 {
        error!("Wrong number of arguments");
    }

    let mut it = args.iter();

    // the unwrap call is safe, the number of args has already been checked
    let first = it.next().unwrap();
    let orig_string = first.as_string_or_error();

    let copy = LispObject::from_raw(unsafe { Fcopy_sequence(first.to_raw()) });

    // this is a C style Lisp_Object because that is what Fcons expects and returns.
    // Once Fcons is ported to Rust this can be migrated to a LispObject.
    let mut properties = Qnil;

    while let Some(a) = it.next() {
        let b = it.next().unwrap(); // safe due to the odd check at the beginning
        properties = unsafe { Fcons(a.to_raw(), Fcons(b.to_raw(), properties)) };
    }

    unsafe {
        Fadd_text_properties(
            LispObject::from_natnum(0).to_raw(),
            LispObject::from_natnum(orig_string.len_chars() as EmacsInt).to_raw(),
            properties,
            copy.to_raw(),
        );
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

    LispObject::from_raw(unsafe {
        make_string_from_bytes(buffer.as_ptr() as *const i8, 1, len as isize)
    })
}

/// Convert arg BYTE to a unibyte string containing that byte.
#[lisp_fn]
pub fn byte_to_string(byte: LispObject) -> LispObject {
    let b = byte.as_fixnum_or_error();
    if b < 0 || b > 255 {
        error!("Invalid byte");
    }
    let b = b as i8;

    LispObject::from_raw(unsafe { make_string_from_bytes(&b as *const i8, 1, 1) })
}

/// Return the first character in STRING.
#[lisp_fn]
pub fn string_to_char(string: LispStringRef) -> EmacsInt {
    if string.len_chars() > 0 {
        if string.is_multibyte() {
            let (cp, _) = multibyte_char_at(string.as_slice());
            cp as EmacsInt
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
        LispObject::constant_nil(),
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
        LispObject::constant_nil(),
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
    pos: Option<EmacsInt>,
    escape_from_edge: bool,
    limit: Option<EmacsInt>,
) -> EmacsInt {
    let mut beg = 0;
    unsafe {
        find_field(
            LispObject::from(pos).to_raw(),
            LispObject::from(escape_from_edge).to_raw(),
            LispObject::from(limit).to_raw(),
            &mut beg,
            Qnil,
            ptr::null_mut(),
        );
    }

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
    pos: Option<EmacsInt>,
    escape_from_edge: bool,
    limit: Option<EmacsInt>,
) -> EmacsInt {
    let mut end = 0;
    unsafe {
        find_field(
            LispObject::from(pos).to_raw(),
            LispObject::from(escape_from_edge).to_raw(),
            Qnil,
            ptr::null_mut(),
            LispObject::from(limit).to_raw(),
            &mut end,
        );
    }

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
    let begv = ThreadState::current_buffer().begv as EmacsInt;

    if unsafe { globals.f_Vinhibit_field_text_motion == Qnil } && new_pos != old_pos
        && (get_char_property(
            new_pos,
            LispObject::from_raw(Qfield),
            LispObject::constant_nil()).is_not_nil()
            || get_char_property(
                old_pos,
                LispObject::from_raw(Qfield),
                LispObject::constant_nil()).is_not_nil()
            // To recognize field boundaries, we must also look at the
            // previous positions; we could use `Fget_pos_property'
            // instead, but in itself that would fail inside non-sticky
            // fields (like comint prompts).
            || (new_pos > begv
                && get_char_property(
                    prev_new,
                    LispObject::from_raw(Qfield),
                    LispObject::constant_nil()).is_not_nil())
            || (old_pos > begv
                && get_char_property(
                    prev_old,
                    LispObject::from_raw(Qfield),
                    LispObject::constant_nil(),
                ).is_not_nil()))
        && (inhibit_capture_property.is_nil()
            // Field boundaries are again a problem; but now we must
            // decide the case exactly, so we need to call
            // `get_pos_property' as well.
            || (unsafe {
                Fget_pos_property(
                    LispObject::from(old_pos).to_raw(),
                    inhibit_capture_property.to_raw(),
                    Qnil) == Qnil
            }
                && (old_pos <= begv
                    || get_char_property(
                        old_pos,
                        inhibit_capture_property,
                        LispObject::constant_nil()).is_nil()
                    || get_char_property(
                        prev_old,
                        inhibit_capture_property,
                        LispObject::constant_nil()).is_nil())))
    // It is possible that NEW_POS is not within the same field as
    // OLD_POS; try to move NEW_POS so that it is.
    {
        let field_bound = if fwd {
            field_end(Some(old_pos), escape_from_edge, Some(new_pos))
        } else {
            field_beginning(Some(old_pos), escape_from_edge, Some(new_pos))
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

include!(concat!(env!("OUT_DIR"), "/editfns_exports.rs"));
