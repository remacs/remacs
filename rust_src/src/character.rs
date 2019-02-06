//! Operations on characters.

use libc::{c_uchar, ptrdiff_t};

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    multibyte::{make_char_multibyte, raw_byte_from_codepoint_safe},
    multibyte::{Codepoint, MAX_CHAR},
    remacs_sys::EmacsInt,
    remacs_sys::Qcharacterp,
    threads::ThreadState,
};

impl LispObject {
    /// Nonzero iff X is a character.
    pub fn is_character(self) -> bool {
        self.as_fixnum()
            .map_or(false, |i| 0 <= i && i <= EmacsInt::from(MAX_CHAR))
    }

    /// Check if Lisp object is a character or not and return the codepoint
    /// Similar to CHECK_CHARACTER
    pub fn as_character_or_error(self) -> Codepoint {
        if !self.is_character() {
            wrong_type!(Qcharacterp, self)
        }
        self.as_fixnum().unwrap() as Codepoint
    }
}

/// True iff byte starts a character in a multibyte form.
///
/// Same as the `CHAR_HEAD_P` macro.
pub fn char_head_p(byte: c_uchar) -> bool {
    (byte & 0xC0) != 0x80
}

/// Decrement the buffer byte position `POS_BYTE` of the current buffer
/// to the previous character boundary. No range checking of POS.
///
/// Can be used instead of the `DEC_POS` macro.
pub unsafe fn dec_pos(pos_byte: ptrdiff_t) -> ptrdiff_t {
    let buffer_ref = ThreadState::current_buffer_unchecked();

    let mut new_pos = pos_byte - 1;
    let mut offset = new_pos - buffer_ref.beg_byte();
    if new_pos >= buffer_ref.gpt_byte() {
        offset += buffer_ref.gap_size();
    }
    let mut chp = buffer_ref.beg_addr().offset(offset);

    while !char_head_p(*chp) {
        chp = chp.offset(-1);
        new_pos -= 1;
    }

    new_pos
}

/// Return the character of the maximum code.
#[lisp_fn]
pub fn max_char() -> LispObject {
    MAX_CHAR.into()
}

/// Return non-nil if OBJECT is a character.
/// In Emacs Lisp, characters are represented by character codes, which
/// are non-negative integers.  The function `max-char' returns the
/// maximum character code.
/// usage: (fn OBJECT)
#[lisp_fn(min = "1")]
pub fn characterp(object: LispObject, _ignore: LispObject) -> bool {
    object.is_character()
}

/// Return t if OBJECT is a character or a string.
#[lisp_fn]
pub fn char_or_string_p(object: LispObject) -> bool {
    object.is_character() || object.is_string()
}

/// Convert the byte CH to multibyte character.
#[lisp_fn]
pub fn unibyte_char_to_multibyte(ch: LispObject) -> LispObject {
    let c = ch.as_character_or_error();
    if c >= 0x100 {
        error!("Not a unibyte character: {}", c);
    }
    make_char_multibyte(c).into()
}

/// Convert the multibyte character CH to a byte.
/// If the multibyte character does not represent a byte, return -1.
#[lisp_fn]
pub fn multibyte_char_to_unibyte(ch: LispObject) -> LispObject {
    let c = ch.as_character_or_error();
    if c < 256 {
        // Can't distinguish a byte read from a unibyte buffer from
        // a latin1 char, so let's let it slide.
        ch
    } else {
        raw_byte_from_codepoint_safe(c).into()
    }
}

include!(concat!(env!("OUT_DIR"), "/character_exports.rs"));
