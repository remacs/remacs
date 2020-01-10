//! Beginnings of a Emacs-encoded string handling library.
//!
//! Emacs Lisp strings (and by extension, most strings handled by the
//! Emacs C API) are encoded in one of two ways:
//!
//! * "unibyte" strings are just sequences of 8-bit bytes that don't
//!   carry encoding information.  Their interpretation is governed
//!   by runtime settings (`set-language-environment').
//!
//! * "multibyte" strings are sequences of characters from an extended
//!   set of character codes, encoded in a fashion similar to UTF-8.
//!
//! The uniqueness of the Multibyte encoding is due to these features:
//!
//! * Codepoints up to 0x10FFFF coincide with Unicode.  However, the
//!   maximum codepoint is 0x3FFFFF.  The additional codepoints are
//!   used for "characters not unified with Unicode" and for 8-bit
//!   bytes, see below.
//!
//! * "Raw 8-bit" bytes, e.g. used when opening a file which is not
//!   properly encoded in a single encoding, are supported.
//!
//!   Raw 8-bit bytes are represented by codepoints 0x3FFF80 to
//!   0x3FFFFF.  However, in the UTF-8 like encoding, where they
//!   should be represented by a 5-byte sequence starting with 0xF8,
//!   they are instead represented by a 2-byte sequence starting with
//!   0xC0 or 0xC1.  These 2-byte sequences are disallowed in UTF-8,
//!   because they would form a duplicate encoding for the the 1-byte
//!   ASCII range.
//!
//! Due to these specialties, we cannot treat Emacs strings as Rust
//! `&str`, and this module regrettably contains adapted copies of
//! stretches of `std::str` functions.

use std::convert::TryFrom;
use std::fmt;
use std::ptr;
use std::slice;

use libc::{c_char, c_int, c_uchar, c_uint, c_void, memset, ptrdiff_t, size_t};

use crate::{
    hashtable::LispHashTableRef,
    lisp::{ExternalPtr, LispObject, LispStructuralEqual},
    obarray::LispObarrayRef,
    remacs_sys::{
        buffer_display_table, char_width, compare_string_intervals, empty_unibyte_string,
        find_composition as c_find_composition, get_composition_id, string_char_to_byte,
    },
    remacs_sys::{
        char_bits, composition_table, equal_kind, EmacsDouble, EmacsInt, Lisp_Interval,
        Lisp_String, Lisp_Type,
    },
    remacs_sys::{Qcharacterp, Qnil, Qstringp},
    symbols::LispSymbolRef,
};

pub type LispStringRef = ExternalPtr<Lisp_String>;

/// Maximum character code
pub const MAX_CHAR: u32 = (1 << char_bits::CHARACTERBITS as usize) - 1;

// Maximum character codes for several encoded lengths
/// Maximum value for a single byte codepoint
pub const MAX_1_BYTE_CHAR: u32 = 0x7F;

/// Minimum value for a two byte codepoint
pub const MIN_2_BYTE_CHAR: u32 = 0x80;
/// Maximum value for a two byte codepoint
pub const MAX_2_BYTE_CHAR: u32 = 0x7FF;

/// Minimum value for a three byte codepoint
pub const MIN_3_BYTE_CHAR: u32 = 0x800;
/// Maximum value for a three byte codepoint
pub const MAX_3_BYTE_CHAR: u32 = 0xFFFF;

/// Minimum value for a four byte codepoint
pub const MIN_4_BYTE_CHAR: u32 = 0x10000;
/// Maximum value for a four byte codepoint
pub const MAX_4_BYTE_CHAR: u32 = 0x1F_FFFF;

/// Minimum value for a five byte codepoint
pub const MIN_5_BYTE_CHAR: u32 = 0x20_0000;
/// Maximum value for a five byte codepoint
pub const MAX_5_BYTE_CHAR: u32 = 0x3F_FF7F;

pub const MIN_BYTE8_CHAR: u32 = 0x3F_FF80;
pub const MAX_BYTE8_CHAR: u32 = 0x3F_FFFF;

/// Maximum length of a single encoded codepoint
pub const MAX_MULTIBYTE_LENGTH: usize = 5;

const BYTE8_OFFSET: u32 = 0x3F_FF00;

// cannot use `char`, it takes values out of its range
#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Codepoint(u32);

impl Codepoint {
    // Equivalent to BYTE8_TO_CHAR
    /// Create a codepoint from a raw byte. If non-ascii, byte8 encode it.
    pub fn from_raw(byte: u8) -> Self {
        match Self::from(byte) {
            cp if cp.is_ascii() => cp,
            cp => Self::from(cp.0 + BYTE8_OFFSET),
        }
    }

    // Equivalent to ASCII_CHAR_P
    /// Whether the character is an ASCII value
    pub const fn is_ascii(self) -> bool {
        self.0 < MIN_2_BYTE_CHAR
    }

    // Equivalent to SINGLE_BYTE_CHAR_P
    /// Whether the character is a single byte value (i.e. less than 256/0x100)
    pub const fn is_single_byte(self) -> bool {
        self.0 < 0x100
    }

    // Equivalent to CHAR_BYTE8_P
    /// Whether the character is a byte8 value, i.e. an encoded raw 8bit byte.
    pub const fn is_byte8(self) -> bool {
        self.0 >= MIN_BYTE8_CHAR
    }

    /// The amount of bytes needed to represent the codepoint's multibyte form.
    pub fn len_bytes(self) -> usize {
        match self.0 {
            0..=MAX_1_BYTE_CHAR => 1,
            MIN_2_BYTE_CHAR..=MAX_2_BYTE_CHAR => 2,
            MIN_3_BYTE_CHAR..=MAX_3_BYTE_CHAR => 3,
            MIN_4_BYTE_CHAR..=MAX_4_BYTE_CHAR => 4,
            MIN_5_BYTE_CHAR..=MAX_5_BYTE_CHAR => 5,
            MIN_BYTE8_CHAR..=MAX_BYTE8_CHAR => 2,
            _ => invalid_character(self),
        }
    }

    /// Get the integer value of the codepoint
    pub const fn val(self) -> u32 {
        self.0
    }

    // Equivalent to CHAR_TO_BYTE8
    /// Extract the encoded byte8 or raw byte value.
    ///
    /// Note that this does not check if the codepoint is within the
    /// appropriate range.
    pub fn to_byte8_unchecked(self) -> u8 {
        if self.is_byte8() {
            (self.0 - BYTE8_OFFSET) as u8
        } else {
            (self.0 & 0xFF) as u8
        }
    }

    // Equivalent to CHAR_TO_BYTE_SAFE
    /// Return the raw 8-bit byte for the character, or None if it doesn't
    /// correnspond to a byte.
    pub fn to_byte8(self) -> Option<u8> {
        if self.is_ascii() {
            Some(self.0 as u8)
        } else if self.is_byte8() {
            Some((self.0 - BYTE8_OFFSET) as u8)
        } else {
            None
        }
    }

    // Equivalent to UNIBYTE_TO_CHAR
    pub fn unibyte_to_char(self) -> Self {
        if self.is_ascii() {
            self
        } else {
            Self::from_raw(self.0 as u8)
        }
    }

    // Equivalent to MAKE_CHAR_MULTIBYTE
    /// Transform an 8-bit codepoint to its byte8 encoded form.
    pub fn to_multibyte(self) -> Self {
        debug_assert!(self.is_single_byte());
        self.unibyte_to_char()
    }

    // Equivalent to CHAR_STRING
    /// Write the codepoint to the given slice. The slice needs to be big
    /// enough to hold the resulting bytes. Returns the amount of bytes written
    pub fn write_to(self, to: &mut [u8]) -> usize {
        let cp: u32 = self.into();
        if cp <= MAX_1_BYTE_CHAR {
            to[0] = cp as u8;
            1
        } else if cp <= MAX_2_BYTE_CHAR {
            // note: setting later bytes first to avoid multiple bound checks
            to[1] = 0x80 | (cp & 0x3F) as u8;
            to[0] = 0xC0 | (cp >> 6) as u8;
            2
        } else if cp <= MAX_3_BYTE_CHAR {
            to[2] = 0x80 | (cp & 0x3F) as u8;
            to[1] = 0x80 | ((cp >> 6) & 0x3F) as u8;
            to[0] = 0xE0 | (cp >> 12) as u8;
            3
        } else if cp <= MAX_4_BYTE_CHAR {
            to[3] = 0x80 | (cp & 0x3F) as u8;
            to[2] = 0x80 | ((cp >> 6) & 0x3F) as u8;
            to[1] = 0x80 | ((cp >> 12) & 0x3F) as u8;
            to[0] = 0xF0 | (cp >> 18) as u8;
            4
        } else if cp <= MAX_5_BYTE_CHAR {
            to[4] = 0x80 | (cp & 0x3F) as u8;
            to[3] = 0x80 | ((cp >> 6) & 0x3F) as u8;
            to[2] = 0x80 | ((cp >> 12) & 0x3F) as u8;
            to[1] = 0x80 | ((cp >> 18) & 0x0F) as u8;
            to[0] = 0xF8;
            5
        } else if cp <= MAX_CHAR {
            let b = Self::from(cp).to_byte8_unchecked();
            to[1] = 0x80 | (b & 0x3F);
            to[0] = 0xC0 | ((b >> 6) & 1);
            2
        } else {
            error!("Invalid character: {:#x}", cp)
        }
    }

    /// If character code C has modifier masks, reflect them to the character
    /// code if possible. Return the resulting code.
    pub fn resolve_modifier_mask(self) -> Self {
        let mut cp = self.0;
        // A non-ASCII character can't reflect modifier bits to the code.
        if !Self::from(cp & !char_bits::CHAR_MODIFIER_MASK).is_ascii() {
            return Self::from(cp);
        }
        let ascii = (cp & 0x7F) as u8;
        // For Meta, Shift, and Control modifiers, we need special care.
        if cp & char_bits::CHAR_SHIFT != 0 {
            let unshifted = cp & !char_bits::CHAR_SHIFT;
            // Shift modifier is valid only with [A-Za-z].
            // Shift modifier for control characters and SPC is ignored.
            if (ascii >= b'A' && ascii <= b'Z') || ascii <= b' ' {
                cp = unshifted;
            } else if ascii >= b'a' && ascii <= b'z' {
                cp = unshifted & !0x20;
            }
        }
        // Simulate the code in lread.c.
        if cp & char_bits::CHAR_CTL != 0 {
            // Allow `\C- ' and `\C-?'.
            if ascii == b' ' {
                cp &= !0x7F & !char_bits::CHAR_CTL;
            } else if ascii == b'?' {
                cp = 0x7F | (cp & !0x7F & !char_bits::CHAR_CTL);
            } else if ascii >= b'@' && ascii <= b'_' {
                // ASCII control chars are made from letters (both cases),
                // as well as the non-letters within 0o100...0o137.
                cp &= 0x1F | (!0x7F & !char_bits::CHAR_CTL);
            }
        }
        Self::from(cp)
    }
}

impl PartialEq<char> for Codepoint {
    fn eq(&self, other: &char) -> bool {
        self.0 == u32::from(*other)
    }
}

impl std::fmt::Display for Codepoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match char::try_from(self.0) {
            Ok(ch) => std::fmt::Display::fmt(&ch, f),
            Err(_) => std::fmt::LowerHex::fmt(self, f),
        }
    }
}

impl std::fmt::LowerHex for Codepoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        std::fmt::LowerHex::fmt(&self.0, f)
    }
}

impl From<u8> for Codepoint {
    fn from(u: u8) -> Self {
        Self(u32::from(u))
    }
}
impl From<u16> for Codepoint {
    fn from(u: u16) -> Self {
        Self(u32::from(u))
    }
}
impl From<u32> for Codepoint {
    fn from(u: u32) -> Self {
        Self(u)
    }
}
impl From<char> for Codepoint {
    fn from(c: char) -> Self {
        Self(u32::from(c))
    }
}
impl From<Codepoint> for u32 {
    fn from(c: Codepoint) -> Self {
        c.0
    }
}
impl From<Codepoint> for i64 {
    fn from(c: Codepoint) -> Self {
        c.0.into()
    }
}
impl From<Codepoint> for u64 {
    fn from(c: Codepoint) -> Self {
        c.0.into()
    }
}
impl From<Codepoint> for LispObject {
    fn from(c: Codepoint) -> Self {
        c.0.into()
    }
}

impl From<LispObject> for Codepoint {
    fn from(o: LispObject) -> Self {
        match o.as_fixnum() {
            Some(i) if 0 <= i && i <= EmacsInt::from(MAX_CHAR) => Self::from(i as u32),
            _ => wrong_type!(Qcharacterp, o),
        }
    }
}

// String support (LispType == 4)

impl LispStringRef {
    /// Return the string's len in bytes.
    pub fn len_bytes(self) -> ptrdiff_t {
        let s = unsafe { self.u.s };
        if s.size_byte < 0 {
            s.size
        } else {
            s.size_byte
        }
    }

    /// Return the string's length in characters.  Differs from
    /// `len_bytes` for multibyte strings.
    pub fn len_chars(self) -> ptrdiff_t {
        let s = unsafe { self.u.s };
        s.size
    }

    /// Return width of the string when displayed in the current buffer. The
    /// width is measured by how many columns it occupies on the screen while
    /// paying attention to compositions. This is a convenience function for
    /// `self.display_width(None)`
    pub fn width(self) -> usize {
        let (width, _) = self.display_width(None);
        width
    }

    /// Return width of the string when displayed in the current buffer. The
    /// width is measured by how many columns it occupies on the screen while
    /// paying attention to compositions.
    ///
    /// With `precision` argument, return the width of longest substring that
    /// doesn't exceed `precision`, and the number of characters and bytes it
    /// contains in the returned tuple.
    pub fn display_width(self, precision: Option<usize>) -> (usize, Option<(usize, usize)>) {
        // Manually determine if string is unibyte (lets us ignore multibyte
        // handling in more cases).
        let len = self.len_chars() as usize;
        let multibyte = self.len_chars() < self.len_bytes();
        // The buffer display table
        let distab = unsafe { buffer_display_table() };
        // Sum width
        let mut width = 0;
        // Character index
        let mut i = 0;
        // Byte index
        let mut b = 0;

        while i < len {
            // If there is a composition, get its id and end position.
            let (cmp_id, end) = match find_composition(i, None, self.into()) {
                Some((_, end, val)) => (
                    unsafe {
                        get_composition_id(
                            i as isize,
                            b as isize,
                            (end - i) as isize,
                            val,
                            self.into(),
                        )
                    },
                    end,
                ),
                None => (-1, 0),
            };
            let (chars, bytes, thiswidth) = if cmp_id >= 0 {
                // Character is a composition, look it up in the composition table.
                let chars = end - i;
                let bytes = unsafe { string_char_to_byte(self.into(), end as isize) } - b as isize;
                let thiswidth = unsafe { (*(*composition_table.offset(cmp_id))).width } as usize;
                (chars, bytes as usize, thiswidth)
            } else {
                // Character is a single codepoint, calculate it if multibyte, otherwise get
                // raw byte at b.
                let (ch, bytes) = if multibyte {
                    unsafe { string_char_and_length(self.const_data_ptr().add(b)) }
                } else {
                    (self.as_slice()[b].into(), 1)
                };
                let chars = unsafe { char_width(ch.val() as i32, distab) } as usize;
                (1, bytes, chars)
            };

            // Return if adding character exceeds precision
            if let Some(precision) = precision {
                if precision - width < thiswidth {
                    return (width, Some((i, b)));
                }
            }

            width = match width.checked_add(thiswidth) {
                Some(w) => w,
                None => string_overflow(),
            };
            i += chars;
            b += bytes;
        }

        // If precision argument was given, set char and byte width of substring
        let sizes = precision.map(|_| (i, b));
        (width, sizes)
    }

    pub fn is_empty(self) -> bool {
        self.len_chars() == 0
    }

    pub fn is_multibyte(self) -> bool {
        let s = unsafe { self.u.s };
        s.size_byte >= 0
    }

    pub fn data_ptr(&mut self) -> *mut c_uchar {
        let s = unsafe { self.u.s };
        s.data as *mut c_uchar
    }

    pub fn sdata_ptr(&mut self) -> *mut c_char {
        let s = unsafe { self.u.s };
        s.data as *mut c_char
    }

    pub fn const_data_ptr(self) -> *const c_uchar {
        let s = unsafe { self.u.s };
        s.data as *const c_uchar
    }

    pub fn const_sdata_ptr(self) -> *const c_char {
        let s = unsafe { self.u.s };
        s.data as *const c_char
    }

    pub fn set_intervals(&mut self, interval: *mut Lisp_Interval) {
        let mut s = unsafe { self.u.s };
        s.intervals = interval;
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.u.s.data as *const u8, self.len_bytes() as usize) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.u.s.data as *mut u8, self.len_bytes() as usize) }
    }

    pub fn byte_at(self, index: ptrdiff_t) -> u8 {
        unsafe { *self.const_data_ptr().offset(index) }
    }

    /// This function does not allocate. It will not change the size of the data allocation.
    /// It will only set the 'size' variable of the string, if it is safe to do so.
    /// Replaces STRING_SET_CHARS from C.
    pub unsafe fn set_num_chars(mut self, newsize: isize) {
        debug_assert!(if self.is_multibyte() {
            0 <= newsize && newsize == self.len_bytes()
        } else {
            newsize == self.len_chars()
        });

        self.u.s.size = newsize;
    }

    pub fn clear_data(self) {
        unsafe { memset(self.u.s.data as *mut c_void, 0, self.len_bytes() as size_t) };
    }

    /// Replaces STRING_SET_UNIBYTE in C. If your string has size 0,
    /// it will replace your string variable with 'empty_unibyte_string'.
    pub fn mark_as_unibyte(&mut self) {
        let mut s = unsafe { self.u.s };
        if s.size == 0 {
            *self = LispObject::empty_unibyte_string();
        } else {
            s.size_byte = -1;
        }
    }

    /// Mark STR as a multibyte string.  Assure that STR contains only
    /// ASCII characters in advance.
    pub fn mark_as_multibyte(&mut self) {
        let mut s = unsafe { self.u.s };
        if s.size == 0 {
            *self = LispObject::empty_unibyte_string();
        } else {
            s.size_byte = s.size;
        }
    }

    pub fn set_byte(&mut self, idx: ptrdiff_t, elt: c_uchar) {
        unsafe { ptr::write(self.data_ptr().offset(idx), elt) };
    }
}

impl PartialEq<&[u8]> for LispStringRef {
    fn eq(&self, other: &&[u8]) -> bool {
        self.as_slice() == *other
    }
}

impl<'a> PartialEq<&'a str> for LispStringRef {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_slice() == other.as_bytes()
    }
}

impl PartialEq<String> for LispStringRef {
    fn eq(&self, other: &String) -> bool {
        self == &other.as_str()
    }
}

impl LispStructuralEqual for LispStringRef {
    fn equal(
        &self,
        other: Self,
        kind: equal_kind::Type,
        _depth: i32,
        _ht: &mut LispHashTableRef,
    ) -> bool {
        self.len_chars() == other.len_chars()
            && self.len_bytes() == other.len_bytes()
            && self.as_slice() == other.as_slice()
            && (kind != equal_kind::EQUAL_INCLUDING_PROPERTIES
                || unsafe { compare_string_intervals((*self).into(), other.into()) })
    }
}

impl fmt::Display for LispStringRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let slice =
            unsafe { slice::from_raw_parts(self.const_data_ptr(), self.len_bytes() as usize) };
        write!(f, "{}", String::from_utf8_lossy(slice).into_owned())
    }
}

impl fmt::Debug for LispStringRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct LispStringRefIterator<'a> {
    string_ref: &'a LispStringRef,
    cur: usize,
}

pub struct LispStringRefCharIterator<'a>(LispStringRefIterator<'a>);

// Substitute for FETCH_STRING_CHAR_ADVANCE
impl<'a> Iterator for LispStringRefIterator<'a> {
    type Item = (usize, Codepoint);

    fn next(&mut self) -> Option<(usize, Codepoint)> {
        if self.cur < self.string_ref.len_bytes() as usize {
            let codepoint: Codepoint;
            let old_index = self.cur;
            let ref_slice = self.string_ref.as_slice();
            if self.string_ref.is_multibyte() {
                let (cp, advance) = multibyte_char_at(&ref_slice[self.cur..]);
                codepoint = cp;
                self.cur += advance;
            } else {
                codepoint = Codepoint::from(ref_slice[self.cur]);
                self.cur += 1;
            }

            Some((old_index, codepoint))
        } else {
            None
        }
    }
}

impl<'a> Iterator for LispStringRefCharIterator<'a> {
    type Item = Codepoint;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| result.1)
    }
}

impl LispStringRef {
    pub const fn char_indices(&self) -> LispStringRefIterator {
        LispStringRefIterator {
            string_ref: self,
            cur: 0,
        }
    }

    #[allow(dead_code)]
    pub fn chars(&self) -> LispStringRefCharIterator {
        LispStringRefCharIterator(self.char_indices())
    }
}

impl From<EmacsDouble> for LispObject {
    fn from(v: EmacsDouble) -> Self {
        Self::from_float(v)
    }
}

impl From<LispObject> for LispStringRef {
    fn from(o: LispObject) -> Self {
        o.as_string().unwrap_or_else(|| wrong_type!(Qstringp, o))
    }
}

impl From<LispObject> for Option<LispStringRef> {
    fn from(o: LispObject) -> Self {
        if o.is_string() {
            Some(o.force_string())
        } else {
            None
        }
    }
}

impl From<LispStringRef> for LispObject {
    fn from(s: LispStringRef) -> Self {
        Self::tag_ptr(s, Lisp_Type::Lisp_String)
    }
}

impl LispObject {
    pub fn is_string(self) -> bool {
        self.get_type() == Lisp_Type::Lisp_String
    }

    pub fn force_string(self) -> LispStringRef {
        unsafe { self.to_string_unchecked() }
    }

    pub fn as_string(self) -> Option<LispStringRef> {
        self.into()
    }

    pub unsafe fn to_string_unchecked(self) -> LispStringRef {
        LispStringRef::new(self.get_untaggedptr() as *mut Lisp_String)
    }

    pub fn empty_unibyte_string() -> LispStringRef {
        LispStringRef::from(unsafe { empty_unibyte_string })
    }

    // We can excuse not using an option here because extracting the value checks the type
    // TODO: this is false with the enum model, change this
    pub fn as_symbol_or_string(self) -> LispSymbolOrString {
        self.into()
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum LispSymbolOrString {
    String(LispStringRef),
    Symbol(LispSymbolRef),
}

impl LispSymbolOrString {
    pub fn is_string(self) -> bool {
        match self {
            LispSymbolOrString::String(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(self) -> bool {
        match self {
            LispSymbolOrString::Symbol(_) => true,
            _ => false,
        }
    }
}

impl From<LispSymbolOrString> for LispObject {
    fn from(s: LispSymbolOrString) -> Self {
        match s {
            LispSymbolOrString::String(s) => s.into(),
            LispSymbolOrString::Symbol(sym) => sym.into(),
        }
    }
}

impl From<LispSymbolOrString> for LispStringRef {
    fn from(s: LispSymbolOrString) -> Self {
        match s {
            LispSymbolOrString::String(s) => s,
            LispSymbolOrString::Symbol(sym) => sym.symbol_name().into(),
        }
    }
}

impl From<LispStringRef> for LispSymbolOrString {
    fn from(s: LispStringRef) -> Self {
        Self::String(s)
    }
}

impl From<LispSymbolOrString> for LispSymbolRef {
    fn from(s: LispSymbolOrString) -> Self {
        match s {
            LispSymbolOrString::String(s) => LispObarrayRef::global().intern(s).into(),
            LispSymbolOrString::Symbol(sym) => sym,
        }
    }
}

impl From<LispSymbolRef> for LispSymbolOrString {
    fn from(s: LispSymbolRef) -> Self {
        Self::Symbol(s)
    }
}

impl From<LispObject> for LispSymbolOrString {
    fn from(o: LispObject) -> Self {
        if let Some(s) = o.as_string() {
            Self::String(s)
        } else if let Some(sym) = o.as_symbol() {
            Self::Symbol(sym)
        } else {
            wrong_type!(Qstringp, o)
        }
    }
}

impl PartialEq<LispObject> for LispSymbolOrString {
    fn eq(&self, other: &LispObject) -> bool {
        (*other).eq(*self)
    }
}

fn invalid_character(cp: Codepoint) -> ! {
    error!("Invalid character: {:#x}", cp)
}

fn string_overflow() -> ! {
    error!("Maximum string size exceeded")
}

/// Parse unibyte string at STR of LEN bytes, and return the number of
/// bytes it may occupy when converted to multibyte string by
/// `str_to_multibyte`.
#[no_mangle]
pub unsafe extern "C" fn count_size_as_multibyte(ptr: *const c_uchar, len: ptrdiff_t) -> ptrdiff_t {
    let slice = slice::from_raw_parts(ptr, len as usize);
    slice.iter().fold(0, |total, &byte| {
        let n = if Codepoint::from(byte).is_ascii() {
            1
        } else {
            2
        };
        total.checked_add(n).unwrap_or_else(|| string_overflow())
    })
}

/// If character code C has modifier masks, reflect them to the
/// character code if possible.  Return the resulting code.
#[no_mangle]
pub extern "C" fn char_resolve_modifier_mask(ch: EmacsInt) -> EmacsInt {
    Codepoint::from(ch as u32).resolve_modifier_mask().0.into()
}

/// Store multibyte form of character CP at TO.  If CP has modifier bits,
/// handle them appropriately.
#[no_mangle]
pub unsafe extern "C" fn char_string(mut cp: c_uint, to: *mut c_uchar) -> c_int {
    if cp & char_bits::CHAR_MODIFIER_MASK != 0 {
        cp = char_resolve_modifier_mask(EmacsInt::from(cp)) as c_uint;
        cp &= !char_bits::CHAR_MODIFIER_MASK;
    }
    Codepoint::from(cp).write_to(slice::from_raw_parts_mut(to, MAX_MULTIBYTE_LENGTH)) as c_int
}

/// Convert unibyte text at STR of BYTES bytes to a multibyte text
/// that contains the same single-byte characters.  It actually
/// converts all 8-bit characters to multibyte forms.  It is assured
/// that we can use LEN bytes at STR as a work area and that is
/// enough.  Returns the byte length of the multibyte string.
#[no_mangle]
pub unsafe extern "C" fn str_to_multibyte(
    ptr: *mut c_uchar,
    len: ptrdiff_t,
    bytes: ptrdiff_t,
) -> ptrdiff_t {
    // slice covers the whole work area to be able to write back
    let slice = slice::from_raw_parts_mut(ptr, len as usize);
    // first, search ASCII-only prefix that we can skip processing
    let mut start = 0;
    for (idx, &byte) in slice.iter().enumerate() {
        if !Codepoint::from(byte).is_ascii() {
            start = idx;
            break;
        }
        // whole string is ASCII-only, done!
        if idx as ptrdiff_t == bytes - 1 {
            return bytes;
        }
    }
    // copy the rest to the end of the work area, which is guaranteed to be
    // large enough, so we can read from there while writing the output
    let offset = (len - bytes) as usize;
    let slice = &mut slice[start..];
    ptr::copy(
        slice.as_mut_ptr(),
        slice[offset..].as_mut_ptr(),
        bytes as usize - start,
    );
    let mut to = 0;
    for from in offset..slice.len() {
        let byte = slice[from];
        to += Codepoint::from_raw(byte).write_to(&mut slice[to..]);
    }
    (start + to) as ptrdiff_t
}

/// Same as `MULTIBYTE_LENGTH` macro in C.
#[allow(clippy::if_same_then_else)]
fn multibyte_length(slice: &[c_uchar], allow_encoded_raw: bool) -> Option<usize> {
    let len = slice.len();
    if len < 1 {
        None
    } else if slice[0] & 0x80 == 0 {
        Some(1)
    } else if len < 2 || slice[1] & 0xC0 != 0x80 {
        None
    } else if !allow_encoded_raw && slice[0] & 0xFE == 0xC0 {
        None
    } else if slice[0] & 0xE0 == 0xC0 {
        Some(2)
    } else if len < 3 || slice[2] & 0xC0 != 0x80 {
        None
    } else if slice[0] & 0xF0 == 0xE0 {
        Some(3)
    } else if len < 4 || slice[3] & 0xC0 != 0x80 {
        None
    } else if slice[0] & 0xF8 == 0xF0 {
        Some(4)
    } else if len < 5 || slice[4] & 0xC0 != 0x80 {
        None
    } else if slice[0] == 0xF8 && slice[1] & 0xF0 == 0x80 {
        Some(5)
    } else {
        None
    }
}

/// Same as the `STRING_CHAR_ADVANCE` macro.
pub fn multibyte_char_at(slice: &[c_uchar]) -> (Codepoint, usize) {
    let head = u32::from(slice[0]);
    if head & 0x80 == 0 {
        (head.into(), 1)
    } else if head & 0x20 == 0 {
        let mut cp = ((head & 0x1F) << 6) | (u32::from(slice[1]) & 0x3F);
        if head < 0xC2 {
            cp |= 0x3F_FF80
        };
        (cp.into(), 2)
    } else if head & 0x10 == 0 {
        let cp = ((head & 0x0F) << 12)
            | ((u32::from(slice[1]) & 0x3F) << 6)
            | (u32::from(slice[2]) & 0x3F);
        (cp.into(), 3)
    } else if head & 0x08 == 0 {
        let cp = ((head & 0x07) << 18)
            | ((u32::from(slice[1]) & 0x3F) << 12)
            | ((u32::from(slice[2]) & 0x3F) << 6)
            | (u32::from(slice[3]) & 0x3F);
        (cp.into(), 4)
    } else {
        // the relevant bytes of "head" are always zero
        let cp = ((u32::from(slice[1]) & 0x3F) << 18)
            | ((u32::from(slice[2]) & 0x3F) << 12)
            | ((u32::from(slice[3]) & 0x3F) << 6)
            | (u32::from(slice[4]) & 0x3F);
        (cp.into(), 5)
    }
}

/// Same as STRING_CHAR_AND_LENGTH
pub unsafe fn string_char_and_length(ptr: *const u8) -> (Codepoint, usize) {
    let head = *ptr;
    // using multibyte_length_by_head is slightly more expnsive, as it also
    // checks if head & 0x08 == 0. Since this is function is going to be used
    // pretty often as invocations of the original macro gets replaced, it may
    // be worth it to directly make the bitwise comparisons.
    match multibyte_length_by_head(head) {
        1 => (head.into(), 1),
        2 => {
            let cp = (u32::from((head & 0x1F) << 6) | u32::from(*ptr.add(1) & 0x3F))
                + if head < 0xC2 { 0x3F_FF_80 } else { 0 };
            (cp.into(), 2)
        }
        3 => {
            let cp = (u32::from(head & 0x0F) << 12)
                | ((u32::from(*ptr.add(1) & 0x3F)) << 6)
                | u32::from(*ptr.add(2) & 0x3F);
            (cp.into(), 3)
        }
        _ => {
            let mut len = 0;
            let cp = string_char(ptr, ptr::null_mut(), &mut len);
            ((cp as u32).into(), len as usize)
        }
    }
}

/// Same as `BYTES_BY_CHAR_HEAD` macro in C.
pub fn multibyte_length_by_head(byte: c_uchar) -> usize {
    if byte & 0x80 == 0 {
        1
    } else if byte & 0x20 == 0 {
        2
    } else if byte & 0x10 == 0 {
        3
    } else if byte & 0x08 == 0 {
        4
    } else {
        5
    }
}

/// Return the number of characters in the NBYTES bytes at PTR.
/// This works by looking at the contents and checking for multibyte
/// sequences while assuming that there's no invalid sequence.  It
/// ignores enable-multibyte-characters.
#[no_mangle]
pub unsafe extern "C" fn multibyte_chars_in_text(
    ptr: *const c_uchar,
    nbytes: ptrdiff_t,
) -> ptrdiff_t {
    let slice = slice::from_raw_parts(ptr, nbytes as usize);
    let len = slice.len();
    let mut idx = 0;
    let mut chars = 0;
    // TODO: make this an iterator?
    while idx < len {
        idx += multibyte_length(&slice[idx..], true).unwrap_or_else(|| panic!());
        chars += 1;
    }
    chars as ptrdiff_t
}

/// Parse unibyte text at STR of LEN bytes as a multibyte text, count
/// characters and bytes in it, and store them in *NCHARS and *NBYTES
/// respectively.  On counting bytes, pay attention to that 8-bit
/// characters not constructing a valid multibyte sequence are
/// represented by 2-byte in a multibyte text.
#[no_mangle]
pub unsafe extern "C" fn parse_str_as_multibyte(
    ptr: *const c_uchar,
    len: ptrdiff_t,
    nchars: *mut ptrdiff_t,
    nbytes: *mut ptrdiff_t,
) {
    let slice = slice::from_raw_parts(ptr, len as usize);
    let len = slice.len();
    let mut chars = 0;
    let mut bytes = 0;
    let mut idx = 0;
    // XXX: in the original, there is an "unchecked" version of multibyte_length
    // called while the remaining length is >= MAX_MULTIBYTE_LENGTH.
    while idx < len {
        chars += 1;
        match multibyte_length(&slice[idx..], false) {
            None => {
                // This is either an invalid multibyte sequence, or
                // one that would encode a raw 8-bit byte, which we
                // only use internally when the string is *already*
                // multibyte.
                idx += 1;
                bytes += 2;
            }
            Some(n) => {
                idx += n;
                bytes += n as ptrdiff_t;
            }
        }
    }
    *nchars = chars;
    *nbytes = bytes;
}

/// Arrange unibyte text at STR of NBYTES bytes as a multibyte text.
/// It actually converts only such 8-bit characters that don't construct
/// a multibyte sequence to multibyte forms of Latin-1 characters.  If
/// NCHARS is nonzero, set *NCHARS to the number of characters in the
/// text.  It is assured that we can use LEN bytes at STR as a work
/// area and that is enough.  Return the number of bytes of the
/// resulting text.
#[no_mangle]
pub unsafe extern "C" fn str_as_multibyte(
    ptr: *mut c_uchar,
    len: ptrdiff_t,
    mut nbytes: ptrdiff_t,
    nchars: *mut ptrdiff_t,
) -> ptrdiff_t {
    // slice covers the whole work area to be able to write back
    let slice = slice::from_raw_parts_mut(ptr, len as usize);
    // first, search ASCII-only prefix that we can skip processing
    let mut start = None;
    let mut chars = 0;
    let mut idx = 0;
    while idx < nbytes as usize {
        match multibyte_length(&slice[idx..], false) {
            None => {
                start = Some(idx);
                break;
            }
            Some(n) => {
                idx += n;
                chars += 1;
            }
        }
    }
    if let Some(start) = start {
        // copy the rest to the end of the work area, which is guaranteed to be
        // large enough, so we can read from there while writing the output
        let offset = (len - nbytes) as usize;
        let slice = &mut slice[start..];
        ptr::copy(
            slice.as_mut_ptr(),
            slice[offset..].as_mut_ptr(),
            nbytes as usize - start,
        );
        let mut to = 0;
        let mut from = offset;
        while from < slice.len() {
            chars += 1;
            match multibyte_length(&slice[from..], false) {
                Some(n) => {
                    for _ in 0..n {
                        slice[to] = slice[from];
                        from += 1;
                        to += 1;
                    }
                }
                None => {
                    let byte = slice[from];
                    to += Codepoint::from_raw(byte).write_to(&mut slice[to..]);
                    from += 1;
                }
            }
        }
        nbytes = (start + to) as ptrdiff_t;
    }
    if !nchars.is_null() {
        *nchars = chars;
    }
    nbytes
}

/// Arrange multibyte text at STR of LEN bytes as a unibyte text.  It
/// actually converts characters in the range 0x80..0xFF to unibyte.
#[no_mangle]
pub unsafe extern "C" fn str_as_unibyte(ptr: *mut c_uchar, bytes: ptrdiff_t) -> ptrdiff_t {
    let slice = slice::from_raw_parts_mut(ptr, bytes as usize);
    let mut from = 0;
    while from < bytes as usize {
        let byte = slice[from];
        match multibyte_length_by_head(byte) {
            2 if byte & 0xFE == 0xC0 => break,
            n => from += n,
        }
    }
    let mut to = from;
    while from < bytes as usize {
        let byte = slice[from];
        match multibyte_length_by_head(byte) {
            2 if byte & 0xFE == 0xC0 => {
                let newbyte = 0x80 | ((byte & 1) << 6) | (slice[from + 1] & 0x3F);
                slice[to] = newbyte;
                from += 2;
                to += 1;
            }
            n => {
                for _ in 0..n {
                    slice[to] = slice[from];
                    from += 1;
                    to += 1;
                }
            }
        }
    }
    to as ptrdiff_t
}

/// Return a character whose multibyte form is at P.  If LEN is not
/// NULL, it must be a pointer to integer.  In that case, set *LEN to
/// the byte length of the multibyte form.  If ADVANCED is not NULL, it
/// must be a pointer to unsigned char.  In that case, set *ADVANCED to
/// the ending address (i.e., the starting address of the next
/// character) of the multibyte form.
#[no_mangle]
pub unsafe extern "C" fn string_char(
    ptr: *const c_uchar,
    advanced: *mut *const c_uchar,
    len: *mut c_int,
) -> c_int {
    let slice = slice::from_raw_parts(ptr, MAX_MULTIBYTE_LENGTH);
    let (cp, cplen) = multibyte_char_at(slice);
    if !len.is_null() {
        *len = cplen as c_int;
    }
    if !advanced.is_null() {
        *advanced = ptr.add(cplen);
    }
    cp.val() as c_int
}

/// Convert eight-bit chars in SRC (in multibyte form) to the
/// corresponding byte and store in DST.  CHARS is the number of
/// characters in SRC.  The value is the number of bytes stored in DST.
/// Usually, the value is the same as CHARS, but is less than it if SRC
/// contains a non-ASCII, non-eight-bit character.
#[no_mangle]
pub unsafe extern "C" fn str_to_unibyte(
    src: *const c_uchar,
    dst: *mut c_uchar,
    chars: ptrdiff_t,
) -> ptrdiff_t {
    let mut srcslice = slice::from_raw_parts(src, chars as usize);
    let dstslice = slice::from_raw_parts_mut(dst, chars as usize);
    for i in 0..chars {
        let (cp, cplen) = multibyte_char_at(srcslice);
        srcslice = &srcslice[cplen..];
        dstslice[i as usize] = if cp.val() > MAX_5_BYTE_CHAR {
            cp.to_byte8_unchecked()
        } else if cp.is_ascii() {
            cp.0 as c_uchar
        } else {
            return i;
        };
    }
    chars
}

/// Wrapper function for find_composition in src/composite.c
pub fn find_composition(
    pos: usize,
    limit: Option<usize>,
    object: LispObject,
) -> Option<(usize, usize, LispObject)> {
    let pos = pos as isize;
    let limit = limit.map_or(-1, |l| l as isize);
    let (mut start, mut end, mut prop) = (0, 0, Qnil);
    unsafe {
        if c_find_composition(pos, limit, &mut start, &mut end, &mut prop, object) {
            Some((start as usize, end as usize, prop))
        } else {
            None
        }
    }
}

#[no_mangle]
pub extern "C" fn lisp_string_width(
    string: LispObject,
    precision: isize,
    nchars: *mut isize,
    nbytes: *mut isize,
) -> isize {
    let precision = if precision < 0 {
        None
    } else {
        Some(precision as usize)
    };
    let (width, n) = string.force_string().display_width(precision);
    if let Some((chars, bytes)) = n {
        unsafe {
            *nchars = chars as isize;
            *nbytes = bytes as isize;
        }
    };
    width as isize
}
