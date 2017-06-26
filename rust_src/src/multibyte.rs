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
//! * Codepoints up to 0x10FFFF coindice with Unicode.  However, the
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

use std::ptr;
use std::slice;
use libc::{ptrdiff_t, c_char, c_uchar, c_uint, c_int};

use lisp::ExternalPtr;
use remacs_sys::{CHAR_MODIFIER_MASK, CHAR_SHIFT, CHAR_CTL, emacs_abort, CHARACTERBITS, EmacsInt,
                 Lisp_String, error};

pub type LispStringRef = ExternalPtr<Lisp_String>;

// cannot use `char`, it takes values out of its range
pub type Codepoint = u32;

/// Maximum character code
pub const MAX_CHAR: Codepoint = (1 << CHARACTERBITS) - 1;

/// Maximum character codes for several encoded lengths
const MAX_1_BYTE_CHAR: Codepoint = 0x7F;
const MAX_2_BYTE_CHAR: Codepoint = 0x7FF;
const MAX_3_BYTE_CHAR: Codepoint = 0xFFFF;
const MAX_4_BYTE_CHAR: Codepoint = 0x1F_FFFF;
const MAX_5_BYTE_CHAR: Codepoint = 0x3F_FF7F;

/// Maximum length of a single encoded codepoint
const MAX_MULTIBYTE_LENGTH: usize = 5;


impl LispStringRef {
    /// Return the string's len in bytes.
    pub fn len_bytes(&self) -> ptrdiff_t {
        if self.size_byte < 0 {
            self.size
        } else {
            self.size_byte
        }
    }

    /// Return the string's length in characters.  Differs from
    /// `len_bytes` for multibyte strings.
    pub fn len_chars(&self) -> ptrdiff_t {
        self.size
    }

    pub fn is_multibyte(&self) -> bool {
        self.size_byte >= 0
    }

    pub fn data_ptr(&mut self) -> *mut c_uchar {
        self.data as *mut c_uchar
    }

    pub fn sdata_ptr(&mut self) -> *mut c_char {
        self.data as *mut c_char
    }

    pub fn const_data_ptr(&self) -> *const c_uchar {
        self.data as *const c_uchar
    }

    pub fn const_sdata_ptr(&self) -> *const c_char {
        self.data as *const c_char
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.data as *const u8, self.len_bytes() as usize) }
    }

    #[inline]
    pub fn as_mut_slice(&self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.data as *mut u8, self.len_bytes() as usize) }
    }
}

pub struct LispStringRefIterator<'a> {
    string_ref: &'a LispStringRef,
    cur: usize
}

// Substitue for FETCH_STRING_CHAR_ADVANCE
impl<'a> Iterator for LispStringRefIterator<'a> {
    type Item = (Codepoint, usize);

    fn next(&mut self) -> Option<(Codepoint, usize)> {
        if self.cur < self.string_ref.len_bytes() as usize {
            let mut codepoint: Codepoint = 0x00;
            let ref_slice = self.string_ref.as_slice();
            if self.string_ref.is_multibyte() {
                let (cp, advance) = multibyte_char_at(&ref_slice[self.cur..]);
                codepoint = cp;
                self.cur += advance;
            } else {
                codepoint = ref_slice[self.cur] as u32;
                self.cur += 1;
            }
            
            Some((codepoint, self.cur))
        } else {
            None
        }
    }
}

impl LispStringRef {
    pub fn iter(&self) -> LispStringRefIterator {
        LispStringRefIterator {
            string_ref: self,
            cur: 0
        }
    }
}

fn string_overflow() -> ! {
    unsafe { error("Maximum string size exceeded\0".as_ptr()) }
}

/// Parse unibyte string at STR of LEN bytes, and return the number of
/// bytes it may occupy when converted to multibyte string by
/// `str_to_multibyte'.
#[no_mangle]
pub fn count_size_as_multibyte(ptr: *const c_uchar, len: ptrdiff_t) -> ptrdiff_t {
    let slice = unsafe { slice::from_raw_parts(ptr, len as usize) };
    slice.iter().fold(0, |total, &byte| {
        let n = if byte < 0x80 { 1 } else { 2 };
        total.checked_add(n).unwrap_or_else(|| string_overflow())
    })
}

/// Same as the BYTE8_TO_CHAR macro.
#[inline]
fn raw_byte_codepoint(byte: c_uchar) -> Codepoint {
    if byte >= 0x80 {
        byte as Codepoint + 0x3F_FF00
    } else {
        byte as Codepoint
    }
}

/// Same as the CHAR_TO_BYTE8 macro.
#[inline]
fn raw_byte_from_codepoint(cp: Codepoint) -> c_uchar {
    (cp - 0x3F_FF00) as c_uchar
}

/// Same as the CHAR_STRING macro.
#[inline]
fn write_codepoint(to: &mut [c_uchar], cp: Codepoint) -> usize {
    if cp <= MAX_1_BYTE_CHAR {
        to[0] = cp as c_uchar;
        1
    } else if cp <= MAX_2_BYTE_CHAR {
        // note: setting later bytes first to avoid multiple bound checks
        to[1] = 0x80 | (cp & 0x3F) as c_uchar;
        to[0] = 0xC0 | (cp >> 6) as c_uchar;
        2
    } else if cp <= MAX_3_BYTE_CHAR {
        to[2] = 0x80 | (cp & 0x3F) as c_uchar;
        to[1] = 0x80 | ((cp >> 6) & 0x3F) as c_uchar;
        to[0] = 0xE0 | (cp >> 12) as c_uchar;
        3
    } else if cp <= MAX_4_BYTE_CHAR {
        to[3] = 0x80 | (cp & 0x3F) as c_uchar;
        to[2] = 0x80 | ((cp >> 6) & 0x3F) as c_uchar;
        to[1] = 0x80 | ((cp >> 12) & 0x3F) as c_uchar;
        to[0] = 0xF0 | (cp >> 18) as c_uchar;
        4
    } else if cp <= MAX_5_BYTE_CHAR {
        to[4] = 0x80 | (cp & 0x3F) as c_uchar;
        to[3] = 0x80 | ((cp >> 6) & 0x3F) as c_uchar;
        to[2] = 0x80 | ((cp >> 12) & 0x3F) as c_uchar;
        to[1] = 0x80 | ((cp >> 18) & 0x0F) as c_uchar;
        to[0] = 0xF8;
        5
    } else if cp <= MAX_CHAR {
        let b = raw_byte_from_codepoint(cp);
        to[1] = 0x80 | (b & 0x3F);
        to[0] = 0xC0 | ((b >> 6) & 1);
        2
    } else {
        unsafe { error("Invalid character: %x\0".as_ptr(), cp) }
    }
}

/// If character code C has modifier masks, reflect them to the
/// character code if possible.  Return the resulting code.
#[no_mangle]
pub fn char_resolve_modifier_mask(ch: EmacsInt) -> EmacsInt {
    let mut cp = ch as Codepoint;
    // A non-ASCII character can't reflect modifier bits to the code.
    if (cp & !CHAR_MODIFIER_MASK) >= 0x80 {
        return cp as EmacsInt;
    }
    let ascii = (cp & 0x7F) as u8;
    // For Meta, Shift, and Control modifiers, we need special care.
    if cp & CHAR_SHIFT != 0 {
        let unshifted = cp & !CHAR_SHIFT;
        // Shift modifier is valid only with [A-Za-z].
        if ascii >= b'A' && ascii <= b'Z' {
            cp = unshifted;
        } else if ascii >= b'a' && ascii <= b'z' {
            cp = unshifted & !0x20;
        } else if ascii <= b' ' {
            // Shift modifier for control characters and SPC is ignored.
            cp = unshifted;
        }
    }
    // Simulate the code in lread.c.
    if cp & CHAR_CTL != 0 {
        // Allow `\C- ' and `\C-?'.
        if ascii == b' ' {
            cp &= !0x7F & !CHAR_CTL;
        } else if ascii == b'?' {
            cp = 0x7F | (cp & !0x7F & !CHAR_CTL);
        } else if ascii >= b'@' && ascii <= b'_' {
            // ASCII control chars are made from letters (both cases),
            // as well as the non-letters within 0o100...0o137.
            cp &= 0x1F | (!0x7F & !CHAR_CTL);
        }
    }
    cp as EmacsInt
}

/// Store multibyte form of character CP at TO.  If CP has modifier bits,
/// handle them appropriately.
#[no_mangle]
pub fn char_string(mut cp: c_uint, to: *mut c_uchar) -> c_int {
    if cp & CHAR_MODIFIER_MASK != 0 {
        cp = char_resolve_modifier_mask(cp as EmacsInt) as Codepoint;
        cp &= !CHAR_MODIFIER_MASK;
    }
    write_codepoint(
        unsafe { slice::from_raw_parts_mut(to, MAX_MULTIBYTE_LENGTH) },
        cp,
    ) as c_int
}

/// Convert unibyte text at STR of BYTES bytes to a multibyte text
/// that contains the same single-byte characters.  It actually
/// converts all 8-bit characters to multibyte forms.  It is assured
/// that we can use LEN bytes at STR as a work area and that is
/// enough.  Returns the byte length of the multibyte string.
#[no_mangle]
pub fn str_to_multibyte(ptr: *mut c_uchar, len: ptrdiff_t, bytes: ptrdiff_t) -> ptrdiff_t {
    // slice covers the whole work area to be able to write back
    let slice = unsafe { slice::from_raw_parts_mut(ptr, len as usize) };
    // first, search ASCII-only prefix that we can skip processing
    let mut start = 0;
    for (idx, &byte) in slice.iter().enumerate() {
        if byte >= 0x80 {
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
    unsafe {
        ptr::copy(
            slice.as_mut_ptr(),
            slice[offset..].as_mut_ptr(),
            bytes as usize - start,
        );
    }
    let mut to = 0;
    for from in offset..slice.len() {
        let byte = slice[from];
        to += write_codepoint(&mut slice[to..], raw_byte_codepoint(byte));
    }
    (start + to) as ptrdiff_t
}

/// Same as MULTIBYTE_LENGTH macro in C.
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

/// Same as the STRING_CHAR_ADVANCE macro.
#[inline]
fn multibyte_char_at(slice: &[c_uchar]) -> (Codepoint, usize) {
    let head = slice[0] as Codepoint;
    if head & 0x80 == 0 {
        (head, 1)
    } else if head & 0x20 == 0 {
        let cp = ((head & 0x1F) << 6) | (slice[1] as Codepoint & 0x3F);
        if head < 0xC2 {
            (cp | 0x3F_FF80, 2)
        } else {
            (cp, 2)
        }
    } else if head & 0x10 == 0 {
        (
            ((head & 0x0F) << 12) | ((slice[1] as Codepoint & 0x3F) << 6) |
                (slice[2] as Codepoint & 0x3F),
            3,
        )
    } else if head & 0x08 == 0 {
        (
            ((head & 0x07) << 18) | ((slice[1] as Codepoint & 0x3F) << 12) |
                ((slice[2] as Codepoint & 0x3F) << 6) | (slice[3] as Codepoint & 0x3F),
            4,
        )
    } else {
        // the relevant bytes of "head" are always zero
        (
            ((slice[1] as Codepoint & 0x3F) << 18) | ((slice[2] as Codepoint & 0x3F) << 12) |
                ((slice[3] as Codepoint & 0x3F) << 6) | (slice[4] as Codepoint & 0x3F),
            5,
        )
    }
}

/// Same as BYTES_BY_CHAR_HEAD macro in C.
fn multibyte_length_by_head(byte: c_uchar) -> usize {
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
pub fn multibyte_chars_in_text(ptr: *const c_uchar, nbytes: ptrdiff_t) -> ptrdiff_t {
    let slice = unsafe { slice::from_raw_parts(ptr, nbytes as usize) };
    let len = slice.len();
    let mut idx = 0;
    let mut chars = 0;
    // TODO: make this an iterator?
    while idx < len {
        idx += multibyte_length(&slice[idx..], true).unwrap_or_else(|| unsafe { emacs_abort() });
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
pub fn parse_str_as_multibyte(
    ptr: *const c_uchar,
    len: ptrdiff_t,
    nchars: *mut ptrdiff_t,
    nbytes: *mut ptrdiff_t,
) {
    let slice = unsafe { slice::from_raw_parts(ptr, len as usize) };
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
    unsafe {
        *nchars = chars;
        *nbytes = bytes;
    }
}

/// Arrange unibyte text at STR of NBYTES bytes as a multibyte text.
/// It actually converts only such 8-bit characters that don't construct
/// a multibyte sequence to multibyte forms of Latin-1 characters.  If
/// NCHARS is nonzero, set *NCHARS to the number of characters in the
/// text.  It is assured that we can use LEN bytes at STR as a work
/// area and that is enough.  Return the number of bytes of the
/// resulting text.
#[no_mangle]
pub fn str_as_multibyte(
    ptr: *mut c_uchar,
    len: ptrdiff_t,
    mut nbytes: ptrdiff_t,
    nchars: *mut ptrdiff_t,
) -> ptrdiff_t {
    // slice covers the whole work area to be able to write back
    let slice = unsafe { slice::from_raw_parts_mut(ptr, len as usize) };
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
        unsafe {
            ptr::copy(
                slice.as_mut_ptr(),
                slice[offset..].as_mut_ptr(),
                nbytes as usize - start,
            );
        }
        let mut to = 0;
        let mut from = 0;
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
                    to += write_codepoint(&mut slice[to..], raw_byte_codepoint(byte));
                }
            }
        }
        nbytes = (start + to) as ptrdiff_t;
    }
    if !nchars.is_null() {
        unsafe {
            *nchars = chars;
        }
    }
    nbytes
}

/// Arrange multibyte text at STR of LEN bytes as a unibyte text.  It
/// actually converts characters in the range 0x80..0xFF to unibyte.
#[no_mangle]
pub fn str_as_unibyte(ptr: *mut c_uchar, bytes: ptrdiff_t) -> ptrdiff_t {
    let slice = unsafe { slice::from_raw_parts_mut(ptr, bytes as usize) };
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
pub fn string_char(ptr: *const c_uchar, advanced: *mut *const c_uchar, len: *mut c_int) -> c_int {
    let slice = unsafe { slice::from_raw_parts(ptr, MAX_MULTIBYTE_LENGTH) };
    let (cp, cplen) = multibyte_char_at(slice);
    if !len.is_null() {
        unsafe {
            *len = cplen as c_int;
        }
    }
    if !advanced.is_null() {
        unsafe {
            *advanced = ptr.offset(cplen as isize);
        }
    }
    cp as c_int
}

/// Convert eight-bit chars in SRC (in multibyte form) to the
/// corresponding byte and store in DST.  CHARS is the number of
/// characters in SRC.  The value is the number of bytes stored in DST.
/// Usually, the value is the same as CHARS, but is less than it if SRC
/// contains a non-ASCII, non-eight-bit character.
#[no_mangle]
pub fn str_to_unibyte(src: *const c_uchar, dst: *mut c_uchar, chars: ptrdiff_t) -> ptrdiff_t {
    let mut srcslice = unsafe { slice::from_raw_parts(src, chars as usize) };
    let mut dstslice = unsafe { slice::from_raw_parts_mut(dst, chars as usize) };
    for i in 0..chars {
        let (cp, cplen) = multibyte_char_at(srcslice);
        srcslice = &srcslice[cplen..];
        dstslice[i as usize] = if cp > MAX_5_BYTE_CHAR {
            raw_byte_from_codepoint(cp)
        } else if cp >= 0x80 {
            return i;
        } else {
            cp as c_uchar
        };
    }
    chars
}
