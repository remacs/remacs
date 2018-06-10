#![allow(unused)]

//! This module contains all FFI declarations.
//!
//! These types and constants are generated at build time to mimic how they are
//! in C:
//!
//! - `EmacsInt`
//! - `EmacsUint`
//! - `EmacsDouble`
//! - `EMACS_INT_MAX`
//! - `EMACS_INT_SIZE`
//! - `EMACS_FLOAT_SIZE`
//! - `GCTYPEBITS`
//! - `USE_LSB_TAG`
//! - `BoolBF`

extern crate libc;
extern crate std;

use data::{Lisp_Boolfwd, Lisp_Fwd, Lisp_Fwd_Bool, Lisp_Fwd_Int, Lisp_Fwd_Kboard_Obj, Lisp_Fwd_Obj,
           Lisp_Intfwd, Lisp_Kboard_Objfwd, Lisp_Objfwd};
use libc::{c_char, c_double, c_float, c_int, c_short, c_uchar, c_void, intmax_t, off_t, ptrdiff_t,
           size_t, time_t, timespec};
use lisp::LispObject;

// libc prefers not to merge pid_t as an alias for c_int in Windows, so we will not use libc::pid_t
// and alias it ourselves.
pub type pid_t = libc::c_int;

include!(concat!(env!("OUT_DIR"), "/definitions.rs"));
include!(concat!(env!("OUT_DIR"), "/globals.rs"));

pub type char_bits = u32;
pub const CHAR_ALT: char_bits = 0x0400000;
pub const CHAR_SUPER: char_bits = 0x0800000;
pub const CHAR_HYPER: char_bits = 0x1000000;
pub const CHAR_SHIFT: char_bits = 0x2000000;
pub const CHAR_CTL: char_bits = 0x4000000;
pub const CHAR_META: char_bits = 0x8000000;
pub const CHAR_MODIFIER_MASK: char_bits =
    CHAR_ALT | CHAR_SUPER | CHAR_HYPER | CHAR_SHIFT | CHAR_CTL | CHAR_META;
pub const CHARACTERBITS: char_bits = 22;

pub const PSEUDOVECTOR_FLAG: ptrdiff_t = std::isize::MAX - std::isize::MAX / 2;
pub const PSEUDOVECTOR_SIZE_BITS: ptrdiff_t = 12;
pub const PSEUDOVECTOR_SIZE_MASK: ptrdiff_t = (1 << PSEUDOVECTOR_SIZE_BITS) - 1;
pub const PSEUDOVECTOR_REST_BITS: ptrdiff_t = 12;
pub const PSEUDOVECTOR_REST_MASK: ptrdiff_t =
    (((1 << PSEUDOVECTOR_REST_BITS) - 1) << PSEUDOVECTOR_SIZE_BITS);
pub const PSEUDOVECTOR_AREA_BITS: ptrdiff_t = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS;
pub const PVEC_TYPE_MASK: ptrdiff_t = 0x3f << PSEUDOVECTOR_AREA_BITS;

// Number of bits in a LispObject tag.
pub const VALBITS: EmacsInt = EMACS_INT_SIZE * 8 - GCTYPEBITS;
pub const INTTYPEBITS: EmacsInt = GCTYPEBITS - 1;
pub const FIXNUM_BITS: EmacsInt = VALBITS + 1;
pub const VAL_MAX: EmacsInt = EMACS_INT_MAX >> (GCTYPEBITS - 1);
pub const VALMASK: EmacsInt = [VAL_MAX, -(1 << GCTYPEBITS)][USE_LSB_TAG as usize];
pub const INTMASK: EmacsInt = (EMACS_INT_MAX >> (INTTYPEBITS - 1));

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> INTTYPEBITS;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

// Max value for the first argument of wait_reading_process_output.
pub const WAIT_READING_MAX: i64 = std::i64::MAX;

#[cfg(windows)]
#[repr(C)]
struct BitfieldPadding {
    _p1: u16,
    _p2: u16,
    _p3: u32,
    _p4: u32,
}

#[cfg(not(windows))]
#[repr(C)]
struct BitfieldPadding {
    _p1: u16,
    _p2: u16,
}

/// Bit pattern used in the least significant bits of a lisp object,
/// to denote its type.
#[repr(u8)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Lisp_Type {
    // Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.
    Lisp_Symbol = 0,

    // Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
    // whose first member indicates the subtype.
    Lisp_Misc = 1,

    // Integer.  XINT (obj) is the integer value.
    Lisp_Int0 = 2,
    Lisp_Int1 = 3 + (USE_LSB_TAG as usize as u8) * 3, // 3 | 6

    // String.  XSTRING (object) points to a struct Lisp_String.
    // The length of the string, and its contents, are stored therein.
    Lisp_String = 4,

    // Vector of Lisp objects, or something resembling it.
    // XVECTOR (object) points to a struct Lisp_Vector, which contains
    // the size and contents.  The size field also contains the type
    // information, if it's not a real vector object.
    Lisp_Vectorlike = 5,

    // Cons.  XCONS (object) points to a struct Lisp_Cons.
    Lisp_Cons = 6 - (USE_LSB_TAG as usize as u8) * 3, // 6 | 3

    Lisp_Float = 7,
}

#[repr(C)]
pub enum CaseAction {
    CaseUp = 0,
    CaseDown,
    CaseCapitalize,
    CaseCapitalizeUp,
}

#[repr(C)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum PseudovecType {
    PVEC_NORMAL_VECTOR = 0,
    PVEC_FREE,
    PVEC_PROCESS,
    PVEC_FRAME,
    PVEC_WINDOW,
    PVEC_BOOL_VECTOR,
    PVEC_BUFFER,
    PVEC_HASH_TABLE,
    PVEC_TERMINAL,
    PVEC_WINDOW_CONFIGURATION,
    PVEC_SUBR,
    PVEC_OTHER,
    PVEC_XWIDGET,
    PVEC_XWIDGET_VIEW,
    PVEC_THREAD,
    PVEC_MUTEX,
    PVEC_CONDVAR,
    PVEC_MODULE_FUNCTION,

    /* These should be last, check internal_equal to see why.  */
    PVEC_COMPILED,
    PVEC_CHAR_TABLE,
    PVEC_SUB_CHAR_TABLE,
    PVEC_RECORD,
    PVEC_FONT, /* Should be last because it's used for range checking.  */
}

pub type Lisp_Compiled = u32;
pub const COMPILED_ARGLIST: Lisp_Compiled = 0;
pub const COMPILED_BYTECODE: Lisp_Compiled = 1;
pub const COMPILED_CONSTANTS: Lisp_Compiled = 2;
pub const COMPILED_STACK_DEPTH: Lisp_Compiled = 3;
pub const COMPILED_DOC_STRING: Lisp_Compiled = 4;
pub const COMPILED_INTERACTIVE: Lisp_Compiled = 5;

#[repr(C)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum TextCursorKinds {
    DEFAULT_CURSOR = -2,
    NO_CURSOR = -1,
    FILLED_BOX_CURSOR,
    HOLLOW_BOX_CURSOR,
    BAR_CURSOR,
    HBAR_CURSOR,
}

pub type bits_word = size_t;

pub type Lisp_Subr_Lang = c_int;
pub const Lisp_Subr_Lang_C: Lisp_Subr_Lang = 0;
pub const Lisp_Subr_Lang_Rust: Lisp_Subr_Lang = 1;

/// Representation of an Emacs Lisp function symbol.
#[repr(C)]
pub struct Lisp_Subr {
    pub header: Lisp_Vectorlike_Header,

    /// This is the function pointer that will be called when the user invokes
    /// the Emacs Lisp function. Also, this field is actually an union in C.
    pub function: *const c_void,

    /// The minimum number of arguments that can be passed to the Emacs Lisp
    /// function.
    pub min_args: c_short,

    /// The maximum number of arguments that can be passed to te Emacs Lisp
    /// function.
    pub max_args: c_short,

    /// The name of the function in Emacs Lisp.
    pub symbol_name: *const c_char,

    /// The interactive specification. This may be a normal prompt
    /// string, such as `"bBuffer: "` or an elisp form as a string.
    /// If the function is not interactive, this should be a null
    /// pointer.
    pub intspec: *const c_char,

    // TODO: Change this to EMACS_INT
    //
    // If you wan't to give it a try and solve this you should see this commit:
    // https://github.com/Wilfred/remacs/commit/c5461d03a411ff5c6f43885a0a9030e8a94bbc2e
    /// The docstring of the Emacs Lisp function.
    pub doc: *const c_char,

    pub lang: c_int,
}

// In order to use `lazy_static!` with LispSubr, it must be Sync. Raw
// pointers are not Sync, but it isn't a problem to define Sync if we
// never mutate LispSubr values. If we do, we will need to create
// these objects at runtime, perhaps using forget().
//
// Based on http://stackoverflow.com/a/28116557/509706
unsafe impl Sync for Lisp_Subr {}

/// Represents a string value in elisp
#[repr(C)]
pub struct Lisp_String {
    pub size: ptrdiff_t,
    pub size_byte: ptrdiff_t,
    // TODO: Use correct definition for this.
    //
    // Maybe use rust nightly unions?
    pub intervals: *mut c_void, // @TODO implement
    pub data: *mut c_char,
}

pub type symbol_redirect = u32;
pub const SYMBOL_PLAINVAL: symbol_redirect = 4;
pub const SYMBOL_VARALIAS: symbol_redirect = 1;
pub const SYMBOL_LOCALIZED: symbol_redirect = 2;
pub const SYMBOL_FORWARDED: symbol_redirect = 3;

#[repr(C)]
pub union SymbolUnion {
    pub value: LispObject,
    pub alias: *mut Lisp_Symbol,
    pub blv: *mut Lisp_Buffer_Local_Value,
    pub fwd: *mut Lisp_Fwd,
}

/// This struct has 4 bytes of padding, representing the bitfield that
/// lives at the top of a Lisp_Symbol. The first 10 bits of this field are
/// used.
#[repr(C)]
pub struct Lisp_Symbol {
    _padding: BitfieldPadding,
    pub name: LispObject,
    pub val: SymbolUnion,
    pub function: LispObject,
    pub plist: LispObject,
    pub next: *mut Lisp_Symbol,
}

extern "C" {
    pub fn get_symbol_declared_special(sym: *const Lisp_Symbol) -> bool;
    pub fn get_symbol_redirect(sym: *const Lisp_Symbol) -> symbol_redirect;
    pub fn set_symbol_redirect(sym: *const Lisp_Symbol, v: symbol_redirect);

    pub fn set_symbol_declared_special(sym: *mut Lisp_Symbol, value: bool);
}

/* The only field contains various pieces of information:
- The MSB (ARRAY_MARK_FLAG) holds the gcmarkbit.
- The next bit (PSEUDOVECTOR_FLAG) indicates whether this is a plain
  vector (0) or a pseudovector (1).
- If PSEUDOVECTOR_FLAG is 0, the rest holds the size (number
  of slots) of the vector.
- If PSEUDOVECTOR_FLAG is 1, the rest is subdivided into three fields:
  - a) pseudovector subtype held in PVEC_TYPE_MASK field;
  - b) number of LispObjects slots at the beginning of the object
    held in PSEUDOVECTOR_SIZE_MASK field.  These objects are always
    traced by the GC;
  - c) size of the rest fields held in PSEUDOVECTOR_REST_MASK and
    measured in word_size units.  Rest fields may also include
    LispObjects, but these objects usually needs some special treatment
    during GC.
  There are some exceptions.  For PVEC_FREE, b) is always zero.  For
  PVEC_BOOL_VECTOR and PVEC_SUBR, both b) and c) are always zero.
  Current layout limits the pseudovectors to 63 PVEC_xxx subtypes,
  4095 LispObjects in GC-ed area and 4095 word-sized other slots.  */

#[repr(C)]
pub struct Lisp_Vectorlike_Header {
    pub size: ptrdiff_t,
}

#[repr(C)]
pub struct Lisp_Vectorlike {
    pub header: Lisp_Vectorlike_Header,
    // shouldn't look at the contents without knowing the structure...
}

#[repr(C)]
pub struct Lisp_Vector {
    pub header: Lisp_Vectorlike_Header,
    // actually any number of items... not sure how to express this
    pub contents: [LispObject; 1],
}

// No C equivalent.  Generic type for a vectorlike with one or more
// LispObject slots after the header.
#[repr(C)]
pub struct Lisp_Vectorlike_With_Slots {
    pub header: Lisp_Vectorlike_Header,
    // actually any number of items... not sure how to express this
    pub contents: [LispObject; 1],
}

#[repr(C)]
pub struct Lisp_Bool_Vector {
    pub _header: Lisp_Vectorlike_Header,
    pub size: EmacsInt,
    // actually any number of items again
    pub data: [bits_word; 1],
}

// This is the set of data types that share a common structure.
// The first member of the structure is a type code from this set.
// The enum values are arbitrary, but we'll use large numbers to make it
// more likely that we'll spot the error if a random word in memory is
// mistakenly interpreted as a Lisp_Misc.
#[repr(C)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Lisp_Misc_Type {
    Free = 0x5eab,
    Marker,
    Overlay,
    SaveValue,
    Finalizer,
    UserPtr,
}

// Supertype of all Misc types.
#[repr(C)]
pub struct Lisp_Misc_Any {
    _padding: BitfieldPadding,
}

// TODO: write a docstring based on the docs in lisp.h.
#[repr(C)]
pub struct Lisp_Marker {
    _padding: BitfieldPadding,
    // TODO: define a proper buffer struct.
    pub buffer: *const Lisp_Buffer,
    pub next: *const Lisp_Marker,
    pub charpos: ptrdiff_t,
    pub bytepos: ptrdiff_t,
}

extern "C" {
    pub fn mget_insertion_type(marker: *const Lisp_Marker) -> BoolBF;
    pub fn mset_insertion_type(marker: *const Lisp_Marker, val: BoolBF);
    pub fn mget_next_marker(marker: *const Lisp_Marker) -> *const Lisp_Marker;
    pub fn mset_next_marker(marker: *const Lisp_Marker, next: *const Lisp_Marker);
    pub fn mget_buffer(marker: *const Lisp_Marker) -> *const Lisp_Buffer;
    pub fn mset_buffer(m: *const Lisp_Marker, b: *mut Lisp_Buffer);
    pub fn mget_charpos(m: *const Lisp_Marker) -> ptrdiff_t;
    pub fn mget_bytepos(m: *const Lisp_Marker) -> ptrdiff_t;
    pub fn mset_charpos(m: *const Lisp_Marker, charpos: ptrdiff_t);
    pub fn mset_bytepos(m: *const Lisp_Marker, bytepos: ptrdiff_t);
}

// TODO: write a docstring based on the docs in lisp.h.
#[repr(C)]
pub struct Lisp_Overlay {
    _padding: BitfieldPadding,
    pub next: *const Lisp_Overlay,
    pub start: LispObject,
    pub end: LispObject,
    pub plist: LispObject,
}

/// Represents the cursor position within an Emacs window. For
/// documentation see stuct cursor_pos in window.h.
#[repr(C)]
pub struct CursorPos {
    // Pixel position.  These are always window relative.
    x: c_int,
    y: c_int,
    // Glyph matrix position.
    hpos: c_int,
    vpos: c_int,
}

#[repr(C)]
pub struct glyph {
    /// Position from which this glyph was drawn.  If `object' below is a
    /// Lisp string, this is an index into that string.  If it is a
    /// buffer, this is a position in that buffer.  In addition, some
    /// special glyphs have special values for this:
    ///
    /// glyph standing for newline at end of line    0
    /// empty space after the end of the line       -1
    /// overlay arrow on a TTY                      -1
    /// glyph displaying line number                -1
    /// glyph at EOB that ends in a newline         -1
    /// left truncation glyphs:                     -1
    /// right truncation/continuation glyphs        next buffer position
    /// glyph standing for newline of an empty line buffer position of newline
    /// stretch glyph at left edge of R2L lines     buffer position of newline
    charpos: ptrdiff_t,

    /// Lisp object source of this glyph.  Currently either a buffer or a
    /// string, if the glyph was produced from characters which came from
    /// a buffer or a string; or nil if the glyph was inserted by
    /// redisplay for its own purposes, such as padding, truncation, or
    /// continuation glyphs, or the overlay-arrow glyphs on TTYs.
    object: LispObject,

    /// Width in pixels.
    pixel_width: i16,
}

#[repr(C)]
pub struct glyph_pool {
    // Vector of glyphs allocated from the heap.
    glyphs: *mut glyph,

    // Allocated size of `glyphs'.
    nglyphs: ptrdiff_t,

    // Number of rows and columns in a matrix.
    nrows: c_int,
    ncolumns: c_int,
}

#[repr(C)]
pub struct glyph_matrix {
    pub pool: *mut glyph_pool,
    pub rows: *mut glyph_row,
    pub rows_allocated: ptrdiff_t,
    pub nrows: c_int,
    /* more to come */
}

/// Text positions
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct text_pos {
    pub charpos: isize,
    pub bytepos: isize,
}
#[test]
fn bindgen_test_layout_text_pos() {
    assert_eq!(
        ::std::mem::size_of::<text_pos>(),
        16usize,
        concat!("Size of: ", stringify!(text_pos))
    );
    assert_eq!(
        ::std::mem::align_of::<text_pos>(),
        8usize,
        concat!("Alignment of ", stringify!(text_pos))
    );
    assert_eq!(
        unsafe { &(*(0 as *const text_pos)).charpos as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(text_pos),
            "::",
            stringify!(charpos)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const text_pos)).bytepos as *const _ as usize },
        8usize,
        concat!(
            "Alignment of field: ",
            stringify!(text_pos),
            "::",
            stringify!(bytepos)
        )
    );
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct display_pos {
    pub pos: text_pos,
    pub overlay_string_index: isize,
    pub string_pos: text_pos,
    pub dpvec_index: ::libc::c_int,
}
#[test]
fn bindgen_test_layout_display_pos() {
    assert_eq!(
        ::std::mem::size_of::<display_pos>(),
        48usize,
        concat!("Size of: ", stringify!(display_pos))
    );
    assert_eq!(
        ::std::mem::align_of::<display_pos>(),
        8usize,
        concat!("Alignment of ", stringify!(display_pos))
    );
    assert_eq!(
        unsafe { &(*(0 as *const display_pos)).pos as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(display_pos),
            "::",
            stringify!(pos)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const display_pos)).overlay_string_index as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(display_pos),
            "::",
            stringify!(overlay_string_index)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const display_pos)).string_pos as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(display_pos),
            "::",
            stringify!(string_pos)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const display_pos)).dpvec_index as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(display_pos),
            "::",
            stringify!(dpvec_index)
        )
    );
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct glyph_row {
    /// Pointers to beginnings of areas.  The end of an area A is found at
    /// A + 1 in the vector.  The last element of the vector is the end
    /// of the whole row.
    ///
    /// Kludge alert: Even if used[TEXT_AREA] == 0, glyphs[TEXT_AREA][0]'s
    /// position field is used.  It is -1 if this row does not correspond
    /// to any text; it is some buffer position if the row corresponds to
    /// an empty display line that displays a line end.  This is what old
    /// redisplay used to do.  (Except in code for terminal frames, this
    /// kludge is no longer used, I believe. --gerd).
    ///
    /// See also start, end, displays_text_p and ends_at_zv_p for cleaner
    /// ways to do it.  The special meaning of positions 0 and -1 will be
    /// removed some day, so don't use it in new code.
    pub glyphs: [*mut glyph; 4usize],

    /// Number of glyphs actually filled in areas. This could have size
    /// LAST_AREA, but it's 1 + LAST_AREA to simplify offset calculations.
    pub used: [::libc::c_short; 4usize],

    /// Hash code. This hash code is available as soon as the row
    /// is constructed, i.e. after a call to display_line.
    pub hash: ::libc::c_uint,

    /// Window-relative x and y-position of the top-left corner of this
    /// row. If y < 0, this means that eabs (y) pixels of the row are
    /// invisible because it is partially visible at the top of a window.
    /// If x < 0, this means that eabs (x) pixels of the first glyph of
    /// the text area of the row are invisible because the glyph is
    /// partially visible.
    pub x: ::libc::c_int,
    pub y: ::libc::c_int,

    /// Width of the row in pixels without taking face extension at the
    /// end of the row into account, and without counting truncation
    /// and continuation glyphs at the end of a row on ttys.
    pub pixel_width: ::libc::c_int,

    /// and height is 1 on terminal frames.
    pub ascent: ::libc::c_int,

    pub height: ::libc::c_int,
    pub phys_ascent: ::libc::c_int,
    pub phys_height: ::libc::c_int,
    pub visible_height: ::libc::c_int,
    pub extra_line_spacing: ::libc::c_int,
    pub start: display_pos,
    pub end: display_pos,
    pub minpos: text_pos,
    pub maxpos: text_pos,
    pub overlay_arrow_bitmap: ::libc::c_int,
    pub _bitfield_1: [u32; 8usize],
    pub continuation_lines_width: ::libc::c_int,
}
#[test]
fn bindgen_test_layout_glyph_row() {
    assert_eq!(
        ::std::mem::size_of::<glyph_row>(),
        248usize,
        concat!("Size of: ", stringify!(glyph_row))
    );
    assert_eq!(
        ::std::mem::align_of::<glyph_row>(),
        8usize,
        concat!("Alignment of ", stringify!(glyph_row))
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).glyphs as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(glyphs)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).used as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(used)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).hash as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(hash)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).x as *const _ as usize },
        44usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(x)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).y as *const _ as usize },
        48usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(y)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).pixel_width as *const _ as usize },
        52usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(pixel_width)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).ascent as *const _ as usize },
        56usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(ascent)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).height as *const _ as usize },
        60usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(height)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).phys_ascent as *const _ as usize },
        64usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(phys_ascent)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).phys_height as *const _ as usize },
        68usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(phys_height)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).visible_height as *const _ as usize },
        72usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(visible_height)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).extra_line_spacing as *const _ as usize },
        76usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(extra_line_spacing)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).start as *const _ as usize },
        80usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(start)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).end as *const _ as usize },
        128usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(end)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).minpos as *const _ as usize },
        176usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(minpos)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).maxpos as *const _ as usize },
        192usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(maxpos)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).overlay_arrow_bitmap as *const _ as usize },
        208usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(overlay_arrow_bitmap)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const glyph_row)).continuation_lines_width as *const _ as usize },
        244usize,
        concat!(
            "Alignment of field: ",
            stringify!(glyph_row),
            "::",
            stringify!(continuation_lines_width)
        )
    );
}

pub type face_id = i32;
pub const DEFAULT_FACE_ID: face_id = 0;
pub const MODE_LINE_FACE_ID: face_id = 1;
pub const MODE_LINE_INACTIVE_FACE_ID: face_id = 2;
pub const TOOL_BAR_FACE_ID: face_id = 3;
pub const FRINGE_FACE_ID: face_id = 4;
pub const HEADER_LINE_FACE_ID: face_id = 5;
pub const SCROLL_BAR_FACE_ID: face_id = 6;
pub const BORDER_FACE_ID: face_id = 7;
pub const CURSOR_FACE_ID: face_id = 8;
pub const MOUSE_FACE_ID: face_id = 9;
pub const MENU_FACE_ID: face_id = 10;
pub const VERTICAL_BORDER_FACE_ID: face_id = 11;
pub const WINDOW_DIVIDER_FACE_ID: face_id = 12;
pub const WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID: face_id = 13;
pub const WINDOW_DIVIDER_LAST_PIXEL_FACE_ID: face_id = 14;
pub const INTERNAL_BORDER_FACE_ID: face_id = 15;
pub const BASIC_FACE_ID_SENTINEL: face_id = 16;

/// Represents an Emacs window. For documentation see struct window in
/// window.h.
#[repr(C)]
pub struct Lisp_Window {
    pub header: Lisp_Vectorlike_Header,
    pub frame: LispObject,
    pub next: LispObject,
    pub prev: LispObject,
    pub parent: LispObject,
    pub normal_lines: LispObject,
    pub normal_cols: LispObject,
    pub new_total: LispObject,
    pub new_normal: LispObject,
    pub new_pixel: LispObject,
    pub contents: LispObject,
    pub start: LispObject,
    pub pointm: LispObject,
    pub old_pointm: LispObject,
    pub temslot: LispObject,
    pub vertical_scroll_bar: LispObject,
    pub vertical_scroll_bar_type: LispObject,
    pub horizontal_scroll_bar: LispObject,
    pub horizontal_scroll_bar_type: LispObject,
    pub display_table: LispObject,
    pub dedicated: LispObject,
    pub redisplay_end_trigger: LispObject,
    pub combination_limit: LispObject,
    pub window_parameters: LispObject,
    pub current_matrix: *mut c_void,
    pub desired_matrix: *mut c_void,
    pub prev_buffers: LispObject,
    pub next_buffers: LispObject,
    pub use_time: EmacsInt,
    pub sequence_number: EmacsInt,
    pub pixel_left: c_int,
    pub pixel_top: c_int,
    pub left_col: c_int,
    pub top_line: c_int,
    pub pixel_width: c_int,
    pub pixel_height: c_int,
    pub pixel_width_before_size_change: c_int,
    pub pixel_height_before_size_change: c_int,
    pub total_cols: c_int,
    pub total_lines: c_int,
    pub hscroll: ptrdiff_t,
    pub min_hscroll: ptrdiff_t,
    pub hscroll_whole: ptrdiff_t,
    pub last_modified: EmacsInt,
    pub last_overlay_modified: EmacsInt,
    pub last_point: ptrdiff_t,
    pub base_line_number: ptrdiff_t,
    pub base_line_pos: ptrdiff_t,
    pub column_number_displayed: ptrdiff_t,
    pub nrows_scale_factor: c_int,
    pub ncols_scale_factor: c_int,
    pub cursor: CursorPos,
    pub phys_cursor: CursorPos,
    pub output_cursor: CursorPos,
    pub last_cursor_vpos: c_int,
    pub phys_cursor_type: TextCursorKinds,
    pub phys_cursor_width: c_int,
    pub phys_cursor_ascent: c_int,
    pub phys_cursor_height: c_int,
    pub left_fringe_width: c_int,
    pub right_fringe_width: c_int,
    pub left_margin_cols: c_int,
    pub right_margin_cols: c_int,
    pub scroll_bar_width: c_int,
    pub scroll_bar_height: c_int,
    pub mode_line_height: c_int,
    pub header_line_height: c_int,
    pub window_end_pos: ptrdiff_t,
    pub window_end_vpos: c_int,
    // XXX: in Emacs, a bitfield of 16 booleans
    flags: u16,
    vscroll: c_int,
    window_end_bytepos: ptrdiff_t,
}

extern "C" {
    pub fn wget_current_matrix(w: *const Lisp_Window) -> *mut glyph_matrix;
    pub fn wget_mode_line_height(w: *const Lisp_Window) -> c_int;
    pub fn wget_parent(w: *const Lisp_Window) -> LispObject;
    pub fn wget_pixel_height(w: *const Lisp_Window) -> c_int;
    pub fn wget_pseudo_window_p(w: *const Lisp_Window) -> bool;

    pub fn wset_mode_line_height(w: *mut Lisp_Window, height: c_int);

    pub fn window_parameter(w: *const Lisp_Window, parameter: LispObject) -> LispObject;
}

/// Area in window glyph matrix.  If values are added or removed,
/// the function mark_glyph_matrix in alloc.c may need to be changed.
pub type glyph_row_area = i32;
pub const ANY_AREA: glyph_row_area = -1;
pub const LEFT_MARGIN_AREA: glyph_row_area = 0;
pub const TEXT_AREA: glyph_row_area = 1;
pub const RIGHT_MARGIN_AREA: glyph_row_area = 2;
pub const LAST_AREA: glyph_row_area = 3;

/// Represents an Emacs buffer. For documentation see struct buffer in
/// buffer.h.
#[repr(C)]
pub struct Lisp_Buffer {
    pub header: Lisp_Vectorlike_Header,
    pub name: LispObject,
    pub filename: LispObject,
    pub directory: LispObject,
    pub backed_up: LispObject,
    pub save_length: LispObject,
    pub auto_save_file_name: LispObject,
    pub read_only: LispObject,
    pub mark: LispObject,
    pub local_var_alist: LispObject,
    pub major_mode: LispObject,
    pub mode_name: LispObject,
    pub mode_line_format: LispObject,
    pub header_line_format: LispObject,
    pub keymap: LispObject,
    pub abbrev_table: LispObject,
    pub syntax_table: LispObject,
    pub category_table: LispObject,
    pub case_fold_search: LispObject,
    pub tab_width: LispObject,
    pub fill_column: LispObject,
    pub left_margin: LispObject,
    pub auto_fill_function: LispObject,
    pub downcase_table: LispObject,
    pub upcase_table: LispObject,
    pub case_canon_table: LispObject,
    pub case_eqv_table: LispObject,
    pub truncate_lines: LispObject,
    pub word_wrap: LispObject,
    pub ctl_arrow: LispObject,
    pub bidi_display_reordering: LispObject,
    pub bidi_paragraph_direction: LispObject,
    pub bidi_paragraph_separate_re: LispObject,
    pub bidi_paragraph_start_re: LispObject,
    pub selective_display: LispObject,
    pub selective_display_ellipses: LispObject,
    pub minor_modes: LispObject,
    pub overwrite_mode: LispObject,
    pub abbrev_mode: LispObject,
    pub display_table: LispObject,
    pub mark_active: LispObject,
    pub enable_multibyte_characters: LispObject,
    pub buffer_file_coding_system: LispObject,
    pub file_format: LispObject,
    pub auto_save_file_format: LispObject,
    pub cache_long_scans: LispObject,
    pub width_table: LispObject,
    pub pt_marker: LispObject,
    pub begv_marker: LispObject,
    pub zv_marker: LispObject,
    pub point_before_scroll: LispObject,
    pub file_truename: LispObject,
    pub invisibility_spec: LispObject,
    pub last_selected_window: LispObject,
    pub display_count: LispObject,
    pub left_margin_cols: LispObject,
    pub right_margin_cols: LispObject,
    pub left_fringe_width: LispObject,
    pub right_fringe_width: LispObject,
    pub fringes_outside_margins: LispObject,
    pub scroll_bar_width: LispObject,
    pub scroll_bar_height: LispObject,
    pub vertical_scroll_bar_type: LispObject,
    pub horizontal_scroll_bar_type: LispObject,
    pub indicate_empty_lines: LispObject,
    pub indicate_buffer_boundaries: LispObject,
    pub fringe_indicator_alist: LispObject,
    pub fringe_cursor_alist: LispObject,
    pub display_time: LispObject,
    pub scroll_up_aggressively: LispObject,
    pub scroll_down_aggressively: LispObject,
    pub cursor_type: LispObject,
    pub extra_line_spacing: LispObject,
    pub cursor_in_non_selected_windows: LispObject,

    pub own_text: Lisp_Buffer_Text,
    pub text: *mut Lisp_Buffer_Text,
    pub next: *mut Lisp_Buffer,

    pub pt: ptrdiff_t,
    pub pt_byte: ptrdiff_t,
    pub begv: ptrdiff_t,
    pub begv_byte: ptrdiff_t,
    pub zv: ptrdiff_t,
    pub zv_byte: ptrdiff_t,

    pub base_buffer: *mut Lisp_Buffer,
    pub indirections: c_int,
    pub window_count: c_int,
    pub local_flags: [c_uchar; 50],

    pub modtime: timespec,
    pub modtime_size: off_t,
    pub auto_save_modified: EmacsInt,
    pub display_error_modiff: EmacsInt,
    pub auto_save_failure_time: time_t,

    pub last_window_start: ptrdiff_t,
    pub newline_cache: *mut c_void,
    pub width_run_cache: *mut c_void,
    pub bidi_paragraph_cache: *mut c_void,

    // XXX in C, bitfield with two bools
    flags: u8,

    overlays_before: *mut c_void,
    overlays_after: *mut c_void,
    overlay_center: ptrdiff_t,

    undo_list: LispObject,
}

extern "C" {
    pub fn bget_overlays_before(b: *const Lisp_Buffer) -> *mut c_void;
    pub fn bget_overlays_after(b: *const Lisp_Buffer) -> *mut c_void;
    pub fn bset_markers(b: *mut Lisp_Buffer, m: *mut Lisp_Marker);
}

/// struct Lisp_Buffer_Local_Value is used in a symbol value cell when
/// the symbol has buffer-local bindings.  (Exception:
/// some buffer-local variables are built-in, with their values stored
/// in the buffer structure itself.  They are handled differently,
/// using struct Lisp_Buffer_Objfwd.)
///
/// The `realvalue' slot holds the variable's current value, or a
/// forwarding pointer to where that value is kept.  This value is the
/// one that corresponds to the loaded binding.  To read or set the
/// variable, you must first make sure the right binding is loaded;
/// then you can access the value in (or through) `realvalue'.
///
/// `buffer' and `frame' are the buffer and frame for which the loaded
/// binding was found.  If those have changed, to make sure the right
/// binding is loaded it is necessary to find which binding goes with
/// the current buffer and selected frame, then load it.  To load it,
/// first unload the previous binding, then copy the value of the new
/// binding into `realvalue' (or through it).  Also update
/// LOADED-BINDING to point to the newly loaded binding.
///
/// `local_if_set' indicates that merely setting the variable creates a
/// local binding for the current buffer.  Otherwise the latter, setting
/// the variable does not do that; only make-local-variable does that.
#[repr(C)]
pub struct Lisp_Buffer_Local_Value {
    /// True means that merely setting the variable creates a local
    /// binding for the current buffer.
    pub local_if_set: bool,
    /// True means that the binding now loaded was found.
    /// Presumably equivalent to (defcell!=valcell).
    pub found: bool,
    /// If non-NULL, a forwarding to the C var where it should also be set.
    pub fwd: *mut Lisp_Fwd, // Should never be (Buffer|Kboard)_Objfwd.
    /// The buffer or frame for which the loaded binding was found.
    pub where_: LispObject,
    /// A cons cell that holds the default value.  It has the form
    /// (SYMBOL . DEFAULT-VALUE).
    pub defcell: LispObject,
    /// The cons cell from `where's parameter alist.
    /// It always has the form (SYMBOL . VALUE)
    /// Note that if `forward' is non-nil, VALUE may be out of date.
    /// Also if the currently loaded binding is the default binding, then
    /// this is `eq'ual to defcell.
    valcell: LispObject,
}

extern "C" {
    pub fn get_blv_fwd(blv: *const Lisp_Buffer_Local_Value) -> *const Lisp_Fwd;
    pub fn get_blv_value(blv: *const Lisp_Buffer_Local_Value) -> LispObject;
}

/// Represents text contents of an Emacs buffer. For documentation see
/// struct buffer_text in buffer.h.
#[repr(C)]
pub struct Lisp_Buffer_Text {
    pub beg: *mut c_uchar,

    pub gpt: ptrdiff_t,
    pub z: ptrdiff_t,
    pub gpt_byte: ptrdiff_t,
    pub z_byte: ptrdiff_t,
    pub gap_size: ptrdiff_t,

    pub modiff: EmacsInt,
    pub chars_modiff: EmacsInt,
    pub save_modiff: EmacsInt,
    pub overlay_modiff: EmacsInt,
    pub compact: EmacsInt,

    pub beg_unchanged: ptrdiff_t,
    pub end_unchanged: ptrdiff_t,

    pub unchanged_modified: EmacsInt,
    pub overlay_unchanged_modified: EmacsInt,
    // until we define struct interval
    pub intervals: *mut c_void,
    pub markers: *mut Lisp_Marker,

    // XXX: in Emacs, a bitfield of 2 booleans
    flags: u8,
}

/// Represents a floating point value in elisp, or GC bookkeeping for
/// floats.
///
/// # Porting from C
///
/// `Lisp_Float` in C uses a union between a `double` and a
/// pointer. We assume a double, as that's the common case, and
/// require callers to transmute to a `LispFloatChain` if they need
/// the pointer.
#[repr(C)]
pub struct Lisp_Float {
    pub data: [u8; EMACS_FLOAT_SIZE as usize],
}

/// Represents a cons cell, or GC bookkeeping for cons cells.
///
/// A cons cell is pair of two pointers, used to build linked lists in
/// lisp.
///
/// # C Porting Notes
///
/// The equivalent C struct is `Lisp_Cons`. Note that the second field
/// may be used as the cdr or GC bookkeeping.
// TODO: this should be aligned to 8 bytes.
#[repr(C)]
pub struct Lisp_Cons {
    /// Car of this cons cell.
    pub car: LispObject,
    /// Cdr of this cons cell, or the chain used for the free list.
    pub cdr: LispObject,
}

/// Type of comparison for `internal_equal()`.
#[repr(C)]
pub enum EqualKind {
    NoQuit,
    Plain,
    IncludingProperties,
}

#[repr(C)]
pub struct re_registers {
    pub num_regs: libc::c_uint,
    pub start: *mut c_void, // TODO
    pub end: *mut c_void,   // TODO
}

#[repr(C)]
pub struct thread_state {
    pub header: Lisp_Vectorlike_Header,
    /// The buffer in which the last search was performed, or
    /// Qt if the last search was done in a string;
    /// Qnil if no searching has been done yet.
    pub m_last_thing_searched: LispObject,

    pub m_saved_last_thing_searched: LispObject,
    /// The thread's name.
    pub name: LispObject,

    /// The thread's function.
    pub function: LispObject,

    /// If non-nil, this thread has been signaled.
    pub error_symbol: LispObject,
    pub error_data: LispObject,

    /// If we are waiting for some event, this holds the object we are
    /// waiting on.
    pub event_object: LispObject,

    /// m_stack_bottom must be the first non-Lisp field.
    /// An address near the bottom of the stack.
    /// Tells GC how to save a copy of the stack.
    pub m_stack_bottom: *mut c_char,
    /// An address near the top of the stack.
    pub stack_top: *mut c_char,

    pub m_catchlist: *mut c_void, // TODO
    /// Chain of condition handlers currently in effect.
    /// The elements of this chain are contained in the stack frames
    /// of Fcondition_case and internal_condition_case.
    /// When an error is signaled (by calling Fsignal),
    /// this chain is searched for an element that applies.
    pub m_handlerlist: *mut c_void, // TODO
    pub m_handlerlist_list: *mut c_void, // TODO

    /// Current number of specbindings allocated in specpdl.
    pub m_specpdl_size: ptrdiff_t,

    /// Pointer to beginning of specpdl.
    pub m_specpdl: *mut c_void, // TODO
    /// Pointer to first unused element in specpdl.
    pub m_specpdl_ptr: *mut c_void, // TODO
    /// Depth in Lisp evaluations and function calls.
    pub m_lisp_eval_depth: EmacsInt,

    /// This points to the current buffer.
    pub m_current_buffer: *mut c_void,
    /// Every call to re_match, etc., must pass &search_regs as the regs
    /// argument unless you can show it is unnecessary (i.e., if re_match
    /// is certainly going to be called again before region-around-match
    /// can be called).

    /// Since the registers are now dynamically allocated, we need to make
    /// sure not to refer to the Nth register before checking that it has
    /// been allocated by checking search_regs.num_regs.

    /// The regex code keeps track of whether it has allocated the search
    /// buffer using bits in the re_pattern_buffer.  This means that whenever
    /// you compile a new pattern, it completely forgets whether it has
    /// allocated any registers, and will allocate new registers the next
    /// time you call a searching or matching function.  Therefore, we need
    /// to call re_set_registers after compiling a new pattern or after
    /// setting the match registers, so that the regex functions will be
    /// able to free or re-allocate it properly.
    pub m_search_regs: re_registers,
    /// If non-zero the match data have been saved in saved_search_regs
    /// during the execution of a sentinel or filter.
    pub m_search_regs_saved: bool,
    pub m_saved_search_regs: re_registers,
    /// This is the string or buffer in which we
    /// are matching.  It is used for looking up syntax properties.

    /// If the value is a Lisp string object, we are matching text in that
    /// string; if it's nil, we are matching text in the current buffer; if
    /// it's t, we are matching text in a C string.
    pub m_re_match_object: LispObject,
    /// This member is different from waiting_for_input.
    /// It is used to communicate to a lisp process-filter/sentinel (via the
    /// function Fwaiting_for_user_input_p) whether Emacs was waiting
    /// for user-input when that process-filter was called.
    /// waiting_for_input cannot be used as that is by definition 0 when
    /// lisp code is being evalled.
    /// This is also used in record_asynch_buffer_change.
    /// For that purpose, this must be 0
    /// when not inside wait_reading_process_output.
    pub m_waiting_for_user_input_p: c_int,
    /// True while doing kbd input.
    pub m_waiting_for_input: bool,
    // TODO: this struct is incomplete. We're missing thread_id,
    // thread_condvar, wait_condvar, not_holding_lock, and
    // next_thread.
}

extern "C" {
    pub fn SPECPDL_INDEX() -> ptrdiff_t;
}

/// Lisp_Char_Table
#[repr(C)]
pub enum ChartabSize {
    Bits0 = 6,
    Bits1 = 4,
    Bits2 = 5,
    Bits3 = 7,
}

/// Lisp_Char_Table
#[repr(C)]
pub struct Lisp_Char_Table {
    /// HEADER.SIZE is the vector's size field, which also holds the
    /// pseudovector type information.  It holds the size, too.
    /// The size counts the defalt, parent, purpose, ascii,
    /// contents, and extras slots.
    pub header: Lisp_Vectorlike_Header,

    /// This holds a default value,
    /// which is used whenever the value for a specific character is nil.
    pub default: LispObject,

    /// This points to another char table, which we inherit from when the
    /// value for a specific character is nil.  The `defalt' slot takes
    /// precedence over this.
    pub parent: LispObject,

    /// This is a symbol which says what kind of use this char-table is
    /// meant for.
    pub purpose: LispObject,

    /// The bottom sub char-table for characters of the range 0..127.  It
    /// is nil if none of ASCII character has a specific value.
    pub ascii: LispObject,

    pub contents: [LispObject; 1 << ChartabSize::Bits0 as u8],

    /// These hold additional data.  It is a vector.
    // actually any number of items
    pub extras: [LispObject; 1],
}

#[repr(C)]
pub struct Lisp_Sub_Char_Table {
    /// HEADER.SIZE is the vector's size field, which also holds the
    /// pseudovector type information.  It holds the size, too.
    pub header: Lisp_Vectorlike_Header,

    /// Depth of this sub char-table.  It should be 1, 2, or 3.  A sub
    /// char-table of depth 1 contains 16 elements, and each element
    /// covers 4096 (128*32) characters.  A sub char-table of depth 2
    /// contains 32 elements, and each element covers 128 characters.  A
    /// sub char-table of depth 3 contains 128 elements, and each element
    /// is for one character.
    pub depth: libc::c_int,

    /// Minimum character covered by the sub char-table.
    pub min_char: libc::c_int,

    /// Use set_sub_char_table_contents to set this.
    pub contents: [LispObject; 1],
}

extern "C" {
    pub fn uniprop_table_uncompress(table: LispObject, idx: libc::c_int) -> LispObject;
}

#[repr(C)]
pub struct Lisp_Process {
    pub header: Lisp_Vectorlike_Header,

    /// Name of subprocess terminal.
    pub tty_name: LispObject,

    /// Name of this process.
    pub name: LispObject,

    /// List of command arguments that this process was run with.
    /// Is set to t for a stopped network process; nil otherwise.
    pub command: LispObject,

    /// (funcall FILTER PROC STRING)  (if FILTER is non-nil)
    /// to dispose of a bunch of chars from the process all at once.
    pub filter: LispObject,

    /// (funcall SENTINEL PROCESS) when process state changes.
    pub sentinel: LispObject,

    /// (funcall LOG SERVER CLIENT MESSAGE) when a server process
    /// accepts a connection from a client.
    pub log: LispObject,

    /// Buffer that output is going to.
    pub buffer: LispObject,

    /// t if this is a real child process.  For a network or serial
    /// connection, it is a plist based on the arguments to
    /// make-network-process or make-serial-process.
    pub childp: LispObject,

    /// Plist for programs to keep per-process state information, parameters, etc.
    pub plist: LispObject,

    /// Symbol indicating the type of process: real, network, serial.
    pub process_type: LispObject,

    /// Marker set to end of last buffer-inserted output from this process.
    pub mark: LispObject,

    /// Symbol indicating status of process.
    /// This may be a symbol: run, open, closed, listen, or failed.
    /// Or it may be a pair (connect . ADDRINFOS) where ADDRINFOS is
    /// a list of remaining (PROTOCOL . ADDRINFO) pairs to try.
    /// Or it may be (failed ERR) where ERR is an integer, string or symbol.
    /// Or it may be a list, whose car is stop, exit or signal
    /// and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
    /// or (SIGNAL_NUMBER . COREDUMP_FLAG).
    pub status: LispObject,

    /// Coding-system for decoding the input from this process.
    pub decode_coding_system: LispObject,

    /// Working buffer for decoding.
    pub decoding_buf: LispObject,

    /// Coding-system for encoding the output to this process.
    pub encode_coding_system: LispObject,

    /// Working buffer for encoding.
    pub encoding_buf: LispObject,

    /// Queue for storing waiting writes.
    pub write_queue: LispObject,
    // This struct is incomplete.
    // To access remaining fields use access functions written in
    // src/process.c and export them here for use in Rust.
}

/// Functions to access members of `struct Lisp_Process`.
extern "C" {
    pub fn pget_pid(p: *const Lisp_Process) -> pid_t;
    pub fn pget_kill_without_query(p: *const Lisp_Process) -> BoolBF;
    pub fn pget_process_inherit_coding_system_flag(p: *const Lisp_Process) -> BoolBF;
}

/// Functions to set members of `struct Lisp_Process`.
extern "C" {
    pub fn pset_kill_without_query(p: *mut Lisp_Process, b: BoolBF);
}

#[repr(C)]
pub struct Lisp_Frame {
    pub header: Lisp_Vectorlike_Header,

    /// All LispObject components must come first.
    /// That ensures they are all aligned normally.

    /// Name of this frame: a Lisp string.  It is used for looking up resources,
    /// as well as for the title in some cases.
    pub name: LispObject,

    /// The name to use for the icon, the last time
    /// it was refreshed.  nil means not explicitly specified.
    pub icon_name: LispObject,

    /// This is the frame title specified explicitly, if any.
    /// Usually it is nil.
    pub title: LispObject,

    // This struct is incomplete.
    // It is difficult, if not impossible, to import the rest of this struct.
    // 1. #IFDEF logic means the proper number of fields is hard to determine.
    // 2. Bitfields are compiler dependent. How much padding, where?
    //    The current count is roughly 50 bits.
    //
    // Because of this, access functions are written in src/frame.c and
    // exported here for use in Rust. This means that instead of
    // frame.foo the proper method is fget_foo(frame).
    /// This frame's parent frame, if it has one.
    parent_frame: LispObject,

    ///  The frame which should receive keystrokes that occur in this
    /// frame, or nil if they should go to the frame itself.  This is
    /// usually nil, but if the frame is minibufferless, we can use this
    /// to redirect keystrokes to a surrogate minibuffer frame when
    /// needed.
    ///
    /// Note that a value of nil is different than having the field point
    /// to the frame itself.  Whenever the Fselect_frame function is used
    /// to shift from one frame to the other, any redirections to the
    /// original frame are shifted to the newly selected frame; if
    /// focus_frame is nil, Fselect_frame will leave it alone.
    focus_frame: LispObject,

    /// This frame's root window.  Every frame has one.
    /// If the frame has only a minibuffer window, this is it.
    /// Otherwise, if the frame has a minibuffer window, this is its sibling.
    root_window: LispObject,

    /// This frame's selected window.
    /// Each frame has its own window hierarchy
    /// and one of the windows in it is selected within the frame.
    /// The selected window of the selected frame is Emacs's selected window.
    selected_window: LispObject,

    /// This frame's minibuffer window.
    /// Most frames have their own minibuffer windows,
    /// but only the selected frame's minibuffer window
    /// can actually appear to exist.
    minibuffer_window: LispObject,

    /// Parameter alist of this frame.
    /// These are the parameters specified when creating the frame
    /// or modified with modify-frame-parameters.
    param_alist: LispObject,

    /// List of scroll bars on this frame.
    /// Actually, we don't specify exactly what is stored here at all; the
    /// scroll bar implementation code can use it to store anything it likes.
    /// This field is marked by the garbage collector.  It is here
    /// instead of in the `device' structure so that the garbage
    /// collector doesn't need to look inside the window-system-dependent
    /// structure.
    scroll_bars: LispObject,
    condemned_scroll_bars: LispObject,

    /// Vector describing the items to display in the menu bar.
    /// Each item has four elements in this vector.
    /// They are KEY, STRING, SUBMAP, and HPOS.
    /// (HPOS is not used in when the X toolkit is in use.)
    /// There are four additional elements of nil at the end, to terminate.
    menu_bar_items: LispObject,

    /// Alist of elements (FACE-NAME . FACE-VECTOR-DATA).
    face_alist: LispObject,

    /// A vector that records the entire structure of this frame's menu bar.
    /// For the format of the data, see extensive comments in xmenu.c.
    /// Only the X toolkit version uses this.
    menu_bar_vector: LispObject,

    /// Predicate for selecting buffers for other-buffer.
    buffer_predicate: LispObject,

    /// List of buffers viewed in this frame, for other-buffer.
    buffer_list: LispObject,

    /// List of buffers that were viewed, then buried in this frame.  The
    /// most recently buried buffer is first.  For last-buffer.
    buried_buffer_list: LispObject,
}

extern "C" {
    pub fn fget_buffer_list(frame: *const Lisp_Frame) -> LispObject;
    pub fn fget_buried_buffer_list(frame: *const Lisp_Frame) -> LispObject;
    pub fn fget_internal_border_width(frame: *const Lisp_Frame) -> c_int;
    pub fn fget_selected_window(frame: *const Lisp_Frame) -> LispObject;
    pub fn fset_selected_window(frame: *mut Lisp_Frame, window: LispObject);
}

pub type KBOARD = kboard;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct kboard {
    pub next_kboard: *mut KBOARD,
    pub Voverriding_terminal_local_map_: LispObject,
    pub Vlast_command_: LispObject,
    pub Vreal_last_command_: LispObject,
    pub Vkeyboard_translate_table_: LispObject,
    pub Vlast_repeatable_command_: LispObject,
    pub Vprefix_arg_: LispObject,
    pub Vlast_prefix_arg_: LispObject,
    pub kbd_queue_: LispObject,
    pub defining_kbd_macro_: LispObject,
    pub kbd_macro_buffer: *mut LispObject,
    pub kbd_macro_ptr: *mut LispObject,
    pub kbd_macro_end: *mut LispObject,
    pub kbd_macro_bufsize: isize,
    pub Vlast_kbd_macro_: LispObject,
    pub Vsystem_key_alist_: LispObject,
    pub system_key_syms_: LispObject,
    pub Vwindow_system_: LispObject,
    pub Vlocal_function_key_map_: LispObject,
    pub Vinput_decode_map_: LispObject,
    pub Vdefault_minibuffer_frame_: LispObject,
    pub reference_count: ::libc::c_int,
    pub echo_string_: LispObject,
    pub kbd_queue_has_data: BoolBF,
    pub _bitfield_1: u8,
    pub echo_prompt_: LispObject,
}
#[test]
fn bindgen_test_layout_kboard() {
    assert_eq!(
        ::std::mem::size_of::<kboard>(),
        200usize,
        concat!("Size of: ", stringify!(kboard))
    );
    assert_eq!(
        ::std::mem::align_of::<kboard>(),
        8usize,
        concat!("Alignment of ", stringify!(kboard))
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).next_kboard as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(next_kboard)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Voverriding_terminal_local_map_ as *const _ as usize },
        8usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Voverriding_terminal_local_map_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vlast_command_ as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vlast_command_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vreal_last_command_ as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vreal_last_command_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vkeyboard_translate_table_ as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vkeyboard_translate_table_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vlast_repeatable_command_ as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vlast_repeatable_command_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vprefix_arg_ as *const _ as usize },
        48usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vprefix_arg_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vlast_prefix_arg_ as *const _ as usize },
        56usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vlast_prefix_arg_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).kbd_queue_ as *const _ as usize },
        64usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(kbd_queue_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).defining_kbd_macro_ as *const _ as usize },
        72usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(defining_kbd_macro_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).kbd_macro_buffer as *const _ as usize },
        80usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(kbd_macro_buffer)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).kbd_macro_ptr as *const _ as usize },
        88usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(kbd_macro_ptr)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).kbd_macro_end as *const _ as usize },
        96usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(kbd_macro_end)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).kbd_macro_bufsize as *const _ as usize },
        104usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(kbd_macro_bufsize)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vlast_kbd_macro_ as *const _ as usize },
        112usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vlast_kbd_macro_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vsystem_key_alist_ as *const _ as usize },
        120usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vsystem_key_alist_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).system_key_syms_ as *const _ as usize },
        128usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(system_key_syms_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vwindow_system_ as *const _ as usize },
        136usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vwindow_system_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vlocal_function_key_map_ as *const _ as usize },
        144usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vlocal_function_key_map_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vinput_decode_map_ as *const _ as usize },
        152usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vinput_decode_map_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).Vdefault_minibuffer_frame_ as *const _ as usize },
        160usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(Vdefault_minibuffer_frame_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).reference_count as *const _ as usize },
        168usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(reference_count)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).echo_string_ as *const _ as usize },
        176usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(echo_string_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).kbd_queue_has_data as *const _ as usize },
        184usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(kbd_queue_has_data)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const kboard)).echo_prompt_ as *const _ as usize },
        192usize,
        concat!(
            "Alignment of field: ",
            stringify!(kboard),
            "::",
            stringify!(echo_prompt_)
        )
    );
}
impl kboard {
    #[inline]
    pub fn immediate_echo(&self) -> BoolBF {
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        let mask = 0x1 as u8;
        let val = (unit_field_val & mask) >> 0usize;
        unsafe { ::std::mem::transmute(val as u8) }
    }
    #[inline]
    pub fn set_immediate_echo(&mut self, val: BoolBF) {
        let mask = 0x1 as u8;
        let val = val as u8 as u8;
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        unit_field_val &= !mask;
        unit_field_val |= (val << 0usize) & mask;
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &unit_field_val as *const _ as *const u8,
                &mut self._bitfield_1 as *mut _ as *mut u8,
                ::std::mem::size_of::<u8>(),
            );
        }
    }
    #[inline]
    pub const fn new_bitfield_1(immediate_echo: BoolBF) -> u8 {
        (0 | ((immediate_echo as u8 as u8) << 0usize) & (0x1 as u8))
    }
}

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum output_method {
    output_initial = 0,
    output_termcap = 1,
    output_x_window = 2,
    output_msdos_raw = 3,
    output_w32 = 4,
    output_ns = 5,
}

#[repr(C)]
#[derive(Debug, Copy)]
pub struct image_cache {
    pub buckets: *mut *mut image,
    pub images: *mut *mut image,
    pub size: isize,
    pub used: isize,
    pub refcount: isize,
}
#[test]
fn bindgen_test_layout_image_cache() {
    assert_eq!(
        ::std::mem::size_of::<image_cache>(),
        40usize,
        concat!("Size of: ", stringify!(image_cache))
    );
    assert_eq!(
        ::std::mem::align_of::<image_cache>(),
        8usize,
        concat!("Alignment of ", stringify!(image_cache))
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_cache)).buckets as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_cache),
            "::",
            stringify!(buckets)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_cache)).images as *const _ as usize },
        8usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_cache),
            "::",
            stringify!(images)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_cache)).size as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_cache),
            "::",
            stringify!(size)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_cache)).used as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_cache),
            "::",
            stringify!(used)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_cache)).refcount as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_cache),
            "::",
            stringify!(refcount)
        )
    );
}
impl Clone for image_cache {
    fn clone(&self) -> Self {
        *self
    }
}

#[repr(C)]
#[derive(Debug, Copy)]
pub struct image_type {
    pub type_: ::libc::c_int,
    pub valid_p: ::std::option::Option<unsafe extern "C" fn(spec: LispObject) -> bool>,
    pub load:
        ::std::option::Option<unsafe extern "C" fn(f: *mut Lisp_Frame, img: *mut image) -> bool>,
    pub free: ::std::option::Option<unsafe extern "C" fn(f: *mut Lisp_Frame, img: *mut image)>,
    pub init: ::std::option::Option<unsafe extern "C" fn() -> bool>,
    pub next: *mut image_type,
}
#[test]
fn bindgen_test_layout_image_type() {
    assert_eq!(
        ::std::mem::size_of::<image_type>(),
        48usize,
        concat!("Size of: ", stringify!(image_type))
    );
    assert_eq!(
        ::std::mem::align_of::<image_type>(),
        8usize,
        concat!("Alignment of ", stringify!(image_type))
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_type)).type_ as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_type),
            "::",
            stringify!(type_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_type)).valid_p as *const _ as usize },
        8usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_type),
            "::",
            stringify!(valid_p)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_type)).load as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_type),
            "::",
            stringify!(load)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_type)).free as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_type),
            "::",
            stringify!(free)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_type)).init as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_type),
            "::",
            stringify!(init)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image_type)).next as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(image_type),
            "::",
            stringify!(next)
        )
    );
}
impl Clone for image_type {
    fn clone(&self) -> Self {
        *self
    }
}

pub type XID = ::libc::c_ulong;
pub type XPointer = *mut ::libc::c_char;
pub type XImagePtr = *mut ::libc::c_void;
pub type Pixmap = XID;

#[repr(C)]
#[derive(Copy)]
pub struct image {
    pub timestamp: timespec,
    pub pixmap: Pixmap,
    pub mask: Pixmap,
    pub ximg: XImagePtr,
    pub mask_img: XImagePtr,
    pub colors: *mut ::libc::c_ulong,
    pub ncolors: ::libc::c_int,
    pub background: ::libc::c_ulong,
    pub frame_foreground: ::libc::c_ulong,
    pub frame_background: ::libc::c_ulong,
    pub _bitfield_1: u8,
    pub width: ::libc::c_int,
    pub height: ::libc::c_int,
    pub corners: [::libc::c_int; 4usize],
    pub ascent: ::libc::c_int,
    pub spec: LispObject,
    pub dependencies: LispObject,
    pub relief: ::libc::c_int,
    pub hmargin: ::libc::c_int,
    pub vmargin: ::libc::c_int,
    pub type_: *mut image_type,
    pub load_failed_p: bool,
    pub lisp_data: LispObject,
    pub hash: EmacsUint,
    pub id: isize,
    pub next: *mut image,
    pub prev: *mut image,
}
#[test]
fn bindgen_test_layout_image() {
    assert_eq!(
        ::std::mem::size_of::<image>(),
        208usize,
        concat!("Size of: ", stringify!(image))
    );
    assert_eq!(
        ::std::mem::align_of::<image>(),
        8usize,
        concat!("Alignment of ", stringify!(image))
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).timestamp as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(timestamp)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).pixmap as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(pixmap)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).mask as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(mask)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).ximg as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(ximg)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).mask_img as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(mask_img)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).colors as *const _ as usize },
        48usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(colors)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).ncolors as *const _ as usize },
        56usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(ncolors)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).background as *const _ as usize },
        64usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(background)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).frame_foreground as *const _ as usize },
        72usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(frame_foreground)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).frame_background as *const _ as usize },
        80usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(frame_background)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).width as *const _ as usize },
        92usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(width)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).height as *const _ as usize },
        96usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(height)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).corners as *const _ as usize },
        100usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(corners)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).ascent as *const _ as usize },
        116usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(ascent)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).spec as *const _ as usize },
        120usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(spec)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).dependencies as *const _ as usize },
        128usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(dependencies)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).relief as *const _ as usize },
        136usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(relief)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).hmargin as *const _ as usize },
        140usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(hmargin)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).vmargin as *const _ as usize },
        144usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(vmargin)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).type_ as *const _ as usize },
        152usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(type_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).load_failed_p as *const _ as usize },
        160usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(load_failed_p)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).lisp_data as *const _ as usize },
        168usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(lisp_data)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).hash as *const _ as usize },
        176usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(hash)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).id as *const _ as usize },
        184usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(id)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).next as *const _ as usize },
        192usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(next)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const image)).prev as *const _ as usize },
        200usize,
        concat!(
            "Alignment of field: ",
            stringify!(image),
            "::",
            stringify!(prev)
        )
    );
}
impl Clone for image {
    fn clone(&self) -> Self {
        *self
    }
}
impl image {
    #[inline]
    pub fn background_transparent(&self) -> BoolBF {
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        let mask = 1u64 as u8;
        let val = (unit_field_val & mask) >> 0usize;
        unsafe { ::std::mem::transmute(val as u8) }
    }
    #[inline]
    pub fn set_background_transparent(&mut self, val: BoolBF) {
        let mask = 1u64 as u8;
        let val = val as u8 as u8;
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        unit_field_val &= !mask;
        unit_field_val |= (val << 0usize) & mask;
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &unit_field_val as *const _ as *const u8,
                &mut self._bitfield_1 as *mut _ as *mut u8,
                ::std::mem::size_of::<u8>(),
            );
        }
    }
    #[inline]
    pub fn background_valid(&self) -> BoolBF {
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        let mask = 2u64 as u8;
        let val = (unit_field_val & mask) >> 1usize;
        unsafe { ::std::mem::transmute(val as u8) }
    }
    #[inline]
    pub fn set_background_valid(&mut self, val: BoolBF) {
        let mask = 2u64 as u8;
        let val = val as u8 as u8;
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        unit_field_val &= !mask;
        unit_field_val |= (val << 1usize) & mask;
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &unit_field_val as *const _ as *const u8,
                &mut self._bitfield_1 as *mut _ as *mut u8,
                ::std::mem::size_of::<u8>(),
            );
        }
    }
    #[inline]
    pub fn background_transparent_valid(&self) -> BoolBF {
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        let mask = 4u64 as u8;
        let val = (unit_field_val & mask) >> 2usize;
        unsafe { ::std::mem::transmute(val as u8) }
    }
    #[inline]
    pub fn set_background_transparent_valid(&mut self, val: BoolBF) {
        let mask = 4u64 as u8;
        let val = val as u8 as u8;
        let mut unit_field_val: u8 = unsafe { ::std::mem::uninitialized() };
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self._bitfield_1 as *const _ as *const u8,
                &mut unit_field_val as *mut u8 as *mut u8,
                ::std::mem::size_of::<u8>(),
            )
        };
        unit_field_val &= !mask;
        unit_field_val |= (val << 2usize) & mask;
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &unit_field_val as *const _ as *const u8,
                &mut self._bitfield_1 as *mut _ as *mut u8,
                ::std::mem::size_of::<u8>(),
            );
        }
    }
    #[inline]
    pub const fn new_bitfield_1(
        background_transparent: BoolBF,
        background_valid: BoolBF,
        background_transparent_valid: BoolBF,
    ) -> u8 {
        ({
            ({ ({ 0 } | ((background_transparent as u8 as u8) << 0usize) & (1u64 as u8)) }
                | ((background_valid as u8 as u8) << 1usize) & (2u64 as u8))
        } | ((background_transparent_valid as u8 as u8) << 2usize) & (4u64 as u8))
    }
}

type tty_display_info = *mut ::libc::c_void;
type x_display_info = *mut ::libc::c_void;
type w32_display_info = *mut ::libc::c_void;
type ns_display_info = *mut ::libc::c_void;

#[repr(C)]
#[derive(Copy)]
pub union terminal_display_info {
    pub tty: *mut tty_display_info,
    pub x: *mut x_display_info,
    pub w32: *mut w32_display_info,
    pub ns: *mut ns_display_info,
}
#[test]
fn bindgen_test_layout_terminal_display_info() {
    assert_eq!(
        ::std::mem::size_of::<terminal_display_info>(),
        8usize,
        concat!("Size of: ", stringify!(terminal_display_info))
    );
    assert_eq!(
        ::std::mem::align_of::<terminal_display_info>(),
        8usize,
        concat!("Alignment of ", stringify!(terminal_display_info))
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal_display_info)).tty as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal_display_info),
            "::",
            stringify!(tty)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal_display_info)).x as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal_display_info),
            "::",
            stringify!(x)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal_display_info)).w32 as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal_display_info),
            "::",
            stringify!(w32)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal_display_info)).ns as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal_display_info),
            "::",
            stringify!(ns)
        )
    );
}
impl Clone for terminal_display_info {
    fn clone(&self) -> Self {
        *self
    }
}
#[test]
fn bindgen_test_layout_terminal() {
    assert_eq!(
        ::std::mem::size_of::<terminal>(),
        376usize,
        concat!("Size of: ", stringify!(terminal))
    );
    assert_eq!(
        ::std::mem::align_of::<terminal>(),
        8usize,
        concat!("Alignment of ", stringify!(terminal))
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).header as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(header)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).param_alist as *const _ as usize },
        8usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(param_alist)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).charset_list as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(charset_list)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).Vselection_alist as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(Vselection_alist)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).glyph_code_table as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(glyph_code_table)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).next_terminal as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(next_terminal)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).id as *const _ as usize },
        48usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(id)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).reference_count as *const _ as usize },
        52usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(reference_count)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).type_ as *const _ as usize },
        56usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(type_)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).name as *const _ as usize },
        64usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(name)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).kboard as *const _ as usize },
        72usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(kboard)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).image_cache as *const _ as usize },
        80usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(image_cache)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).display_info as *const _ as usize },
        88usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(display_info)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).terminal_coding as *const _ as usize },
        96usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(terminal_coding)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).keyboard_coding as *const _ as usize },
        104usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(keyboard_coding)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).rif as *const _ as usize },
        112usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(rif)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).cursor_to_hook as *const _ as usize },
        120usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(cursor_to_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).raw_cursor_to_hook as *const _ as usize },
        128usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(raw_cursor_to_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).clear_to_end_hook as *const _ as usize },
        136usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(clear_to_end_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).clear_frame_hook as *const _ as usize },
        144usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(clear_frame_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).clear_end_of_line_hook as *const _ as usize },
        152usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(clear_end_of_line_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).ins_del_lines_hook as *const _ as usize },
        160usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(ins_del_lines_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).insert_glyphs_hook as *const _ as usize },
        168usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(insert_glyphs_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).write_glyphs_hook as *const _ as usize },
        176usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(write_glyphs_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).delete_glyphs_hook as *const _ as usize },
        184usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(delete_glyphs_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).ring_bell_hook as *const _ as usize },
        192usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(ring_bell_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).toggle_invisible_pointer_hook as *const _ as usize },
        200usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(toggle_invisible_pointer_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).reset_terminal_modes_hook as *const _ as usize },
        208usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(reset_terminal_modes_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).set_terminal_modes_hook as *const _ as usize },
        216usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(set_terminal_modes_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).update_begin_hook as *const _ as usize },
        224usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(update_begin_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).update_end_hook as *const _ as usize },
        232usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(update_end_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).set_terminal_window_hook as *const _ as usize },
        240usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(set_terminal_window_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).mouse_position_hook as *const _ as usize },
        248usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(mouse_position_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).frame_rehighlight_hook as *const _ as usize },
        256usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(frame_rehighlight_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).frame_raise_lower_hook as *const _ as usize },
        264usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(frame_raise_lower_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).fullscreen_hook as *const _ as usize },
        272usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(fullscreen_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).menu_show_hook as *const _ as usize },
        280usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(menu_show_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).popup_dialog_hook as *const _ as usize },
        288usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(popup_dialog_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).set_vertical_scroll_bar_hook as *const _ as usize },
        296usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(set_vertical_scroll_bar_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).set_horizontal_scroll_bar_hook as *const _ as usize },
        304usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(set_horizontal_scroll_bar_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).condemn_scroll_bars_hook as *const _ as usize },
        312usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(condemn_scroll_bars_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).redeem_scroll_bar_hook as *const _ as usize },
        320usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(redeem_scroll_bar_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).judge_scroll_bars_hook as *const _ as usize },
        328usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(judge_scroll_bars_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).read_socket_hook as *const _ as usize },
        336usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(read_socket_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).frame_up_to_date_hook as *const _ as usize },
        344usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(frame_up_to_date_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).buffer_flipping_unblocked_hook as *const _ as usize },
        352usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(buffer_flipping_unblocked_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).delete_frame_hook as *const _ as usize },
        360usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(delete_frame_hook)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const terminal)).delete_terminal_hook as *const _ as usize },
        368usize,
        concat!(
            "Alignment of field: ",
            stringify!(terminal),
            "::",
            stringify!(delete_terminal_hook)
        )
    );
}

type coding_system = ::libc::c_void;
type redisplay_interface = ::libc::c_void;
type scroll_bar_part = ::libc::c_void;
type input_event = ::libc::c_void;

#[repr(C)]
pub struct terminal {
    pub header: Lisp_Vectorlike_Header,
    pub param_alist: LispObject,
    pub charset_list: LispObject,
    pub Vselection_alist: LispObject,
    pub glyph_code_table: LispObject,
    pub next_terminal: *mut terminal,
    pub id: ::libc::c_int,
    pub reference_count: ::libc::c_int,
    pub type_: output_method,
    pub name: *mut ::libc::c_char,
    pub kboard: *mut kboard,
    pub image_cache: *mut image_cache,
    pub display_info: terminal_display_info,
    pub terminal_coding: *mut coding_system,
    pub keyboard_coding: *mut coding_system,
    pub rif: *mut redisplay_interface,
    pub cursor_to_hook: ::std::option::Option<
        unsafe extern "C" fn(f: *mut Lisp_Frame, vpos: ::libc::c_int, hpos: ::libc::c_int),
    >,
    pub raw_cursor_to_hook: ::std::option::Option<
        unsafe extern "C" fn(arg1: *mut Lisp_Frame, arg2: ::libc::c_int, arg3: ::libc::c_int),
    >,
    pub clear_to_end_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub clear_frame_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub clear_end_of_line_hook:
        ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame, arg2: ::libc::c_int)>,
    pub ins_del_lines_hook: ::std::option::Option<
        unsafe extern "C" fn(f: *mut Lisp_Frame, arg1: ::libc::c_int, arg2: ::libc::c_int),
    >,
    pub insert_glyphs_hook: ::std::option::Option<
        unsafe extern "C" fn(f: *mut Lisp_Frame, s: *mut glyph, n: ::libc::c_int),
    >,
    pub write_glyphs_hook: ::std::option::Option<
        unsafe extern "C" fn(f: *mut Lisp_Frame, s: *mut glyph, n: ::libc::c_int),
    >,
    pub delete_glyphs_hook:
        ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame, arg2: ::libc::c_int)>,
    pub ring_bell_hook: ::std::option::Option<unsafe extern "C" fn(f: *mut Lisp_Frame)>,
    pub toggle_invisible_pointer_hook:
        ::std::option::Option<unsafe extern "C" fn(f: *mut Lisp_Frame, invisible: bool)>,
    pub reset_terminal_modes_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut terminal)>,
    pub set_terminal_modes_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut terminal)>,
    pub update_begin_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub update_end_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub set_terminal_window_hook:
        ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame, arg2: ::libc::c_int)>,
    pub mouse_position_hook: ::std::option::Option<
        unsafe extern "C" fn(
            f: *mut *mut Lisp_Frame,
            arg1: ::libc::c_int,
            bar_window: *mut LispObject,
            part: *mut scroll_bar_part,
            x: *mut LispObject,
            y: *mut LispObject,
            arg2: *mut Time,
        ),
    >,
    pub frame_rehighlight_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub frame_raise_lower_hook:
        ::std::option::Option<unsafe extern "C" fn(f: *mut Lisp_Frame, raise_flag: bool)>,
    pub fullscreen_hook: ::std::option::Option<unsafe extern "C" fn(f: *mut Lisp_Frame)>,
    pub menu_show_hook: ::std::option::Option<
        unsafe extern "C" fn(
            f: *mut Lisp_Frame,
            x: ::libc::c_int,
            y: ::libc::c_int,
            menuflags: ::libc::c_int,
            title: LispObject,
            error_name: *mut *const ::libc::c_char,
        ) -> LispObject,
    >,
    pub popup_dialog_hook: ::std::option::Option<
        unsafe extern "C" fn(f: *mut Lisp_Frame, header: LispObject, contents: LispObject)
            -> LispObject,
    >,
    pub set_vertical_scroll_bar_hook: ::std::option::Option<
        unsafe extern "C" fn(
            window: *mut Lisp_Window,
            portion: ::libc::c_int,
            whole: ::libc::c_int,
            position: ::libc::c_int,
        ),
    >,
    pub set_horizontal_scroll_bar_hook: ::std::option::Option<
        unsafe extern "C" fn(
            window: *mut Lisp_Window,
            portion: ::libc::c_int,
            whole: ::libc::c_int,
            position: ::libc::c_int,
        ),
    >,
    pub condemn_scroll_bars_hook:
        ::std::option::Option<unsafe extern "C" fn(frame: *mut Lisp_Frame)>,
    pub redeem_scroll_bar_hook:
        ::std::option::Option<unsafe extern "C" fn(window: *mut Lisp_Window)>,
    pub judge_scroll_bars_hook: ::std::option::Option<unsafe extern "C" fn(FRAME: *mut Lisp_Frame)>,
    pub read_socket_hook: ::std::option::Option<
        unsafe extern "C" fn(terminal: *mut terminal, hold_quit: *mut input_event) -> ::libc::c_int,
    >,
    pub frame_up_to_date_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub buffer_flipping_unblocked_hook:
        ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub delete_frame_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut Lisp_Frame)>,
    pub delete_terminal_hook: ::std::option::Option<unsafe extern "C" fn(arg1: *mut terminal)>,
}

/// Functions to access members of `struct frame`.
extern "C" {
    pub fn fget_column_width(f: *const Lisp_Frame) -> c_int;
    pub fn fget_line_height(f: *const Lisp_Frame) -> c_int;
    pub fn fget_minibuffer_window(f: *const Lisp_Frame) -> LispObject;
    pub fn fget_root_window(f: *const Lisp_Frame) -> LispObject;
    pub fn fget_terminal(f: *const Lisp_Frame) -> *const terminal;
    pub fn fget_output_method(f: *const Lisp_Frame) -> c_int;
    pub fn fget_visible(f: *const Lisp_Frame) -> bool;
    pub fn fget_iconified(f: *const Lisp_Frame) -> BoolBF;
    pub fn fget_pointer_invisible(f: *const Lisp_Frame) -> BoolBF;
    pub fn fget_top_pos(f: *const Lisp_Frame) -> c_int;
    pub fn fget_left_pos(f: *const Lisp_Frame) -> c_int;

    pub fn estimate_mode_line_height(f: *const Lisp_Frame, face_id: face_id) -> c_int;
}

extern "C" {
    pub fn pget_raw_status_new(p: *const Lisp_Process) -> bool;
}

#[repr(C)]
pub struct hash_table_test {
    pub name: LispObject,
    pub user_hash_function: LispObject,
    pub user_cmp_function: LispObject,
    pub cmpfn: extern "C" fn(t: *mut hash_table_test, a: LispObject, b: LispObject) -> bool,
    pub hashfn: extern "C" fn(t: *mut hash_table_test, a: LispObject) -> EmacsUint,
}

#[repr(C)]
pub struct Lisp_Hash_Table {
    pub header: Lisp_Vectorlike_Header,
    pub weak: LispObject,
    pub hash: LispObject,
    pub next: LispObject,
    pub index: LispObject,
    pub count: ptrdiff_t,
    pub next_free: ptrdiff_t,
    pub pure_: bool, // pure is a reserved keyword in Rust
    pub rehash_threshold: c_float,
    pub rehash_size: c_float,
    pub key_and_value: LispObject,
    pub test: hash_table_test,
    pub next_weak: *mut Lisp_Hash_Table,
}

pub type Time = u32;

/// A Lisp time (HI LO US PS), sans the cons cells.
#[repr(C)]
#[derive(Default)]
pub struct lisp_time {
    pub hi: EmacsInt,
    pub lo: c_int,
    pub us: c_int,
    pub ps: c_int,
}

pub type map_keymap_function_t =
    unsafe extern "C" fn(LispObject, LispObject, LispObject, *const c_void);
pub type voidfuncptr = unsafe extern "C" fn();

extern "C" {
    pub static initialized: bool;
    pub static mut current_global_map: LispObject;
    pub static current_thread: *mut thread_state;
    pub static empty_unibyte_string: LispObject;
    pub static fatal_error_in_progress: bool;
    pub static mut globals: emacs_globals;
    pub static initial_obarray: LispObject;
    pub static mut last_per_buffer_idx: usize;
    pub static lispsym: Lisp_Symbol;
    pub static minibuf_level: EmacsInt;
    pub static minibuf_prompt: LispObject;
    pub static minibuf_selected_window: LispObject;
    pub static mut minibuf_window: LispObject;
    pub static selected_frame: LispObject;
    pub static selected_window: LispObject;

    pub static mut Vautoload_queue: LispObject;
    pub static Vbuffer_alist: LispObject;
    pub static Vminibuffer_list: LispObject;
    pub static Vprocess_alist: LispObject;
    pub static Vrun_hooks: LispObject;

    pub fn staticpro(varaddress: *const LispObject);

    // Use LispObject::tag_ptr instead of make_lisp_ptr
    pub fn make_lisp_ptr(ptr: *const c_void, ty: Lisp_Type) -> LispObject;
    pub fn Fmake_char_table(purpose: LispObject, init: LispObject) -> LispObject;
    pub fn map_char_table(
        c_function: unsafe extern "C" fn(LispObject, LispObject, LispObject),
        function: LispObject,
        table: LispObject,
        arg: LispObject,
    );
    pub fn CHAR_TABLE_SET(ct: LispObject, idx: c_int, val: LispObject);

    pub fn aset_multibyte_string(array: LispObject, idxval: EmacsInt, c: c_int);
    pub fn Fcons(car: LispObject, cdr: LispObject) -> LispObject;
    pub fn Fsignal(error_symbol: LispObject, data: LispObject) -> !;
    pub fn Fcopy_sequence(seq: LispObject) -> LispObject;
    pub fn Ffind_operation_coding_system(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;
    pub fn Flocal_variable_p(variable: LispObject, buffer: LispObject) -> LispObject;
    pub fn Ffuncall(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;
    pub fn Fpurecopy(string: LispObject) -> LispObject;
    pub fn Fmapcar(function: LispObject, sequence: LispObject) -> LispObject;
    pub fn Fset(symbol: LispObject, newval: LispObject) -> LispObject;
    pub fn Fset_default(symbol: LispObject, value: LispObject) -> LispObject;
    pub fn Fconcat(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;
    pub fn Fnconc(nargs: ptrdiff_t, args: *mut LispObject) -> LispObject;

    pub fn make_float(float_value: c_double) -> LispObject;
    pub fn make_string(s: *const c_char, length: ptrdiff_t) -> LispObject;
    pub fn make_string_from_bytes(
        contents: *const c_char,
        nchars: ptrdiff_t,
        nbytes: ptrdiff_t,
    ) -> LispObject;
    pub fn make_pure_c_string(data: *const c_char, nchars: ptrdiff_t) -> LispObject;

    pub fn make_lisp_symbol(ptr: *mut Lisp_Symbol) -> LispObject;
    pub fn build_string(s: *const c_char) -> LispObject;
    pub fn make_unibyte_string(s: *const c_char, length: ptrdiff_t) -> LispObject;
    pub fn make_uninit_string(length: EmacsInt) -> LispObject;
    pub fn make_uninit_multibyte_string(nchars: EmacsInt, nbytes: EmacsInt) -> LispObject;
    pub fn make_specified_string(
        contents: *const c_char,
        nchars: ptrdiff_t,
        nbytes: ptrdiff_t,
        multibyte: bool,
    ) -> LispObject;
    pub fn string_to_multibyte(string: LispObject) -> LispObject;
    pub fn initial_define_key(keymap: LispObject, key: c_int, defname: *const c_char);

    pub fn eval_sub(form: LispObject) -> LispObject;

    pub fn preferred_coding_system() -> LispObject;
    pub fn Fcoding_system_p(o: LispObject) -> LispObject;
    pub fn code_convert_string(
        string: LispObject,
        coding_system: LispObject,
        dst_object: LispObject,
        encodep: bool,
        nocopy: bool,
        norecord: bool,
    ) -> LispObject;
    pub fn validate_subarray(
        array: LispObject,
        from: LispObject,
        to: LispObject,
        size: libc::ptrdiff_t,
        ifrom: &mut libc::ptrdiff_t,
        ito: &mut libc::ptrdiff_t,
    );
    pub fn string_char_to_byte(string: LispObject, char_index: libc::ptrdiff_t) -> libc::ptrdiff_t;

    pub fn record_unwind_current_buffer();
    pub fn set_buffer_internal(buffer: *mut Lisp_Buffer);
    pub fn make_buffer_string(
        start: libc::ptrdiff_t,
        end: libc::ptrdiff_t,
        props: bool,
    ) -> LispObject;

    pub fn intern_sym(sym: LispObject, obarray: LispObject, index: LispObject) -> LispObject;
    pub fn oblookup(
        obarray: LispObject,
        s: *const c_char,
        size: ptrdiff_t,
        size_bytes: ptrdiff_t,
    ) -> LispObject;

    pub fn CHECK_IMPURE(obj: LispObject, ptr: *const c_void);
    pub fn internal_equal(
        o1: LispObject,
        o2: LispObject,
        kind: EqualKind,
        depth: c_int,
        ht: LispObject,
    ) -> bool;

    pub fn emacs_abort() -> !;

    pub fn base64_encode_1(
        from: *const c_char,
        to: *mut c_char,
        length: ptrdiff_t,
        line_break: bool,
        multibyte: bool,
    ) -> ptrdiff_t;
    pub fn base64_decode_1(
        from: *const c_char,
        to: *mut c_char,
        length: ptrdiff_t,
        multibyte: bool,
        nchars_return: *mut ptrdiff_t,
    ) -> ptrdiff_t;

    pub fn allocate_pseudovector(
        vecsize: c_int,
        offset1: c_int,
        offset2: c_int,
        pvec_type: PseudovecType,
    ) -> *mut Lisp_Vector;

    pub fn extract_data_from_object(
        spec: LispObject,
        start_byte: *mut ptrdiff_t,
        end_byte: *mut ptrdiff_t,
    ) -> *mut c_char;

    pub fn hash_lookup(h: *mut Lisp_Hash_Table, key: LispObject, hash: *mut EmacsUint)
        -> ptrdiff_t;

    pub fn hash_put(
        h: *mut Lisp_Hash_Table,
        key: LispObject,
        value: LispObject,
        hash: EmacsUint,
    ) -> ptrdiff_t;
    pub fn hash_clear(h: *mut Lisp_Hash_Table);

    pub fn gc_aset(array: LispObject, idx: ptrdiff_t, val: LispObject);

    pub fn hash_remove_from_table(h: *mut Lisp_Hash_Table, key: LispObject);
    pub fn set_point_both(charpos: ptrdiff_t, bytepos: ptrdiff_t);
    pub fn set_point(charpos: ptrdiff_t);
    pub fn buf_charpos_to_bytepos(buffer: *const Lisp_Buffer, charpos: ptrdiff_t) -> ptrdiff_t;

    pub fn insert(string: *const c_char, nbytes: ptrdiff_t) -> LispObject;
    pub fn insert_and_inherit(string: *const c_char, nbytes: ptrdiff_t) -> LispObject;
    pub fn buffer_overflow();

    pub fn wait_reading_process_output(
        time_limit: intmax_t,
        nsecs: c_int,
        read_kbd: c_int,
        do_display: bool,
        wait_for_cell: LispObject,
        wait_proc: *const Lisp_Process,
        just_wait_proc: c_int,
    ) -> c_int;

    pub fn dtotimespec(sec: c_double) -> timespec;
    pub fn timespec_sub(a: timespec, b: timespec) -> timespec;
    pub fn timespec_add(a: timespec, b: timespec) -> timespec;

    pub fn current_column() -> ptrdiff_t;

    pub fn Fadd_text_properties(
        start: LispObject,
        end: LispObject,
        properties: LispObject,
        object: LispObject,
    ) -> LispObject;

    pub fn Fmake_symbol(name: LispObject) -> LispObject;
    pub fn find_symbol_value(symbol: LispObject) -> LispObject;
    pub fn symbol_is_interned(symbol: *const Lisp_Symbol) -> bool;
    pub fn symbol_is_alias(symbol: *const Lisp_Symbol) -> bool;
    pub fn symbol_is_constant(symbol: *const Lisp_Symbol) -> bool;
    pub fn misc_get_ty(any: *const Lisp_Misc_Any) -> u16;
    pub fn is_minibuffer(w: *const Lisp_Window) -> bool;
    pub fn xmalloc(size: size_t) -> *mut c_void;

    pub fn Fmapc(function: LispObject, sequence: LispObject) -> LispObject;

    pub fn Fpos_visible_in_window_p(
        pos: LispObject,
        window: LispObject,
        partially: LispObject,
    ) -> LispObject;
    pub fn find_before_next_newline(
        from: ptrdiff_t,
        to: ptrdiff_t,
        cnt: ptrdiff_t,
        bytepos: *mut ptrdiff_t,
    ) -> ptrdiff_t;
    pub fn get_process(name: LispObject) -> LispObject;
    pub fn update_status(p: *const Lisp_Process);
    pub fn setup_process_coding_systems(process: LispObject);
    pub fn send_process(
        process: LispObject,
        buf: *const c_char,
        len: ptrdiff_t,
        object: LispObject,
    );
    pub fn STRING_BYTES(s: *const Lisp_String) -> ptrdiff_t;
    pub fn Fevent_convert_list(event_desc: LispObject) -> LispObject;
    pub fn map_keymap_item(
        fun: map_keymap_function_t,
        args: LispObject,
        key: LispObject,
        val: LispObject,
        data: *const c_void,
    );
    pub fn map_keymap_char_table_item(args: LispObject, key: LispObject, val: LispObject);
    pub fn map_keymap_call(key: LispObject, val: LispObject, fun: LispObject, void: *const c_void);
    pub fn access_keymap(
        map: LispObject,
        idx: LispObject,
        ok: bool,
        noinherit: bool,
        autoload: bool,
    ) -> LispObject;
    pub fn message_with_string(m: *const c_char, string: LispObject, log: bool);
    pub fn maybe_quit();
    pub fn make_lispy_position(
        f: *const Lisp_Frame,
        x: LispObject,
        y: LispObject,
        t: Time,
    ) -> LispObject;

    pub fn make_save_funcptr_ptr_obj(a: voidfuncptr, b: *const c_void, c: LispObject)
        -> LispObject;

    pub fn Fselect_window(window: LispObject, norecord: LispObject) -> LispObject;

    pub fn Ffset(symbol: LispObject, definition: LispObject) -> LispObject;

    pub fn frame_dimension(x: c_int) -> c_int;
    pub fn window_box_left_offset(w: *const Lisp_Window, area: glyph_row_area) -> c_int;
    pub fn window_menu_bar_p(w: *const Lisp_Window) -> bool;
    pub fn window_tool_bar_p(w: *const Lisp_Window) -> bool;
    pub fn scan_newline_from_point(
        count: ptrdiff_t,
        charpos: *mut ptrdiff_t,
        bytepos: *mut ptrdiff_t,
    ) -> ptrdiff_t;

    pub fn Fmake_marker() -> LispObject;

    pub fn find_field(
        pos: LispObject,
        merge_at_boundary: LispObject,
        beg_limit: LispObject,
        beg: *mut ptrdiff_t,
        end_limit: LispObject,
        end: *mut ptrdiff_t,
    );
    pub fn find_newline(
        start: ptrdiff_t,
        start_byte: ptrdiff_t,
        end: ptrdiff_t,
        end_byte: ptrdiff_t,
        count: ptrdiff_t,
        shortage: *mut ptrdiff_t,
        bytepos: *mut ptrdiff_t,
        allow_quit: bool,
    ) -> ptrdiff_t;

    pub fn Fget_pos_property(
        position: LispObject,
        prop: LispObject,
        object: LispObject,
    ) -> LispObject;
    pub fn Fget_text_property(
        position: LispObject,
        prop: LispObject,
        object: LispObject,
    ) -> LispObject;

    pub fn get_char_property_and_overlay(
        position: LispObject,
        prop: LispObject,
        object: LispObject,
        overlay: *mut LispObject,
    ) -> LispObject;
    pub fn specbind(symbol: LispObject, value: LispObject);
    pub fn unbind_to(count: ptrdiff_t, value: LispObject) -> LispObject;
    pub fn Fapply(nargs: ptrdiff_t, args: *const LispObject) -> LispObject;

    pub fn wset_window_parameters(w: *const Lisp_Window, val: LispObject);
    pub fn wget_window_parameters(w: *const Lisp_Window) -> LispObject;

    pub fn Fnreverse(seq: LispObject) -> LispObject;

    pub fn Fload(
        file: LispObject,
        noerror: LispObject,
        nomessage: LispObject,
        nosuffix: LispObject,
        must_suffix: LispObject,
    ) -> LispObject;
    pub fn record_unwind_protect(function: unsafe extern "C" fn(LispObject), arg: LispObject);
    pub fn record_unwind_save_match_data();
    pub fn un_autoload(oldqueue: LispObject);

    pub fn unchain_marker(marker: *mut Lisp_Marker);
    pub fn del_range(from: ptrdiff_t, to: ptrdiff_t);
    pub fn buf_bytepos_to_charpos(b: *mut Lisp_Buffer, bytepos: ptrdiff_t) -> ptrdiff_t;
    pub fn Fdefault_value(symbol: LispObject) -> LispObject;
    pub fn swap_in_symval_forwarding(sym: *mut Lisp_Symbol, blv: *mut Lisp_Buffer_Local_Value);
    pub fn Fexpand_file_name(filename: LispObject, default_directory: LispObject) -> LispObject;
    pub fn Ffind_file_name_handler(filename: LispObject, operation: LispObject) -> LispObject;
    pub fn window_list_1(
        window: LispObject,
        minibuf: LispObject,
        all_frames: LispObject,
    ) -> LispObject;
    pub fn buffer_local_value(variable: LispObject, buffer: LispObject) -> LispObject;
    pub fn downcase(c: c_int) -> c_int;
    pub fn scan_lists(
        from: EmacsInt,
        count: EmacsInt,
        depth: EmacsInt,
        sexpflag: bool,
    ) -> LispObject;
    pub fn pset_sentinel(p: *mut Lisp_Process, val: LispObject);

    pub fn pset_childp(p: *mut Lisp_Process, val: LispObject);
    pub fn allocate_buffer() -> *mut Lisp_Buffer;
}

/// Contains C definitions from the font.h header.
pub mod font {
    use libc::c_int;

    /// Represents the indices of font properties in the contents of a font
    /// vector.
    ///
    /// # C Porting Notes
    ///
    /// The equivalent C enum is `font_property_index`. Since it is meant to
    /// represent indices for three different length vectors, the C definition
    /// contains duplicate variants, e.g `FONT_OBJLIST_INDEX = FONT_SPEC_MAX`,
    /// to represent sizes. These have been moved out of this enum and are
    /// available as constant `c_int` values on this module.
    #[allow(non_camel_case_types, dead_code)]
    #[repr(C)]
    pub enum FontPropertyIndex {
        FONT_TYPE_INDEX,
        FONT_FOUNDRY_INDEX,
        FONT_FAMILY_INDEX,
        FONT_ADSTYLE_INDEX,
        FONT_REGISTRY_INDEX,
        FONT_WEIGHT_INDEX,
        FONT_SLANT_INDEX,
        FONT_WIDTH_INDEX,
        FONT_SIZE_INDEX,
        FONT_DPI_INDEX,
        FONT_SPACING_INDEX,
        FONT_AVGWIDTH_INDEX,
        FONT_EXTRA_INDEX,
        // In C, we have FONT_SPEC_MAX, FONT_OBJLIST_INDEX = FONT_SPEC_MAX here.
        FONT_OBJLIST_INDEX,
        // In C, we have FONT_ENTITY_MAX, FONT_NAME_INDEX = FONT_ENTITY_MAX here.
        FONT_NAME_INDEX,
        FONT_FULLNAME_INDEX,
        FONT_FILE_INDEX,
        // In C, we have FONT_OBJECT_MAX here.
    }

    pub const FONT_SPEC_MAX: c_int = FontPropertyIndex::FONT_OBJLIST_INDEX as c_int;
    pub const FONT_ENTITY_MAX: c_int = FontPropertyIndex::FONT_NAME_INDEX as c_int;
    pub const FONT_OBJECT_MAX: c_int = (FontPropertyIndex::FONT_FILE_INDEX as c_int) + 1;
}

#[cfg(test)]
macro_rules! offset_of {
    ($ty: ty, $field: ident) => {
        unsafe { &(*(0 as *const $ty)).$field as *const _ as usize }
    };
}

#[cfg(windows)]
#[test]
fn basic_size_and_align() {
    assert!(::std::mem::size_of::<Lisp_Symbol>() == 56);
    assert!(::std::mem::size_of::<Lisp_Marker>() == 48);
    assert!(::std::mem::size_of::<Lisp_Overlay>() == 48);
    assert!(::std::mem::size_of::<SymbolUnion>() == ::std::mem::size_of::<LispObject>());
    assert!(offset_of!(Lisp_Symbol, name) == 16);
    assert!(offset_of!(Lisp_Symbol, next) == 48);
    assert!(offset_of!(Lisp_Symbol, function) == 32);
    assert!(offset_of!(Lisp_Marker, buffer) == 16);
    assert!(offset_of!(Lisp_Overlay, next) == 16);
    assert!(offset_of!(Lisp_Buffer, bidi_paragraph_cache) == 944);

    assert!(::std::mem::size_of::<ptrdiff_t>() == 8);
    assert!(::std::isize::MAX == 9223372036854775807);
}

extern "C" {
    pub fn frame_make_pointer_invisible(frame: *mut Lisp_Frame);
    pub fn bitch_at_user() -> !;
    pub fn translate_char(table: LispObject, c: EmacsInt) -> EmacsInt;
    pub fn concat(
        nargs: ptrdiff_t,
        args: *mut LispObject,
        target_type: Lisp_Type,
        last_special: bool,
    ) -> LispObject;
}

#[repr(u8)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[allow(non_camel_case_types, dead_code)]
pub enum syntaxcode {
    Whitespace,    // for a whitespace character
    Punct,         // for random punctuation characters
    Word,          // for a word constituent
    Symbol,        // symbol constituent but not word constituent
    Open,          // for a beginning delimiter
    Close,         // for an ending delimiter
    Quote,         // for a prefix character like Lisp '
    String,        // for a string-grouping character like Lisp "
    Math,          // for delimiters like $ in Tex.
    Escape,        // for a character that begins a C-style escape
    Charquote,     // for a character that quotes the following character
    Comment,       // for a comment-starting character
    Endcomment,    // for a comment-ending character
    Inherit,       // use the standard syntax table for this character
    Comment_fence, // Starts/ends comment which is delimited on the other side by any char with the same syntaxcode.
    String_fence, // Starts/ends string which is delimited on the other side by any char with the same syntaxcode.
}

extern "C" {
    pub fn syntax_property(c: libc::c_int, via_property: bool) -> syntaxcode;
    pub fn concat2(s1: LispObject, s2: LispObject) -> LispObject;
    pub fn replace_range(
        from: ptrdiff_t,
        to: ptrdiff_t,
        new: LispObject,
        prepare: bool,
        inherit: bool,
        markers: bool,
        adjust_match_data: bool,
    );
    pub fn memory_full(nbytes: libc::size_t) -> !;
    pub fn run_hook(symbol: LispObject);
    pub fn Fchar_width(ch: LispObject) -> LispObject;
    pub fn Fget(symbol: LispObject, propname: LispObject) -> LispObject;
    pub fn Fmove_to_column(column: LispObject, force: LispObject) -> LispObject;
    pub fn Fmake_string(length: LispObject, init: LispObject) -> LispObject;
    pub fn casify_object(case_action: CaseAction, object: LispObject) -> LispObject;
}

extern "C" {
    pub fn defvar_lisp(
        objfwd: *mut Lisp_Objfwd,
        lname: *const libc::c_char,
        place: *mut LispObject,
    );
    pub fn defvar_lisp_nopro(
        objfwd: *mut Lisp_Objfwd,
        lname: *const libc::c_char,
        place: *mut LispObject,
    );
    pub fn defvar_bool(objfwd: *mut Lisp_Boolfwd, lname: *const libc::c_char, place: *mut bool);
    pub fn defvar_int(objfwd: *mut Lisp_Intfwd, lname: *const libc::c_char, place: *mut EmacsInt);
    pub fn defvar_kboard(objfwd: *mut Lisp_Kboard_Objfwd, lname: *const libc::c_char, index: u32);
    pub fn update_buffer_defaults(objvar: *mut LispObject, newval: LispObject);
    pub fn wrong_choice(choice: LispObject, wrong: LispObject) -> !;
    pub fn wrong_range(min: LispObject, max: LispObject, wrong: LispObject) -> !;
    pub fn set_per_buffer_value(b: *mut Lisp_Buffer, offset: isize, value: LispObject);
}

extern "C" {
    pub static mut buffer_local_flags: Lisp_Buffer;
    pub static mut buffer_local_symbols: Lisp_Buffer;
}
