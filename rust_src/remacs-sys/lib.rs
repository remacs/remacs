#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

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

extern crate libc;

pub mod libm;

use libc::{c_char, c_uchar, c_short, c_int, c_double, c_float, c_void, ptrdiff_t, size_t, off_t,
           time_t, timespec};


include!(concat!(env!("OUT_DIR"), "/definitions.rs"));
include!(concat!(env!("OUT_DIR"), "/globals.rs"));

pub type Lisp_Object = EmacsInt;

pub const Qnil: Lisp_Object = 0;

pub type char_bits = u32;
pub const CHAR_ALT: char_bits = 0x0400000;
pub const CHAR_SUPER: char_bits = 0x0800000;
pub const CHAR_HYPER: char_bits = 0x1000000;
pub const CHAR_SHIFT: char_bits = 0x2000000;
pub const CHAR_CTL: char_bits = 0x4000000;
pub const CHAR_META: char_bits = 0x8000000;
pub const CHAR_MODIFIER_MASK: char_bits = CHAR_ALT | CHAR_SUPER | CHAR_HYPER | CHAR_SHIFT |
    CHAR_CTL | CHAR_META;
pub const CHARACTERBITS: char_bits = 22;

pub const PSEUDOVECTOR_FLAG: ptrdiff_t = std::isize::MAX - std::isize::MAX / 2;
pub const PSEUDOVECTOR_SIZE_BITS: ptrdiff_t = 12;
pub const PSEUDOVECTOR_SIZE_MASK: ptrdiff_t = (1 << PSEUDOVECTOR_SIZE_BITS) - 1;
pub const PSEUDOVECTOR_REST_BITS: ptrdiff_t = 12;
pub const PSEUDOVECTOR_REST_MASK: ptrdiff_t = (((1 << PSEUDOVECTOR_REST_BITS) - 1) <<
                                                   PSEUDOVECTOR_SIZE_BITS);
pub const PSEUDOVECTOR_AREA_BITS: ptrdiff_t = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS;
pub const PVEC_TYPE_MASK: ptrdiff_t = 0x3f << PSEUDOVECTOR_AREA_BITS;

// Number of bits in a Lisp_Object tag.
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

/// Bit pattern used in the least significant bits of a lisp object,
/// to denote its type.
#[repr(u8)]
#[derive(PartialEq, Eq)]
#[derive(Copy, Clone, Debug)]
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

#[repr(C)]
pub union SymbolUnion {
    pub value: Lisp_Object,
    pub alias: *mut Lisp_Symbol,
pub blv: *mut c_void, // @TODO implement Lisp_Buffer_Local_Value
pub fwd: *mut c_void, // @TODO implement Lisp_Fwd
}

/// This struct has 4 bytes of padding, representing the bitfield that
/// lives at the top of a Lisp_Symbol. The first 10 bits of this field are
/// used.
#[repr(C)]
pub struct Lisp_Symbol {
    pub symbol_bitfield: u32,
    pub name: Lisp_Object,
    pub val: SymbolUnion,
    pub function: Lisp_Object,
    pub plist: Lisp_Object,
    pub next: *mut Lisp_Symbol,
}

/* The only field contains various pieces of information:
- The MSB (ARRAY_MARK_FLAG) holds the gcmarkbit.
- The next bit (PSEUDOVECTOR_FLAG) indicates whether this is a plain
  vector (0) or a pseudovector (1).
- If PSEUDOVECTOR_FLAG is 0, the rest holds the size (number
  of slots) of the vector.
- If PSEUDOVECTOR_FLAG is 1, the rest is subdivided into three fields:
  - a) pseudovector subtype held in PVEC_TYPE_MASK field;
  - b) number of Lisp_Objects slots at the beginning of the object
    held in PSEUDOVECTOR_SIZE_MASK field.  These objects are always
    traced by the GC;
  - c) size of the rest fields held in PSEUDOVECTOR_REST_MASK and
    measured in word_size units.  Rest fields may also include
    Lisp_Objects, but these objects usually needs some special treatment
    during GC.
  There are some exceptions.  For PVEC_FREE, b) is always zero.  For
  PVEC_BOOL_VECTOR and PVEC_SUBR, both b) and c) are always zero.
  Current layout limits the pseudovectors to 63 PVEC_xxx subtypes,
  4095 Lisp_Objects in GC-ed area and 4095 word-sized other slots.  */

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
    pub contents: [Lisp_Object; 1],
}

#[repr(C)]
pub struct Lisp_Bool_Vector {
    pub _header: Lisp_Vectorlike_Header,
    pub size: EmacsInt,
    // actually any number of items again
    pub _data: [bits_word; 1],
}

// This is the set of data types that share a common structure.
// The first member of the structure is a type code from this set.
// The enum values are arbitrary, but we'll use large numbers to make it
// more likely that we'll spot the error if a random word in memory is
// mistakenly interpreted as a Lisp_Misc.
#[repr(u16)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Lisp_Misc_Type {
    Free = 0x5eab,
    Marker,
    Overlay,
    SaveValue,
    Finalizer,
}

// Supertype of all Misc types.
#[repr(C)]
pub struct Lisp_Misc_Any {
    pub ty: Lisp_Misc_Type,
    // This is actually a GC marker bit plus 15 bits of padding, but
    // we don't care right now.
    padding: u16,
}

// TODO: write a docstring based on the docs in lisp.h.
#[repr(C)]
pub struct Lisp_Marker {
    pub ty: Lisp_Misc_Type,
    // GC mark bit, 13 bits spacer, needs_adjustment flag,
    // insertion_type flag.
    padding: u16,
    // TODO: define a proper buffer struct.
    pub buffer: *const Lisp_Buffer,
    pub next: *const Lisp_Marker,
    pub charpos: ptrdiff_t,
    pub bytepos: ptrdiff_t,
}

// TODO: write a docstring based on the docs in lisp.h.
#[repr(C)]
pub struct Lisp_Overlay {
    pub ty: Lisp_Misc_Type,
    // GC mark bit, 16 bits spacer
    padding: u16,
    pub next: *const Lisp_Overlay,
    pub start: Lisp_Object,
    pub end: Lisp_Object,
    pub plist: Lisp_Object,
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

/// Represents an Emacs window. For documentation see struct window in
/// window.h.
#[repr(C)]
pub struct Lisp_Window {
    pub header: Lisp_Vectorlike_Header,
    pub frame: Lisp_Object,
    pub next: Lisp_Object,
    pub prev: Lisp_Object,
    pub parent: Lisp_Object,
    pub normal_lines: Lisp_Object,
    pub normal_cols: Lisp_Object,
    pub new_total: Lisp_Object,
    pub new_normal: Lisp_Object,
    pub new_pixel: Lisp_Object,
    pub contents: Lisp_Object,
    pub start: Lisp_Object,
    pub pointm: Lisp_Object,
    pub old_pointm: Lisp_Object,
    pub temslot: Lisp_Object,
    pub vertical_scroll_bar: Lisp_Object,
    pub vertical_scroll_bar_type: Lisp_Object,
    pub horizontal_scroll_bar: Lisp_Object,
    pub horizontal_scroll_bar_type: Lisp_Object,
    pub display_table: Lisp_Object,
    pub dedicated: Lisp_Object,
    pub redisplay_end_trigger: Lisp_Object,
    pub combination_limit: Lisp_Object,
    pub window_parameters: Lisp_Object,
    pub current_matrix: *mut c_void,
    pub desired_matrix: *mut c_void,
    pub prev_buffers: Lisp_Object,
    pub next_buffers: Lisp_Object,
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
    pub flags: u16,
    pub vscroll: c_int,
    pub window_end_bytepos: ptrdiff_t,
}

/// Represents an Emacs buffer. For documentation see struct buffer in
/// buffer.h.
#[repr(C)]
pub struct Lisp_Buffer {
    pub header: Lisp_Vectorlike_Header,
    pub name: Lisp_Object,
    pub filename: Lisp_Object,
    pub directory: Lisp_Object,
    pub backed_up: Lisp_Object,
    pub save_length: Lisp_Object,
    pub auto_save_file_name: Lisp_Object,
    pub read_only: Lisp_Object,
    pub mark: Lisp_Object,
    pub local_var_alist: Lisp_Object,
    pub major_mode: Lisp_Object,
    pub mode_name: Lisp_Object,
    pub mode_line_format: Lisp_Object,
    pub header_line_format: Lisp_Object,
    pub keymap: Lisp_Object,
    pub abbrev_table: Lisp_Object,
    pub syntax_table: Lisp_Object,
    pub category_table: Lisp_Object,
    pub case_fold_search: Lisp_Object,
    pub tab_width: Lisp_Object,
    pub fill_column: Lisp_Object,
    pub left_margin: Lisp_Object,
    pub auto_fill_function: Lisp_Object,
    pub downcase_table: Lisp_Object,
    pub upcase_table: Lisp_Object,
    pub case_canon_table: Lisp_Object,
    pub case_eqv_table: Lisp_Object,
    pub truncate_lines: Lisp_Object,
    pub word_wrap: Lisp_Object,
    pub ctl_arrow: Lisp_Object,
    pub bidi_display_reordering: Lisp_Object,
    pub bidi_paragraph_direction: Lisp_Object,
    pub bidi_paragraph_separate_re: Lisp_Object,
    pub bidi_paragraph_start_re: Lisp_Object,
    pub selective_display: Lisp_Object,
    pub selective_display_ellipses: Lisp_Object,
    pub minor_modes: Lisp_Object,
    pub overwrite_mode: Lisp_Object,
    pub abbrev_mode: Lisp_Object,
    pub display_table: Lisp_Object,
    pub mark_active: Lisp_Object,
    pub enable_multibyte_characters: Lisp_Object,
    pub buffer_file_coding_system: Lisp_Object,
    pub file_format: Lisp_Object,
    pub auto_save_file_format: Lisp_Object,
    pub cache_long_scans: Lisp_Object,
    pub width_table: Lisp_Object,
    pub pt_marker: Lisp_Object,
    pub begv_marker: Lisp_Object,
    pub zv_marker: Lisp_Object,
    pub point_before_scroll: Lisp_Object,
    pub file_truename: Lisp_Object,
    pub invisibility_spec: Lisp_Object,
    pub last_selected_window: Lisp_Object,
    pub display_count: Lisp_Object,
    pub left_margin_cols: Lisp_Object,
    pub right_margin_cols: Lisp_Object,
    pub left_fringe_width: Lisp_Object,
    pub right_fringe_width: Lisp_Object,
    pub fringes_outside_margins: Lisp_Object,
    pub scroll_bar_width: Lisp_Object,
    pub scroll_bar_height: Lisp_Object,
    pub vertical_scroll_bar_type: Lisp_Object,
    pub horizontal_scroll_bar_type: Lisp_Object,
    pub indicate_empty_lines: Lisp_Object,
    pub indicate_buffer_boundaries: Lisp_Object,
    pub fringe_indicator_alist: Lisp_Object,
    pub fringe_cursor_alist: Lisp_Object,
    pub display_time: Lisp_Object,
    pub scroll_up_aggressively: Lisp_Object,
    pub scroll_down_aggressively: Lisp_Object,
    pub cursor_type: Lisp_Object,
    pub extra_line_spacing: Lisp_Object,
    pub cursor_in_non_selected_windows: Lisp_Object,

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
    pub flags: u8,

    pub overlays_before: *mut c_void,
    pub overlays_after: *mut c_void,
    pub overlay_center: ptrdiff_t,

    pub undo_list: Lisp_Object,
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
    pub flags: u8,
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
    pub car: Lisp_Object,
    /// Cdr of this cons cell, or the chain used for the free list.
    pub cdr: Lisp_Object,
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
    pub end: *mut c_void, // TODO
}

#[repr(C)]
pub struct thread_state {
    pub header: Lisp_Vectorlike_Header,
    /// The buffer in which the last search was performed, or
    /// Qt if the last search was done in a string;
    /// Qnil if no searching has been done yet.
    pub m_last_thing_searched: Lisp_Object,

    pub m_saved_last_thing_searched: Lisp_Object,
    /// The thread's name.
    pub name: Lisp_Object,

    /// The thread's function.
    pub function: Lisp_Object,

    /// If non-nil, this thread has been signaled.
    pub error_symbol: Lisp_Object,
    pub error_data: Lisp_Object,

    /// If we are waiting for some event, this holds the object we are
    /// waiting on.
    pub event_object: Lisp_Object,

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
    pub m_re_match_object: Lisp_Object,
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

/// Lisp_Char_Table
#[repr(C)]
#[allow(dead_code)]
enum ChartabSize {
    Bits0 = 6,
    Bits1 = 4,
    Bits2 = 5,
    Bits3 = 7,
}

#[repr(C)]
pub struct Lisp_Char_Table {
    /// HEADER.SIZE is the vector's size field, which also holds the
    /// pseudovector type information.  It holds the size, too.
    /// The size counts the defalt, parent, purpose, ascii,
    /// contents, and extras slots.
    pub header: Lisp_Vectorlike_Header,

    /// This holds a default value,
    /// which is used whenever the value for a specific character is nil.
    pub default: Lisp_Object,

    /// This points to another char table, which we inherit from when the
    /// value for a specific character is nil.  The `defalt' slot takes
    /// precedence over this.
    pub parent: Lisp_Object,

    /// This is a symbol which says what kind of use this char-table is
    /// meant for.
    pub purpose: Lisp_Object,

    /// The bottom sub char-table for characters of the range 0..127.  It
    /// is nil if none of ASCII character has a specific value.
    pub ascii: Lisp_Object,

    pub contents: [Lisp_Object; 1 << ChartabSize::Bits0 as u8],

    /// These hold additional data.  It is a vector.
    // actually any number of items
    pub extras: [Lisp_Object; 1],
}

#[repr(C)]
pub struct Lisp_Process {
    /// Name of subprocess terminal.
    pub tty_name: Lisp_Object,

    /// Name of this process.
    pub name: Lisp_Object,

    /// List of command arguments that this process was run with.
    /// Is set to t for a stopped network process; nil otherwise.
    pub command: Lisp_Object,

    /// (funcall FILTER PROC STRING)  (if FILTER is non-nil)
    /// to dispose of a bunch of chars from the process all at once.
    pub filter: Lisp_Object,

    /// (funcall SENTINEL PROCESS) when process state changes.
    pub sentinel: Lisp_Object,

    /// (funcall LOG SERVER CLIENT MESSAGE) when a server process
    /// accepts a connection from a client.
    pub log: Lisp_Object,

    /// Buffer that output is going to.
    pub buffer: Lisp_Object,

    /// t if this is a real child process.  For a network or serial
    /// connection, it is a plist based on the arguments to
    /// make-network-process or make-serial-process.
    pub childp: Lisp_Object,

    /// Plist for programs to keep per-process state information, parameters, etc.
    pub plist: Lisp_Object,

    /// Marker set to end of last buffer-inserted output from this process.
    pub mark: Lisp_Object,

    /// Symbol indicating status of process.
    /// This may be a symbol: run, open, closed, listen, or failed.
    /// Or it may be a pair (connect . ADDRINFOS) where ADDRINFOS is
    /// a list of remaining (PROTOCOL . ADDRINFO) pairs to try.
    /// Or it may be (failed ERR) where ERR is an integer, string or symbol.
    /// Or it may be a list, whose car is stop, exit or signal
    /// and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
    /// or (SIGNAL_NUMBER . COREDUMP_FLAG).
    pub status: Lisp_Object,

    /// Coding-system for decoding the input from this process.
    pub decode_coding_system: Lisp_Object,

    /// Working buffer for decoding.
    pub decoding_buf: Lisp_Object,

    /// Coding-system for encoding the output to this process.
    pub encode_coding_system: Lisp_Object,

    /// Working buffer for encoding.
    pub encoding_buf: Lisp_Object,

    /// Queue for storing waiting writes.
    pub write_queue: Lisp_Object,

    // TODO: this struct is incomplete.
}

#[repr(C)]
pub struct hash_table_test {
    pub name: Lisp_Object,
    pub user_hash_function: Lisp_Object,
    pub user_cmp_function: Lisp_Object,
    pub cmpfn: extern "C" fn(t: *mut hash_table_test, a: Lisp_Object, b: Lisp_Object) -> bool,
    pub hashfn: extern "C" fn(t: *mut hash_table_test, a: Lisp_Object) -> EmacsUint,
}

#[repr(C)]
pub struct Lisp_Hash_Table {
    pub header: Lisp_Vectorlike_Header,
    pub weak: Lisp_Object,
    pub hash: Lisp_Object,
    pub next: Lisp_Object,
    pub index: Lisp_Object,
    pub count: ptrdiff_t,
    pub next_free: ptrdiff_t,
    pub pure_: bool, // pure is a reserved keyword in Rust
    pub rehash_threshold: c_float,
    pub rehash_size: c_float,
    pub key_and_value: Lisp_Object,
    pub test: hash_table_test,
    pub next_weak: *mut Lisp_Hash_Table,
}

extern "C" {
    pub static mut globals: emacs_globals;
    pub static current_thread: *mut thread_state;
    pub static Qt: Lisp_Object;
    pub static Qerror: Lisp_Object;
    pub static Qarith_error: Lisp_Object;
    pub static Qrange_error: Lisp_Object;
    pub static Qwrong_type_argument: Lisp_Object;
    pub static Qargs_out_of_range: Lisp_Object;
    pub static Qnumber_or_marker_p: Lisp_Object;
    pub static Qinteger_or_marker_p: Lisp_Object;
    pub static Qconsp: Lisp_Object;
    pub static Qnumberp: Lisp_Object;
    pub static Qintegerp: Lisp_Object;
    pub static Qfloatp: Lisp_Object;
    pub static Qstringp: Lisp_Object;
    pub static Qsymbolp: Lisp_Object;
    pub static Qlistp: Lisp_Object;
    pub static Qplistp: Lisp_Object;
    pub static Qmarkerp: Lisp_Object;
    pub static Qwholenump: Lisp_Object;
    pub static Qvectorp: Lisp_Object;
    pub static Qsequencep: Lisp_Object;
    pub static Qcharacterp: Lisp_Object;
    pub static Qchar_table_p: Lisp_Object;
    pub static Qbufferp: Lisp_Object;
    pub static Qwindowp: Lisp_Object;
    pub static Qwindow_live_p: Lisp_Object;
    pub static Qprocessp: Lisp_Object;
    pub static Qthreadp: Lisp_Object;
    pub static Qoverlayp: Lisp_Object;
    pub static Qminus: Lisp_Object;
    pub static Qmark_inactive: Lisp_Object;

    pub static Qinteger: Lisp_Object;
    pub static Qsymbol: Lisp_Object;
    pub static Qstring: Lisp_Object;
    pub static Qcons: Lisp_Object;
    pub static Qmarker: Lisp_Object;
    pub static Qoverlay: Lisp_Object;
    pub static Qfinalizer: Lisp_Object;
    pub static Quser_ptr: Lisp_Object;
    pub static Qfloat: Lisp_Object;
    pub static Qwindow_configuration: Lisp_Object;
    pub static Qprocess: Lisp_Object;
    pub static Qwindow: Lisp_Object;
    pub static Qcompiled_function: Lisp_Object;
    pub static Qbuffer: Lisp_Object;
    pub static Qframe: Lisp_Object;
    pub static Qvector: Lisp_Object;
    pub static Qchar_table: Lisp_Object;
    pub static Qcategory_table: Lisp_Object;
    pub static Qbool_vector: Lisp_Object;
    pub static Qhash_table: Lisp_Object;
    pub static Qthread: Lisp_Object;
    pub static Qmutex: Lisp_Object;
    pub static Qcondition_variable: Lisp_Object;
    pub static Qsubr: Lisp_Object;
    pub static Qfont_spec: Lisp_Object;
    pub static Qfont_entity: Lisp_Object;
    pub static Qfont_object: Lisp_Object;
    pub static Qhash_table_p: Lisp_Object;
    pub static Qwrite_region: Lisp_Object;
    pub static Qbuffer_file_coding_system: Lisp_Object;
    pub static Qfont_extra_type: Lisp_Object;
    pub static Qsetting_constant: Lisp_Object;
    pub static Qcyclic_function_indirection: Lisp_Object;

    pub static Qmd5: Lisp_Object;
    pub static Qsha1: Lisp_Object;
    pub static Qsha224: Lisp_Object;
    pub static Qsha256: Lisp_Object;
    pub static Qsha384: Lisp_Object;
    pub static Qsha512: Lisp_Object;

    pub static Qraw_text: Lisp_Object;
    pub static Qcoding_system_error: Lisp_Object;

    pub static lispsym: Lisp_Symbol;
    pub static Vbuffer_alist: Lisp_Object;
    pub static Vprocess_alist: Lisp_Object;
    pub static Vminibuffer_list: Lisp_Object;
    pub static minibuf_level: EmacsInt;
    pub static minibuf_window: Lisp_Object;
    pub static selected_window: Lisp_Object;

    pub fn Faref(array: Lisp_Object, idx: Lisp_Object) -> Lisp_Object;
    pub fn Fcons(car: Lisp_Object, cdr: Lisp_Object) -> Lisp_Object;
    pub fn Fcurrent_buffer() -> Lisp_Object;
    pub fn Fsignal(error_symbol: Lisp_Object, data: Lisp_Object) -> !;
    pub fn Fcopy_sequence(seq: Lisp_Object) -> Lisp_Object;
    pub fn Ffind_operation_coding_system(nargs: ptrdiff_t, args: *mut Lisp_Object) -> Lisp_Object;
    pub fn Flocal_variable_p(variable: Lisp_Object, buffer: Lisp_Object) -> Lisp_Object;
    pub fn Ffuncall(nargs: ptrdiff_t, args: *mut Lisp_Object) -> Lisp_Object;
    pub fn Fpurecopy(string: Lisp_Object) -> Lisp_Object;

    pub fn make_float(float_value: c_double) -> Lisp_Object;
    pub fn make_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn make_lisp_ptr(ptr: *const c_void, ty: Lisp_Type) -> Lisp_Object;
    pub fn build_string(s: *const c_char) -> Lisp_Object;
    pub fn make_unibyte_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn make_uninit_string(length: EmacsInt) -> Lisp_Object;
    pub fn make_uninit_multibyte_string(nchars: EmacsInt, nbytes: EmacsInt) -> Lisp_Object;
    pub fn make_specified_string(
        contents: *const c_char,
        nchars: ptrdiff_t,
        nbytes: ptrdiff_t,
        multibyte: bool,
    ) -> Lisp_Object;
    pub fn string_to_multibyte(string: Lisp_Object) -> Lisp_Object;

    pub fn preferred_coding_system() -> Lisp_Object;
    pub fn Fcoding_system_p(o: Lisp_Object) -> Lisp_Object;
    pub fn code_convert_string(
        string: Lisp_Object,
        coding_system: Lisp_Object,
        dst_object: Lisp_Object,
        encodep: bool,
        nocopy: bool,
        norecord: bool,
    ) -> Lisp_Object;
    pub fn validate_subarray(
        array: Lisp_Object,
        from: Lisp_Object,
        to: Lisp_Object,
        size: libc::ptrdiff_t,
        ifrom: &mut libc::ptrdiff_t,
        ito: &mut libc::ptrdiff_t,
    );
    pub fn string_char_to_byte(string: Lisp_Object, char_index: libc::ptrdiff_t)
        -> libc::ptrdiff_t;

    pub fn record_unwind_current_buffer();
    pub fn set_buffer_internal(buffer: *const libc::c_void); // TODO: buffer*
    pub fn make_buffer_string(
        start: libc::ptrdiff_t,
        end: libc::ptrdiff_t,
        props: bool,
    ) -> Lisp_Object;

    pub fn check_obarray(obarray: Lisp_Object) -> Lisp_Object;
    pub fn check_vobarray() -> Lisp_Object;
    pub fn intern_driver(
        string: Lisp_Object,
        obarray: Lisp_Object,
        index: Lisp_Object,
    ) -> Lisp_Object;
    pub fn oblookup(
        obarray: Lisp_Object,
        s: *const c_char,
        size: ptrdiff_t,
        size_bytes: ptrdiff_t,
    ) -> Lisp_Object;

    pub fn SYMBOL_NAME(s: Lisp_Object) -> Lisp_Object;
    pub fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const c_void);
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: c_int,
        ht: Lisp_Object,
    ) -> bool;

    // These signal an error, therefore are marked as non-returning.
    pub fn circular_list(tail: Lisp_Object) -> !;
    pub fn nsberror(spec: Lisp_Object) -> !;

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
        spec: Lisp_Object,
        start_byte: *mut ptrdiff_t,
        end_byte: *mut ptrdiff_t,
    ) -> *mut c_char;
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
