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
#[macro_use]
extern crate serde_derive;
extern crate serde;

pub mod libm;

use libc::{c_char, c_uchar, c_short, c_int, c_double, c_float, c_void, ptrdiff_t, size_t, off_t,
           time_t, timespec};


include!(concat!(env!("OUT_DIR"), "/definitions.rs"));

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
#[derive(Serialize, Deserialize, Clone)]
pub struct Lisp_Vectorlike_Header {
    pub size: ptrdiff_t,
}

pub static ARRAY_MARK_FLAG: ptrdiff_t = std::isize::MIN;

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

/// Represents the global state of the editor.
///
/// This has been factored out to a single struct in C Emacs to help
/// with future threading support.
#[repr(C)]
pub struct emacs_globals {
    pub f_Vafter_change_functions: Lisp_Object,
    pub f_Vafter_init_time: Lisp_Object,
    pub f_Vafter_insert_file_functions: Lisp_Object,
    pub f_Vafter_load_alist: Lisp_Object,
    pub f_Valternate_fontname_alist: Lisp_Object,
    pub f_Vauto_composition_function: Lisp_Object,
    pub f_Vauto_composition_mode: Lisp_Object,
    pub f_Vauto_fill_chars: Lisp_Object,
    pub f_Vauto_resize_tool_bars: Lisp_Object,
    pub f_Vauto_save_include_big_deletions: Lisp_Object,
    pub f_Vauto_save_list_file_name: Lisp_Object,
    pub f_Vauto_save_timeout: Lisp_Object,
    pub f_Vauto_save_visited_file_name: Lisp_Object,
    pub f_Vbefore_change_functions: Lisp_Object,
    pub f_Vbefore_init_time: Lisp_Object,
    pub f_Vblink_cursor_alist: Lisp_Object,
    pub f_Vbuffer_access_fontified_property: Lisp_Object,
    pub f_Vbuffer_access_fontify_functions: Lisp_Object,
    pub f_Vbuffer_list_update_hook: Lisp_Object,
    pub f_Vbuild_files: Lisp_Object,
    pub f_Vbyte_boolean_vars: Lisp_Object,
    pub f_Vbyte_code_meter: Lisp_Object,
    pub f_Vbytecomp_version_regexp: Lisp_Object,
    pub f_Vcairo_version_string: Lisp_Object,
    pub f_Vchange_major_mode_hook: Lisp_Object,
    pub f_Vchar_code_property_alist: Lisp_Object,
    pub f_Vchar_property_alias_alist: Lisp_Object,
    pub f_Vchar_script_table: Lisp_Object,
    pub f_Vchar_width_table: Lisp_Object,
    pub f_Vcharset_list: Lisp_Object,
    pub f_Vcharset_map_path: Lisp_Object,
    pub f_Vcharset_revision_table: Lisp_Object,
    pub f_Vcode_conversion_map_vector: Lisp_Object,
    pub f_Vcoding_category_list: Lisp_Object,
    pub f_Vcoding_system_alist: Lisp_Object,
    pub f_Vcoding_system_for_read: Lisp_Object,
    pub f_Vcoding_system_for_write: Lisp_Object,
    pub f_Vcoding_system_list: Lisp_Object,
    pub f_Vcombine_after_change_calls: Lisp_Object,
    pub f_Vcommand_debug_status: Lisp_Object,
    pub f_Vcommand_error_function: Lisp_Object,
    pub f_Vcommand_history: Lisp_Object,
    pub f_Vcommand_line_args: Lisp_Object,
    pub f_Vcompletion_ignored_extensions: Lisp_Object,
    pub f_Vcompletion_regexp_list: Lisp_Object,
    pub f_Vcompose_chars_after_function: Lisp_Object,
    pub f_Vcomposition_function_table: Lisp_Object,
    pub f_Vconfigure_info_directory: Lisp_Object,
    pub f_Vcurrent_iso639_language: Lisp_Object,
    pub f_Vcurrent_load_list: Lisp_Object,
    pub f_Vcurrent_prefix_arg: Lisp_Object,
    pub f_Vdata_directory: Lisp_Object,
    pub f_Vdbus_compiled_version: Lisp_Object,
    pub f_Vdbus_debug: Lisp_Object,
    pub f_Vdbus_message_type_error: Lisp_Object,
    pub f_Vdbus_message_type_invalid: Lisp_Object,
    pub f_Vdbus_message_type_method_call: Lisp_Object,
    pub f_Vdbus_message_type_method_return: Lisp_Object,
    pub f_Vdbus_message_type_signal: Lisp_Object,
    pub f_Vdbus_registered_objects_table: Lisp_Object,
    pub f_Vdbus_runtime_version: Lisp_Object,
    pub f_Vdeactivate_mark: Lisp_Object,
    pub f_Vdebug_ignored_errors: Lisp_Object,
    pub f_Vdebug_on_error: Lisp_Object,
    pub f_Vdebug_on_event: Lisp_Object,
    pub f_Vdebug_on_message: Lisp_Object,
    pub f_Vdebug_on_signal: Lisp_Object,
    pub f_Vdebugger: Lisp_Object,
    pub f_Vdefault_file_name_coding_system: Lisp_Object,
    pub f_Vdefault_frame_alist: Lisp_Object,
    pub f_Vdefault_frame_scroll_bars: Lisp_Object,
    pub f_Vdefault_process_coding_system: Lisp_Object,
    pub f_Vdefault_text_properties: Lisp_Object,
    pub f_Vdeferred_action_function: Lisp_Object,
    pub f_Vdeferred_action_list: Lisp_Object,
    pub f_Vdefine_key_rebound_commands: Lisp_Object,
    pub f_Vdelayed_warnings_list: Lisp_Object,
    pub f_Vdelete_frame_functions: Lisp_Object,
    pub f_Vdelete_terminal_functions: Lisp_Object,
    pub f_Vdisable_point_adjustment: Lisp_Object,
    pub f_Vdisplay_pixels_per_inch: Lisp_Object,
    pub f_Vdoc_directory: Lisp_Object,
    pub f_Vdoc_file_name: Lisp_Object,
    pub f_Vdouble_click_time: Lisp_Object,
    pub f_Vdynamic_library_alist: Lisp_Object,
    pub f_Vecho_keystrokes: Lisp_Object,
    pub f_Vemacs_copyright: Lisp_Object,
    pub f_Vemacs_version: Lisp_Object,
    pub f_Vemulation_mode_map_alists: Lisp_Object,
    pub f_Venable_character_translation: Lisp_Object,
    pub f_Venable_disabled_menus_and_buttons: Lisp_Object,
    pub f_Veval_buffer_list: Lisp_Object,
    pub f_Vexec_directory: Lisp_Object,
    pub f_Vexec_path: Lisp_Object,
    pub f_Vexec_suffixes: Lisp_Object,
    pub f_Vexecuting_kbd_macro: Lisp_Object,
    pub f_Vface_default_stipple: Lisp_Object,
    pub f_Vface_font_rescale_alist: Lisp_Object,
    pub f_Vface_ignored_fonts: Lisp_Object,
    pub f_Vface_new_frame_defaults: Lisp_Object,
    pub f_Vface_remapping_alist: Lisp_Object,
    pub f_Vfeatures: Lisp_Object,
    pub f_Vfile_coding_system_alist: Lisp_Object,
    pub f_Vfile_name_coding_system: Lisp_Object,
    pub f_Vfile_name_handler_alist: Lisp_Object,
    pub f_Vfind_word_boundary_function_table: Lisp_Object,
    pub f_Vfirst_change_hook: Lisp_Object,
    pub f_Vfloat_output_format: Lisp_Object,
    pub f_Vfocus_in_hook: Lisp_Object,
    pub f_Vfocus_out_hook: Lisp_Object,
    pub f_Vfont_ccl_encoder_alist: Lisp_Object,
    pub f_Vfont_encoding_alist: Lisp_Object,
    pub f_Vfont_encoding_charset_alist: Lisp_Object,
    pub f_Vfont_log: Lisp_Object,
    pub f_Vfont_slant_table: Lisp_Object,
    pub f_Vfont_weight_table: Lisp_Object,
    pub f_Vfont_width_table: Lisp_Object,
    pub f_Vfontification_functions: Lisp_Object,
    pub f_Vfontset_alias_alist: Lisp_Object,
    pub f_Vframe_alpha_lower_limit: Lisp_Object,
    pub f_Vframe_title_format: Lisp_Object,
    pub f_Vfringe_bitmaps: Lisp_Object,
    pub f_Vfunction_key_map: Lisp_Object,
    pub f_Vgc_cons_percentage: Lisp_Object,
    pub f_Vgc_elapsed: Lisp_Object,
    pub f_Vglobal_disable_point_adjustment: Lisp_Object,
    pub f_Vglobal_mode_string: Lisp_Object,
    pub f_Vglyph_table: Lisp_Object,
    pub f_Vglyphless_char_display: Lisp_Object,
    pub f_Vgtk_version_string: Lisp_Object,
    pub f_Vhelp_char: Lisp_Object,
    pub f_Vhelp_event_list: Lisp_Object,
    pub f_Vhelp_form: Lisp_Object,
    pub f_Vhistory_add_new_input: Lisp_Object,
    pub f_Vhistory_length: Lisp_Object,
    pub f_Vhourglass_delay: Lisp_Object,
    pub f_Vhscroll_step: Lisp_Object,
    pub f_Vicon_title_format: Lisp_Object,
    pub f_Vignore_relative_composition: Lisp_Object,
    pub f_Vimage_cache_eviction_delay: Lisp_Object,
    pub f_Vimage_types: Lisp_Object,
    pub f_Vinhibit_changing_match_data: Lisp_Object,
    pub f_Vinhibit_debugger: Lisp_Object,
    pub f_Vinhibit_field_text_motion: Lisp_Object,
    pub f_Vinhibit_file_name_handlers: Lisp_Object,
    pub f_Vinhibit_file_name_operation: Lisp_Object,
    pub f_Vinhibit_point_motion_hooks: Lisp_Object,
    pub f_Vinhibit_quit: Lisp_Object,
    pub f_Vinhibit_read_only: Lisp_Object,
    pub f_Vinhibit_redisplay: Lisp_Object,
    pub f_Vinitial_environment: Lisp_Object,
    pub f_Vinitial_window_system: Lisp_Object,
    pub f_Vinput_method_function: Lisp_Object,
    pub f_Vinput_method_previous_message: Lisp_Object,
    pub f_Vinstallation_directory: Lisp_Object,
    pub f_Vinternal__top_level_message: Lisp_Object,
    pub f_Vinternal_interpreter_environment: Lisp_Object,
    pub f_Vinvocation_directory: Lisp_Object,
    pub f_Vinvocation_name: Lisp_Object,
    pub f_Vkbd_macro_termination_hook: Lisp_Object,
    pub f_Vkey_translation_map: Lisp_Object,
    pub f_Vkill_buffer_query_functions: Lisp_Object,
    pub f_Vkill_emacs_hook: Lisp_Object,
    pub f_Vlast_code_conversion_error: Lisp_Object,
    pub f_Vlast_coding_system_used: Lisp_Object,
    pub f_Vlast_event_frame: Lisp_Object,
    pub f_Vlatin_extra_code_table: Lisp_Object,
    pub f_Vlexical_binding: Lisp_Object,
    pub f_Vline_number_display_limit: Lisp_Object,
    pub f_Vline_prefix: Lisp_Object,
    pub f_Vload_file_name: Lisp_Object,
    pub f_Vload_file_rep_suffixes: Lisp_Object,
    pub f_Vload_history: Lisp_Object,
    pub f_Vload_path: Lisp_Object,
    pub f_Vload_read_function: Lisp_Object,
    pub f_Vload_source_file_function: Lisp_Object,
    pub f_Vload_suffixes: Lisp_Object,
    pub f_Vlocale_coding_system: Lisp_Object,
    pub f_Vlucid_menu_bar_dirty_flag: Lisp_Object,
    pub f_Vmake_pointer_invisible: Lisp_Object,
    pub f_Vmark_even_if_inactive: Lisp_Object,
    pub f_Vmax_image_size: Lisp_Object,
    pub f_Vmax_mini_window_height: Lisp_Object,
    pub f_Vmemory_full: Lisp_Object,
    pub f_Vmemory_signal_data: Lisp_Object,
    pub f_Vmenu_bar_final_items: Lisp_Object,
    pub f_Vmenu_bar_mode: Lisp_Object,
    pub f_Vmenu_bar_update_hook: Lisp_Object,
    pub f_Vmenu_updating_frame: Lisp_Object,
    pub f_Vmessage_log_max: Lisp_Object,
    pub f_Vminibuf_scroll_window: Lisp_Object,
    pub f_Vminibuffer_completing_file_name: Lisp_Object,
    pub f_Vminibuffer_completion_confirm: Lisp_Object,
    pub f_Vminibuffer_completion_predicate: Lisp_Object,
    pub f_Vminibuffer_completion_table: Lisp_Object,
    pub f_Vminibuffer_exit_hook: Lisp_Object,
    pub f_Vminibuffer_help_form: Lisp_Object,
    pub f_Vminibuffer_history_position: Lisp_Object,
    pub f_Vminibuffer_history_variable: Lisp_Object,
    pub f_Vminibuffer_local_map: Lisp_Object,
    pub f_Vminibuffer_local_ns_map: Lisp_Object,
    pub f_Vminibuffer_message_timeout: Lisp_Object,
    pub f_Vminibuffer_prompt_properties: Lisp_Object,
    pub f_Vminibuffer_setup_hook: Lisp_Object,
    pub f_Vminor_mode_map_alist: Lisp_Object,
    pub f_Vminor_mode_overriding_map_alist: Lisp_Object,
    pub f_Vmodule_file_suffix: Lisp_Object,
    pub f_Vmost_negative_fixnum: Lisp_Object,
    pub f_Vmost_positive_fixnum: Lisp_Object,
    pub f_Vmotif_version_string: Lisp_Object,
    pub f_Vmouse_autoselect_window: Lisp_Object,
    pub f_Vmouse_highlight: Lisp_Object,
    pub f_Vmouse_leave_buffer_hook: Lisp_Object,
    pub f_Vmouse_position_function: Lisp_Object,
    pub f_Vnetwork_coding_system_alist: Lisp_Object,
    pub f_Vnobreak_char_display: Lisp_Object,
    pub f_Vobarray: Lisp_Object,
    pub f_Vold_style_backquotes: Lisp_Object,
    pub f_Voperating_system_release: Lisp_Object,
    pub f_Votf_script_alist: Lisp_Object,
    pub f_Vother_window_scroll_buffer: Lisp_Object,
    pub f_Voverflow_newline_into_fringe: Lisp_Object,
    pub f_Voverlay_arrow_position: Lisp_Object,
    pub f_Voverlay_arrow_string: Lisp_Object,
    pub f_Voverlay_arrow_variable_list: Lisp_Object,
    pub f_Voverriding_local_map: Lisp_Object,
    pub f_Voverriding_local_map_menu_flag: Lisp_Object,
    pub f_Vpath_separator: Lisp_Object,
    pub f_Vpost_command_hook: Lisp_Object,
    pub f_Vpost_gc_hook: Lisp_Object,
    pub f_Vpost_self_insert_hook: Lisp_Object,
    pub f_Vpre_command_hook: Lisp_Object,
    pub f_Vpre_redisplay_function: Lisp_Object,
    pub f_Vprefix_help_command: Lisp_Object,
    pub f_Vpreloaded_file_list: Lisp_Object,
    pub f_Vprevious_system_messages_locale: Lisp_Object,
    pub f_Vprevious_system_time_locale: Lisp_Object,
    pub f_Vprint_charset_text_property: Lisp_Object,
    pub f_Vprint_circle: Lisp_Object,
    pub f_Vprint_continuous_numbering: Lisp_Object,
    pub f_Vprint_gensym: Lisp_Object,
    pub f_Vprint_length: Lisp_Object,
    pub f_Vprint_level: Lisp_Object,
    pub f_Vprint_number_table: Lisp_Object,
    pub f_Vprintable_chars: Lisp_Object,
    pub f_Vprocess_adaptive_read_buffering: Lisp_Object,
    pub f_Vprocess_coding_system_alist: Lisp_Object,
    pub f_Vprocess_connection_type: Lisp_Object,
    pub f_Vprocess_environment: Lisp_Object,
    pub f_Vpurify_flag: Lisp_Object,
    pub f_Vquit_flag: Lisp_Object,
    pub f_Vread_buffer_function: Lisp_Object,
    pub f_Vread_circle: Lisp_Object,
    pub f_Vread_expression_history: Lisp_Object,
    pub f_Vread_hide_char: Lisp_Object,
    pub f_Vread_symbol_positions_list: Lisp_Object,
    pub f_Vread_with_symbol_positions: Lisp_Object,
    pub f_Vreal_this_command: Lisp_Object,
    pub f_Vrecenter_redisplay: Lisp_Object,
    pub f_Vredisplay__all_windows_cause: Lisp_Object,
    pub f_Vredisplay__mode_lines_cause: Lisp_Object,
    pub f_Vredisplay__variables: Lisp_Object,
    pub f_Vredisplay_end_trigger_functions: Lisp_Object,
    pub f_Vreport_emacs_bug_address: Lisp_Object,
    pub f_Vresize_mini_windows: Lisp_Object,
    pub f_Vresume_tty_functions: Lisp_Object,
    pub f_Vring_bell_function: Lisp_Object,
    pub f_Vsaved_region_selection: Lisp_Object,
    pub f_Vscalable_fonts_allowed: Lisp_Object,
    pub f_Vscript_representative_chars: Lisp_Object,
    pub f_Vscroll_preserve_screen_position: Lisp_Object,
    pub f_Vsearch_spaces_regexp: Lisp_Object,
    pub f_Vselect_active_regions: Lisp_Object,
    pub f_Vselect_safe_coding_system_function: Lisp_Object,
    pub f_Vselection_converter_alist: Lisp_Object,
    pub f_Vselection_inhibit_update_commands: Lisp_Object,
    pub f_Vset_auto_coding_function: Lisp_Object,
    pub f_Vshared_game_score_directory: Lisp_Object,
    pub f_Vshell_file_name: Lisp_Object,
    pub f_Vshow_help_function: Lisp_Object,
    pub f_Vshow_trailing_whitespace: Lisp_Object,
    pub f_Vsignal_hook_function: Lisp_Object,
    pub f_Vsource_directory: Lisp_Object,
    pub f_Vspecial_event_map: Lisp_Object,
    pub f_Vstandard_display_table: Lisp_Object,
    pub f_Vstandard_input: Lisp_Object,
    pub f_Vstandard_output: Lisp_Object,
    pub f_Vstandard_translation_table_for_decode: Lisp_Object,
    pub f_Vstandard_translation_table_for_encode: Lisp_Object,
    pub f_Vsuspend_tty_functions: Lisp_Object,
    pub f_Vsystem_configuration: Lisp_Object,
    pub f_Vsystem_configuration_features: Lisp_Object,
    pub f_Vsystem_configuration_options: Lisp_Object,
    pub f_Vsystem_messages_locale: Lisp_Object,
    pub f_Vsystem_name: Lisp_Object,
    pub f_Vsystem_time_locale: Lisp_Object,
    pub f_Vsystem_type: Lisp_Object,
    pub f_Vtemp_buffer_show_function: Lisp_Object,
    pub f_Vtemporary_file_directory: Lisp_Object,
    pub f_Vterminal_frame: Lisp_Object,
    pub f_Vtext_property_default_nonsticky: Lisp_Object,
    pub f_Vtext_quoting_style: Lisp_Object,
    pub f_Vthis_command: Lisp_Object,
    pub f_Vthis_command_keys_shift_translated: Lisp_Object,
    pub f_Vthis_original_command: Lisp_Object,
    pub f_Vthrow_on_input: Lisp_Object,
    pub f_Vtimer_idle_list: Lisp_Object,
    pub f_Vtimer_list: Lisp_Object,
    pub f_Vtool_bar_border: Lisp_Object,
    pub f_Vtool_bar_button_margin: Lisp_Object,
    pub f_Vtool_bar_mode: Lisp_Object,
    pub f_Vtool_bar_separator_image_expression: Lisp_Object,
    pub f_Vtool_bar_style: Lisp_Object,
    pub f_Vtop_level: Lisp_Object,
    pub f_Vtransient_mark_mode: Lisp_Object,
    pub f_Vtranslation_hash_table_vector: Lisp_Object,
    pub f_Vtranslation_table_for_input: Lisp_Object,
    pub f_Vtranslation_table_vector: Lisp_Object,
    pub f_Vtruncate_partial_width_windows: Lisp_Object,
    pub f_Vtty_defined_color_alist: Lisp_Object,
    pub f_Vtty_erase_char: Lisp_Object,
    pub f_Vundo_outer_limit: Lisp_Object,
    pub f_Vundo_outer_limit_function: Lisp_Object,
    pub f_Vunicode_category_table: Lisp_Object,
    pub f_Vunread_command_events: Lisp_Object,
    pub f_Vunread_input_method_events: Lisp_Object,
    pub f_Vunread_post_input_method_events: Lisp_Object,
    pub f_Vuse_default_ascent: Lisp_Object,
    pub f_Vuser_full_name: Lisp_Object,
    pub f_Vuser_init_file: Lisp_Object,
    pub f_Vuser_login_name: Lisp_Object,
    pub f_Vuser_real_login_name: Lisp_Object,
    pub f_Vvalues: Lisp_Object,
    pub f_Vvertical_centering_font_regexp: Lisp_Object,
    pub f_Vvoid_text_area_pointer: Lisp_Object,
    pub f_Vwhere_is_preferred_modifier: Lisp_Object,
    pub f_Vwindow_combination_limit: Lisp_Object,
    pub f_Vwindow_combination_resize: Lisp_Object,
    pub f_Vwindow_configuration_change_hook: Lisp_Object,
    pub f_Vwindow_persistent_parameters: Lisp_Object,
    pub f_Vwindow_point_insertion_type: Lisp_Object,
    pub f_Vwindow_scroll_functions: Lisp_Object,
    pub f_Vwindow_size_change_functions: Lisp_Object,
    pub f_Vwindow_system_version: Lisp_Object,
    pub f_Vwindow_text_change_functions: Lisp_Object,
    pub f_Vword_combining_categories: Lisp_Object,
    pub f_Vword_separating_categories: Lisp_Object,
    pub f_Vwrap_prefix: Lisp_Object,
    pub f_Vwrite_region_annotate_functions: Lisp_Object,
    pub f_Vwrite_region_annotations_so_far: Lisp_Object,
    pub f_Vwrite_region_post_annotation_function: Lisp_Object,
    pub f_Vx_alt_keysym: Lisp_Object,
    pub f_Vx_bitmap_file_path: Lisp_Object,
    pub f_Vx_cursor_fore_pixel: Lisp_Object,
    pub f_Vx_hourglass_pointer_shape: Lisp_Object,
    pub f_Vx_hyper_keysym: Lisp_Object,
    pub f_Vx_keysym_table: Lisp_Object,
    pub f_Vx_lost_selection_functions: Lisp_Object,
    pub f_Vx_max_tooltip_size: Lisp_Object,
    pub f_Vx_meta_keysym: Lisp_Object,
    pub f_Vx_mode_pointer_shape: Lisp_Object,
    pub f_Vx_no_window_manager: Lisp_Object,
    pub f_Vx_nontext_pointer_shape: Lisp_Object,
    pub f_Vx_pixel_size_width_font_regexp: Lisp_Object,
    pub f_Vx_pointer_shape: Lisp_Object,
    pub f_Vx_resource_class: Lisp_Object,
    pub f_Vx_resource_name: Lisp_Object,
    pub f_Vx_select_enable_clipboard_manager: Lisp_Object,
    pub f_Vx_sensitive_text_pointer_shape: Lisp_Object,
    pub f_Vx_sent_selection_functions: Lisp_Object,
    pub f_Vx_session_id: Lisp_Object,
    pub f_Vx_session_previous_id: Lisp_Object,
    pub f_Vx_super_keysym: Lisp_Object,
    pub f_Vx_toolkit_scroll_bars: Lisp_Object,
    pub f_Vx_window_horizontal_drag_shape: Lisp_Object,
    pub f_Vx_window_vertical_drag_shape: Lisp_Object,
    pub f_Vxft_settings: Lisp_Object,
    pub f_do_mouse_tracking: Lisp_Object,
    pub f_eol_mnemonic_dos: Lisp_Object,
    pub f_eol_mnemonic_mac: Lisp_Object,
    pub f_eol_mnemonic_undecided: Lisp_Object,
    pub f_eol_mnemonic_unix: Lisp_Object,
    pub f_frame_inhibit_implied_resize: Lisp_Object,
    pub f_frame_size_history: Lisp_Object,
    pub f_last_command_event: Lisp_Object,
    pub f_last_input_event: Lisp_Object,
    pub f_last_nonmenu_event: Lisp_Object,
    pub f_menu_prompt_more_char: Lisp_Object,
    pub f_meta_prefix_char: Lisp_Object,
    pub f_auto_save_interval: EmacsInt,
    pub f_baud_rate: EmacsInt,
    pub f_cons_cells_consed: EmacsInt,
    pub f_debug_end_pos: EmacsInt,
    pub f_double_click_fuzz: EmacsInt,
    pub f_emacs_scroll_step: EmacsInt,
    pub f_executing_kbd_macro_index: EmacsInt,
    pub f_extra_keyboard_modifiers: EmacsInt,
    pub f_floats_consed: EmacsInt,
    pub f_gc_cons_threshold: EmacsInt,
    pub f_gcs_done: EmacsInt,
    pub f_global_gnutls_log_level: EmacsInt,
    pub f_hscroll_margin: EmacsInt,
    pub f_imagemagick_render_type: EmacsInt,
    pub f_intervals_consed: EmacsInt,
    pub f_line_number_display_limit_width: EmacsInt,
    pub f_max_lisp_eval_depth: EmacsInt,
    pub f_max_specpdl_size: EmacsInt,
    pub f_misc_objects_consed: EmacsInt,
    pub f_next_screen_context_lines: EmacsInt,
    pub f_num_input_keys: EmacsInt,
    pub f_num_nonmacro_input_events: EmacsInt,
    pub f_overline_margin: EmacsInt,
    pub f_polling_period: EmacsInt,
    pub f_profiler_log_size: EmacsInt,
    pub f_profiler_max_stack_depth: EmacsInt,
    pub f_pure_bytes_used: EmacsInt,
    pub f_scroll_conservatively: EmacsInt,
    pub f_scroll_margin: EmacsInt,
    pub f_string_chars_consed: EmacsInt,
    pub f_strings_consed: EmacsInt,
    pub f_symbols_consed: EmacsInt,
    pub f_syntax_propertize__done: EmacsInt,
    pub f_tool_bar_button_relief: EmacsInt,
    pub f_tool_bar_max_label_size: EmacsInt,
    pub f_underline_minimum_offset: EmacsInt,
    pub f_undo_limit: EmacsInt,
    pub f_undo_strong_limit: EmacsInt,
    pub f_vector_cells_consed: EmacsInt,
    pub f_x_selection_timeout: EmacsInt,
    // TODO: Change these bools to something more FFI friendly.
    pub f_Vcomment_end_can_be_escaped: bool,
    pub f_Vfast_but_imprecise_scrolling: bool,
    pub f_auto_raise_tool_bar_buttons_p: bool,
    pub f_auto_window_vscroll_p: bool,
    pub f_automatic_hscrolling_p: bool,
    pub f_byte_metering_on: bool,
    pub f_cannot_suspend: bool,
    pub f_coding_system_require_warning: bool,
    pub f_completion_ignore_case: bool,
    pub f_create_lockfiles: bool,
    pub f_cross_disabled_images: bool,
    pub f_cursor_in_echo_area: bool,
    pub f_debug_on_next_call: bool,
    pub f_debug_on_quit: bool,
    pub f_debugger_may_continue: bool,
    pub f_delete_by_moving_to_trash: bool,
    pub f_delete_exited_processes: bool,
    pub f_disable_ascii_optimization: bool,
    pub f_display_hourglass_p: bool,
    pub f_enable_recursive_minibuffers: bool,
    pub f_focus_follows_mouse: bool,
    pub f_force_load_messages: bool,
    pub f_frame_resize_pixelwise: bool,
    pub f_garbage_collection_messages: bool,
    pub f_highlight_nonselected_windows: bool,
    pub f_history_delete_duplicates: bool,
    pub f_indent_tabs_mode: bool,
    pub f_inherit_process_coding_system: bool,
    pub f_inhibit_bidi_mirroring: bool,
    pub f_inhibit_eol_conversion: bool,
    pub f_inhibit_eval_during_redisplay: bool,
    pub f_inhibit_free_realized_faces: bool,
    pub f_inhibit_iso_escape_detection: bool,
    pub f_inhibit_load_charset_map: bool,
    pub f_inhibit_menubar_update: bool,
    pub f_inhibit_message: bool,
    pub f_inhibit_modification_hooks: bool,
    pub f_inhibit_null_byte_detection: bool,
    pub f_inhibit_try_cursor_movement: bool,
    pub f_inhibit_try_window_id: bool,
    pub f_inhibit_try_window_reusing: bool,
    pub f_inhibit_x_resources: bool,
    pub f_inverse_video: bool,
    pub f_load_convert_to_unibyte: bool,
    pub f_load_dangerous_libraries: bool,
    pub f_load_force_doc_strings: bool,
    pub f_load_in_progress: bool,
    pub f_load_prefer_newer: bool,
    pub f_make_cursor_line_fully_visible_p: bool,
    pub f_menu_prompting: bool,
    pub f_message_truncate_lines: bool,
    pub f_minibuffer_allow_text_properties: bool,
    pub f_minibuffer_auto_raise: bool,
    pub f_mode_line_in_non_selected_windows: bool,
    pub f_multibyte_syntax_as_symbol: bool,
    pub f_multiple_frames: bool,
    pub f_no_redraw_on_reenter: bool,
    pub f_noninteractive1: bool,
    pub f_open_paren_in_column_0_is_defun_start: bool,
    pub f_parse_sexp_ignore_comments: bool,
    pub f_parse_sexp_lookup_properties: bool,
    pub f_print_escape_multibyte: bool,
    pub f_print_escape_newlines: bool,
    pub f_print_escape_nonascii: bool,
    pub f_print_quoted: bool,
    pub f_read_buffer_completion_ignore_case: bool,
    pub f_redisplay_dont_pause: bool,
    pub f_scroll_bar_adjust_thumb_portion_p: bool,
    pub f_system_uses_terminfo: bool,
    pub f_text_quoting_flag: bool,
    pub f_undo_inhibit_record_point: bool,
    pub f_unibyte_display_via_language_environment: bool,
    pub f_use_dialog_box: bool,
    pub f_use_file_dialog: bool,
    pub f_use_system_font: bool,
    pub f_visible_bell: bool,
    pub f_visible_cursor: bool,
    pub f_window_resize_pixelwise: bool,
    pub f_words_include_escapes: bool,
    pub f_write_region_inhibit_fsync: bool,
    pub f_x_frame_normalize_before_maximize: bool,
    pub f_x_gtk_file_dialog_help_text: bool,
    pub f_x_gtk_show_hidden_files: bool,
    pub f_x_gtk_use_old_file_dialog: bool,
    pub f_x_gtk_use_system_tooltips: bool,
    pub f_x_mouse_click_focus_ignore_position: bool,
    pub f_x_stretch_cursor_p: bool,
    pub f_x_underline_at_descent_line: bool,
    pub f_x_use_underline_position_properties: bool,
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
    pub static Qt: Lisp_Object;
    pub static Qerror: Lisp_Object;
    pub static Qarith_error: Lisp_Object;
    pub static Qrange_error: Lisp_Object;
    pub static Qwrong_type_argument: Lisp_Object;
    pub static Qnumber_or_marker_p: Lisp_Object;
    pub static Qinteger_or_marker_p: Lisp_Object;
    pub static Qconsp: Lisp_Object;
    pub static Qnumberp: Lisp_Object;
    pub static Qintegerp: Lisp_Object;
    pub static Qfloatp: Lisp_Object;
    pub static Qstringp: Lisp_Object;
    pub static Qsymbolp: Lisp_Object;
    pub static Qlistp: Lisp_Object;
    pub static Qmarkerp: Lisp_Object;
    pub static Qwholenump: Lisp_Object;
    pub static Qvectorp: Lisp_Object;
    pub static Qsequencep: Lisp_Object;
    pub static Qcharacterp: Lisp_Object;
    pub static Qminus: Lisp_Object;

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
    pub static Qhash_table_test: Lisp_Object;
    pub static QCtest: Lisp_Object;
    pub static Qeql: Lisp_Object;
    pub static Qeq: Lisp_Object;
    pub static Qequal: Lisp_Object;
    pub static QCpurecopy: Lisp_Object;
    pub static QCsize: Lisp_Object;
    pub static QCweakness: Lisp_Object;
    pub static lispsym: Lisp_Symbol;
    pub static Vbuffer_alist: Lisp_Object;
    pub static Vprocess_alist: Lisp_Object;

    pub fn Fcons(car: Lisp_Object, cdr: Lisp_Object) -> Lisp_Object;
    pub fn Fcurrent_buffer() -> Lisp_Object;
    pub fn Fsignal(error_symbol: Lisp_Object, data: Lisp_Object) -> !;
    pub fn Fcopy_sequence(seq: Lisp_Object) -> Lisp_Object;
    pub fn Ffuncall(nargs: ptrdiff_t, args: *mut Lisp_Object) -> Lisp_Object;

    pub fn make_float(float_value: c_double) -> Lisp_Object;
    pub fn make_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn build_string(s: *const c_char) -> Lisp_Object;
    pub fn make_unibyte_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn make_uninit_string(length: EmacsInt) -> Lisp_Object;
    pub fn make_uninit_multibyte_string(nchars: EmacsInt, nbytes: EmacsInt) -> Lisp_Object;
    pub fn string_to_multibyte(string: Lisp_Object) -> Lisp_Object;

    pub fn intern_1(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;

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

    pub fn sxhash(ht: *mut hash_table_test, key: Lisp_Object) -> EmacsUint;
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
