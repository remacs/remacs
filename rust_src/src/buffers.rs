//! Functions operating on buffers.

use libc::{c_void, c_int, c_uchar, ptrdiff_t, off_t, time_t, timespec};

use lisp::{LispObject, ExternalPtr};
use vectors::Lisp_Vectorlike_Header;

use remacs_sys::EmacsInt;

pub const BEG_BYTE: ptrdiff_t = 1;

/// Represents an Emacs buffer. For documentation see struct buffer in
/// buffer.h.
#[repr(C)]
#[allow(non_camel_case_types)]
pub struct Lisp_Buffer {
    header: Lisp_Vectorlike_Header,
    name: LispObject,
    filename: LispObject,
    directory: LispObject,
    backed_up: LispObject,
    save_length: LispObject,
    auto_save_file_name: LispObject,
    read_only: LispObject,
    mark: LispObject,
    local_var_alist: LispObject,
    major_mode: LispObject,
    mode_name: LispObject,
    mode_line_format: LispObject,
    header_line_format: LispObject,
    keymap: LispObject,
    abbrev_table: LispObject,
    syntax_table: LispObject,
    category_table: LispObject,
    case_fold_search: LispObject,
    tab_width: LispObject,
    fill_column: LispObject,
    left_margin: LispObject,
    auto_fill_function: LispObject,
    downcase_table: LispObject,
    upcase_table: LispObject,
    case_canon_table: LispObject,
    case_eqv_table: LispObject,
    truncate_lines: LispObject,
    word_wrap: LispObject,
    ctl_arrow: LispObject,
    bidi_display_reordering: LispObject,
    bidi_paragraph_direction: LispObject,
    selective_display: LispObject,
    selective_display_ellipses: LispObject,
    minor_modes: LispObject,
    overwrite_mode: LispObject,
    abbrev_mode: LispObject,
    display_table: LispObject,
    mark_active: LispObject,
    enable_multibyte_characters: LispObject,
    buffer_file_coding_system: LispObject,
    file_format: LispObject,
    auto_save_file_format: LispObject,
    cache_long_scans: LispObject,
    width_table: LispObject,
    pt_marker: LispObject,
    begv_marker: LispObject,
    zv_marker: LispObject,
    point_before_scroll: LispObject,
    file_truename: LispObject,
    invisibility_spec: LispObject,
    last_selected_window: LispObject,
    display_count: LispObject,
    left_margin_cols: LispObject,
    right_margin_cols: LispObject,
    left_fringe_width: LispObject,
    right_fringe_width: LispObject,
    fringes_outside_margins: LispObject,
    scroll_bar_width: LispObject,
    scroll_bar_height: LispObject,
    vertical_scroll_bar_type: LispObject,
    horizontal_scroll_bar_type: LispObject,
    indicate_empty_lines: LispObject,
    indicate_buffer_boundaries: LispObject,
    fringe_indicator_alist: LispObject,
    fringe_cursor_alist: LispObject,
    display_time: LispObject,
    scroll_up_aggressively: LispObject,
    scroll_down_aggressively: LispObject,
    cursor_type: LispObject,
    extra_line_spacing: LispObject,
    cursor_in_non_selected_windows: LispObject,

    own_text: Lisp_Buffer_Text,
    text: *mut Lisp_Buffer_Text,
    next: *mut Lisp_Buffer,

    pt: ptrdiff_t,
    pt_byte: ptrdiff_t,
    begv: ptrdiff_t,
    begv_byte: ptrdiff_t,
    zv: ptrdiff_t,
    zv_byte: ptrdiff_t,

    base_buffer: *mut Lisp_Buffer,
    indirections: c_int,
    window_count: c_int,
    local_flags: [c_uchar; 50],

    modtime: timespec,
    modtime_size: off_t,
    auto_save_modified: EmacsInt,
    display_error_modiff: EmacsInt,
    auto_save_failure_time: time_t,

    last_window_start: ptrdiff_t,
    newline_cache: *mut c_void,
    width_run_cache: *mut c_void,
    bidi_paragraph_cache: *mut c_void,

    // XXX in C, bitfield with two bools
    flags: u8,

    overlays_before: *mut c_void,
    overlays_after: *mut c_void,
    overlay_center: ptrdiff_t,

    undo_list: LispObject,
}

/// Represents text contents of an Emacs buffer. For documentation see
/// struct buffer_text in buffer.h.
#[repr(C)]
#[allow(non_camel_case_types)]
pub struct Lisp_Buffer_Text {
    beg: *mut c_uchar,

    gpt: ptrdiff_t,
    z: ptrdiff_t,
    gpt_byte: ptrdiff_t,
    z_byte: ptrdiff_t,
    gap_size: ptrdiff_t,

    modiff: EmacsInt,
    chars_modiff: EmacsInt,
    save_modiff: EmacsInt,
    overlay_modiff: EmacsInt,
    compact: EmacsInt,

    beg_unchanged: ptrdiff_t,
    end_unchanged: ptrdiff_t,

    unchanged_modified: EmacsInt,
    overlay_unchanged_modified: EmacsInt,
    // until we define struct interval
    intervals: *mut c_void,
    // until we define struct Lisp_Marker
    markers: *mut c_void,

    // XXX: in Emacs, a bitfield of 2 booleans
    flags: u8,
}

pub type LispBufferRef = ExternalPtr<Lisp_Buffer>;

impl LispBufferRef {
    #[inline]
    pub fn beg_addr(&self) -> *mut c_uchar {
        unsafe { (*self.text).beg }
    }

    #[inline]
    pub fn beg_byte(&self) -> ptrdiff_t {
        BEG_BYTE
    }

    #[inline]
    pub fn gpt_byte(&self) -> ptrdiff_t {
        unsafe { (*self.text).gpt_byte }
    }

    #[inline]
    pub fn gap_end_addr(&self) -> *mut c_uchar {
        unsafe {
            (*self.text).beg.offset(
                (*self.text).gpt_byte + (*self.text).gap_size -
                    BEG_BYTE,
            )
        }
    }

    #[inline]
    pub fn z_addr(&self) -> *mut c_uchar {
        unsafe {
            (*self.text).beg.offset(
                (*self.text).gap_size + (*self.text).z_byte -
                    BEG_BYTE,
            )
        }
    }

    #[inline]
    pub fn z_byte(&self) -> ptrdiff_t {
        unsafe { (*self.text).z_byte }
    }
}
