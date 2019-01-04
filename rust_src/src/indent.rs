//! Indentation functions

use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    buffers::{point_byte, point_min_byte},
    editfns::{point, point_min},
    lisp::defsubr,
    remacs_sys::last_known_column_point,
    remacs_sys::EmacsInt,
    remacs_sys::{self, find_newline, position_indentation},
};

/// Return the indentation of the current line.  This is the
/// horizontal position of the character following any initial
/// whitespace.
#[lisp_fn]
pub fn current_indentation() -> EmacsInt {
    let mut posbyte = 0;
    let pos = unsafe {
        find_newline(
            point() as isize,
            point_byte() as isize,
            point_min() as isize,
            point_min_byte() as isize,
            -1,
            ptr::null_mut(),
            &mut posbyte,
            true,
        );
        position_indentation(posbyte)
    };
    pos as EmacsInt
}

/// Return the horizontal position of point.
/// Beginning of line is column 0.
/// This is calculated by adding together the widths of all the
/// displayed representations of the character between the start of
/// the previous line and point (e.g., control characters will have a
/// width of 2 or 4, tabs will have a variable width). Ignores finite
/// width of frame, which means that this function may return values
/// greater than (frame-width). Whether the line is visible (if
/// `selective-display' is t) has no effect; however, ^M is treated as
/// end of line when `selective-display' is t. Text that has an
/// invisible property is considered as having width 0, unless
/// `buffer-invisibility-spec' specifies that it is replaced by an
/// ellipsis.
#[lisp_fn]
pub fn current_column() -> EmacsInt {
    let column = unsafe { remacs_sys::current_column() };
    column as EmacsInt
}

// Cancel any recorded value of the horizontal position.
#[no_mangle]
pub extern "C" fn invalidate_current_column() {
    unsafe { last_known_column_point = 0 };
}

include!(concat!(env!("OUT_DIR"), "/indent_exports.rs"));
