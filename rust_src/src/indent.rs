//! Indentation functions

use std::{cmp::max, ptr};

use remacs_macros::lisp_fn;

use crate::{
    buffers::{point_byte, point_min_byte},
    editfns::{insert_char, point, point_min},
    lisp::LispObject,
    multibyte::Codepoint,
    remacs_sys::globals,
    remacs_sys::EmacsUint,
    remacs_sys::{
        self, find_newline, position_indentation, sanitize_tab_width, scan_for_column, set_point,
        EmacsInt,
    },
    remacs_sys::{del_range, Qt},
    remacs_sys::{
        last_known_column, last_known_column_modified, last_known_column_point, set_point_both,
    },
    threads::ThreadState,
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

/// Move point to column COLUMN in the current line.
/// Interactively, COLUMN is the value of prefix numeric argument.
/// The column of a character is calculated by adding together the widths
/// as displayed of the previous characters in the line.
/// This function ignores line-continuation;
/// there is no upper limit on the column number a character can have
/// and horizontal scrolling has no effect.
///
/// If specified column is within a character, point goes after that character.
/// If it's past end of line, point goes to end of line.
///
/// Optional second argument FORCE non-nil means if COLUMN is in the
/// middle of a tab character, change it to spaces.
/// In addition, if FORCE is t, and the line is too short to reach
/// COLUMN, add spaces/tabs to get there.
///
/// The return value is the current column.
#[lisp_fn(min = "1", intspec = "NMove to column")]
pub fn move_to_column(column: EmacsUint, force: LispObject) -> EmacsUint {
    let buffer = &mut ThreadState::current_buffer_unchecked();
    let goal = column;

    let mut col = goal as i64;
    let mut pos = buffer.zv as isize;
    let mut prev_col = 0;

    unsafe {
        scan_for_column(&mut pos, &mut col, &mut prev_col);
        set_point(pos);
    }

    let mut col = col as u64;
    let prev_col = prev_col as u64;

    // If a tab char made us overshoot, change it to spaces and scan through it again
    if !force.is_nil() && col > goal {
        let pos_byte = buffer.dec_pos(buffer.pt_byte);
        let c = buffer.fetch_char(pos_byte);

        if c == '\t' as i32 && prev_col < goal {
            unsafe {
                // Insert spaces in front of the tab
                set_point_both(buffer.pt - 1, buffer.pt_byte - 1);
                insert_char(' ' as Codepoint, Some((goal - prev_col) as EmacsInt), true);

                // Delete the tab and indent to COL
                del_range(buffer.pt, buffer.pt + 1);
                let goal_pt = buffer.pt;
                let goal_pt_byte = buffer.pt_byte;
                indent_to(col as EmacsInt, None);
                set_point_both(goal_pt, goal_pt_byte);
            }

            // Set the last_known... vars consistently.
            col = goal;
        }
    }

    // If line ends prematurely, add space to the end.
    if col < goal && force == Qt {
        col = goal;
        indent_to(col as EmacsInt, None);
    }

    unsafe {
        last_known_column = col as isize;
        last_known_column_point = buffer.pt;
        last_known_column_modified = buffer.modifications();
    }

    col
}

// Cancel any recorded value of the horizontal position.
#[no_mangle]
pub extern "C" fn invalidate_current_column() {
    unsafe { last_known_column_point = 0 };
}

/// Indent from point with tabs and spaces until COLUMN is reached.
/// Optional second argument MINIMUM says always do at least MINIMUM
/// spaces even if that goes past COLUMN; by default, MINIMUM is zero.
///
/// The return value is the column where the insertion ends.
#[lisp_fn(min = "1", intspec = "NIndent to column: ")]
pub fn indent_to(column: EmacsInt, minimum: Option<EmacsInt>) -> EmacsInt {
    let buffer = ThreadState::current_buffer_unchecked();
    let tab_width = EmacsInt::from(unsafe { sanitize_tab_width(buffer.tab_width_.force_fixnum()) });
    let arg_minimum = minimum.unwrap_or(0);
    let mut fromcol = current_column();
    let mincol = max(fromcol + arg_minimum, column);

    if fromcol == mincol {
        return mincol;
    }

    if unsafe { globals.indent_tabs_mode } {
        let n = mincol / tab_width - fromcol / tab_width;
        if n != 0 {
            insert_char('\t' as Codepoint, Some(n), true);
            fromcol = (mincol / tab_width) * tab_width;
        }
    }

    let missing = mincol - fromcol;
    insert_char(' ' as Codepoint, Some(missing), true);

    unsafe {
        last_known_column = mincol as isize;
        last_known_column_point = buffer.pt;
        last_known_column_modified = buffer.modifications();
    }

    mincol
}

include!(concat!(env!("OUT_DIR"), "/indent_exports.rs"));
