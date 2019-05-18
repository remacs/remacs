//! Indentation functions

use std::{cmp::max, ptr};

use remacs_macros::lisp_fn;

use crate::{
    buffers::{point_byte, point_min_byte},
    editfns::{insert_char, point, point_min},
    lisp::LispObject,
    lists::LispCons,
    marker::buf_charpos_to_bytepos,
    multibyte::Codepoint,
    numbers::LispNumber,
    remacs_sys::Qt,
    remacs_sys::{
        self, del_range, find_newline, position_indentation, sanitize_tab_width, scan_for_column,
        set_point, set_point_both,
    },
    remacs_sys::{globals, last_known_column, last_known_column_modified, last_known_column_point},
    remacs_sys::{EmacsInt, EmacsUint},
    threads::ThreadState,
    windows::{LispWindowLiveOrSelected, LispWindowRef},
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
                insert_char(
                    Codepoint::from(' '),
                    Some((goal - prev_col) as EmacsInt),
                    true,
                );

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
            insert_char(Codepoint::from('\t'), Some(n), true);
            fromcol = (mincol / tab_width) * tab_width;
        }
    }

    let missing = mincol - fromcol;
    insert_char(Codepoint::from(' '), Some(missing), true);

    unsafe {
        last_known_column = mincol as isize;
        last_known_column_point = buffer.pt;
        last_known_column_modified = buffer.modifications();
    }

    mincol
}

/// Scan through the current buffer, calculating screen position.
/// Scan the current buffer forward from offset FROM, assuming it is
/// at position FROMPOS--a cons of the form (HPOS . VPOS)-- to
/// position TO or position TOPOS--another cons of the form (HPOS
/// . VPOS)-- and return the ending buffer position and screen
/// location.
///
/// If TOPOS is nil, the actual width and height of the window's text
/// area are used.
///
/// There are three additional arguments:
///
/// WIDTH is the number of columns available to display text; this
/// affects handling of continuation lines.  A value of nil
/// corresponds to the actual number of available text columns.
///
/// OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).
/// HSCROLL is the number of columns not being displayed at the left
/// margin; this is usually taken from a window's hscroll member.
/// TAB-OFFSET is the number of columns of the first tab that aren't
/// being displayed, perhaps because the line was continued within it.
/// If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.
///
/// WINDOW is the window to operate on.  It is used to choose the
/// display table; if it is showing the current buffer, it is used
/// also for deciding which overlay properties apply.  Note that
/// `compute-motion' always operates on the current buffer.
///
/// The value is a list of five elements:
///   (POS HPOS VPOS PREVHPOS CONTIN)
/// POS is the buffer position where the scan stopped.
/// VPOS is the vertical position where the scan stopped.
/// HPOS is the horizontal position where the scan stopped.
///
/// PREVHPOS is the horizontal position one character back from POS.
/// CONTIN is t if a line was continued after (or within) the previous character.
///
/// For example, to find the buffer position of column COL of line
/// LINE of a certain window, pass the window's starting location as
/// FROM and the window's upper-left coordinates as FROMPOS.  Pass the
/// buffer's (point-max) as TO, to limit the scan to the end of the
/// visible section of the buffer, and pass LINE and COL as TOPOS.
#[lisp_fn]
pub fn compute_motion(
    from: LispNumber,
    frompos: LispCons,
    to: LispNumber,
    topos: LispObject,
    width: Option<EmacsInt>,
    offsets: LispObject,
    window: LispWindowLiveOrSelected,
) -> LispObject {
    let from_hpos = frompos.car().into();
    let from_vpos = frompos.cdr().into();

    let mut win: LispWindowRef = window.into();
    let window_width = EmacsInt::from(win.body_width(false))
        - if cfg!(feature = "window-system") && win.get_frame().is_gui_window() {
            0
        } else {
            1
        };

    let (to_hpos, to_vpos) = if topos.is_nil() {
        (window_width, EmacsInt::from(win.internal_height()))
    } else {
        let (hpos, vpos) = topos.into();
        (hpos.into(), vpos.into())
    };
    let arg_width = width.unwrap_or(-1);
    let (hscroll, tab_offset) = if offsets.is_nil() {
        (0, 0)
    } else {
        let (hpos, vpos) = offsets.into();
        (hpos.into(), vpos.into())
    };
    if !(0 <= hscroll
        && hscroll <= isize::max_value() as EmacsInt
        && 0 <= tab_offset
        && tab_offset <= EmacsInt::from(libc::INT_MAX))
    {
        args_out_of_range!(hscroll, tab_offset);
    }

    let buffer = &mut ThreadState::current_buffer_unchecked();
    let begv = buffer.begv as EmacsInt;
    let zv = buffer.zv as EmacsInt;
    if from.to_fixnum() < begv || from.to_fixnum() > zv {
        args_out_of_range!(from, begv, zv);
    }
    if to.to_fixnum() < begv || to.to_fixnum() > zv {
        args_out_of_range!(to, begv, zv);
    }

    let pos = unsafe {
        *remacs_sys::compute_motion(
            from.to_fixnum() as isize,
            buf_charpos_to_bytepos(buffer.as_mut(), from.to_fixnum() as isize),
            from_vpos,
            from_hpos,
            false,
            to.to_fixnum() as isize,
            to_vpos,
            to_hpos,
            arg_width,
            hscroll as isize,
            tab_offset as i32,
            win.as_mut(),
        )
    };

    list!(
        pos.bufpos,
        pos.hpos,
        pos.vpos,
        pos.prevhpos,
        (pos.contin != 0)
    )
}

include!(concat!(env!("OUT_DIR"), "/indent_exports.rs"));
