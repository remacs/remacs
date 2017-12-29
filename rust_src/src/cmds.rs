//! Commands

use std::ffi::CString;

use remacs_macros::lisp_fn;
use remacs_sys::{initial_define_key, scan_newline_from_point, set_point, set_point_both,
                 Fline_beginning_position, Fline_end_position};
use remacs_sys::{Qbeginning_of_buffer, Qend_of_buffer, Qnil};
use remacs_sys::EmacsInt;

use keymap::{current_global_map, Ctl};
use lisp::LispObject;
use lisp::defsubr;
use threads::ThreadState;

/// Add N to point; or subtract N if FORWARD is false. N defaults to 1.
/// Validate the new location. Return nil.
fn move_point(n: LispObject, forward: bool) -> () {
    // This used to just set point to 'point + n', and then check
    // to see if it was within boundaries. But now that SET_POINT can
    // potentially do a lot of stuff (calling entering and exiting
    // hooks, et cetera), that's not a good approach. So we validate the
    // proposed position, then set point.

    let mut n = if n.is_nil() {
        1
    } else {
        n.as_fixnum_or_error() as isize
    };

    if !forward {
        n = -n;
    }

    let buffer = ThreadState::current_buffer();
    let mut signal = Qnil;
    let mut new_point = buffer.pt() + n;

    if new_point < buffer.begv {
        new_point = buffer.begv;
        signal = Qbeginning_of_buffer;
    } else if new_point > buffer.zv {
        new_point = buffer.zv;
        signal = Qend_of_buffer;
    }

    unsafe { set_point(new_point) };
    if signal != Qnil {
        xsignal!(signal);
    }
}

/// Move point N characters forward (backward if N is negative).
/// On reaching end or beginning of buffer, stop and signal error.
/// Interactively, N is the numeric prefix argument.
/// If N is omitted or nil, move point 1 character forward.
///
/// Depending on the bidirectional context, the movement may be to the
/// right or to the left on the screen.  This is in contrast with
/// \\[right-char], which see.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn forward_char(n: LispObject) -> () {
    move_point(n, true)
}

/// Move point N characters backward (forward if N is negative).
/// On attempt to pass beginning or end of buffer, stop and signal error.
/// Interactively, N is the numeric prefix argument.
/// If N is omitted or nil, move point 1 character backward.
///
/// Depending on the bidirectional context, the movement may be to the
/// right or to the left on the screen.  This is in contrast with
/// \\[left-char], which see.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn backward_char(n: LispObject) -> () {
    move_point(n, false)
}

/// Return buffer position N characters after (before if N negative) point.
#[lisp_fn]
pub fn forward_point(n: EmacsInt) -> EmacsInt {
    let pt = ThreadState::current_buffer().pt();
    n + pt as EmacsInt
}

/// Move point to beginning of current line (in the logical order).
/// With argument N not nil or 1, move forward N - 1 lines first.
/// If point reaches the beginning or end of buffer, it stops there.
/// This function constrains point to the current field unless this moves
/// point to a different line than the original, unconstrained result.
/// If N is nil or 1, and a front-sticky field starts at point, the point
/// does not move.  To ignore field boundaries bind
/// `inhibit-field-text-motion' to t, or use the `forward-line' function
/// instead.  For instance, `(forward-line 0)' does the same thing as
/// `(beginning-of-line)', except that it ignores field boundaries.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn beginning_of_line(mut n: LispObject) -> () {
    if n.is_nil() {
        n = LispObject::from_fixnum(1);
    }

    unsafe {
        let pos = LispObject::from_raw(Fline_beginning_position(n.to_raw()));
        set_point(pos.as_fixnum_or_error() as isize);
    };
}

/// Move point to end of current line (in the logical order).
/// With argument N not nil or 1, move forward N - 1 lines first.
/// If point reaches the beginning or end of buffer, it stops there.
/// To ignore intangibility, bind `inhibit-point-motion-hooks' to t.
///
/// This function constrains point to the current field unless this moves
/// point to a different line than the original, unconstrained result.  If
/// N is nil or 1, and a rear-sticky field ends at point, the point does
/// not move.  To ignore field boundaries bind `inhibit-field-text-motion'
/// to t.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn end_of_line(n: Option<EmacsInt>) -> () {
    let mut num = n.unwrap_or(1);
    let mut newpos: isize;
    let mut pt: isize;
    let cur_buf = ThreadState::current_buffer();
    loop {
        newpos = unsafe {
            LispObject::from_raw(Fline_end_position(LispObject::from_fixnum(num).to_raw()))
                .as_fixnum_or_error() as isize
        };
        unsafe { set_point(newpos) };
        pt = cur_buf.pt();
        if pt > newpos && cur_buf.fetch_char(pt - 1) == '\n' as i32 {
            // If we skipped over a newline that follows
            // an invisible intangible run,
            // move back to the last tangible position
            // within the line.
            unsafe { set_point(pt - 1) };
            break;
        } else if pt > newpos && pt < cur_buf.zv() && cur_buf.fetch_char(newpos) != '\n' as i32 {
            // If we skipped something intangible
            // and now we're not really at eol,
            // keep going.
            num = 1
        } else {
            break;
        }
    }
}

/// Move N lines forward (backward if N is negative).
/// Precisely, if point is on line I, move to the start of line I + N
/// ("start of line" in the logical order).
/// If there isn't room, go as far as possible (no error).
///
/// Returns the count of lines left to move.  If moving forward,
/// that is N minus number of lines moved; if backward, N plus number
/// moved.
///
/// Exception: With positive N, a non-empty line at the end of the
/// buffer, or of its accessible portion, counts as one line
/// successfully moved (for the return value).  This means that the
/// function will move point to the end of such a line and will count
/// it as a line moved across, even though there is no next line to
/// go to its beginning.
#[lisp_fn(min = "0", intspec = "^p")]
pub fn forward_line(n: LispObject) -> LispObject {
    let cur_buf = ThreadState::current_buffer();

    let opoint = cur_buf.pt();

    let count: isize = if n.is_nil() {
        1
    } else {
        n.as_fixnum_or_error() as isize
    };

    let (mut pos, mut pos_byte) = (0, 0);

    let mut shortage = unsafe { scan_newline_from_point(count, &mut pos, &mut pos_byte) };

    unsafe { set_point_both(pos, pos_byte) };

    if shortage > 0
        && (count <= 0
            || (cur_buf.zv() > cur_buf.begv && cur_buf.pt() != opoint
                && (cur_buf.fetch_byte(cur_buf.pt_byte - 1) != '\n' as u8)))
    {
        shortage -= 1
    };

    LispObject::from_fixnum(if count <= 0 { -shortage } else { shortage } as EmacsInt)
}

pub fn initial_keys() {
    let global_map = current_global_map().to_raw();

    unsafe {
        let A = CString::new("beginning-of-line").unwrap();
        initial_define_key(global_map, Ctl('A'), A.as_ptr());
        let B = CString::new("backward-char").unwrap();
        initial_define_key(global_map, Ctl('B'), B.as_ptr());
        let E = CString::new("end-of-line").unwrap();
        initial_define_key(global_map, Ctl('E'), E.as_ptr());
        let F = CString::new("forward-char").unwrap();
        initial_define_key(global_map, Ctl('F'), F.as_ptr());
    }
}

include!(concat!(env!("OUT_DIR"), "/cmds_exports.rs"));
