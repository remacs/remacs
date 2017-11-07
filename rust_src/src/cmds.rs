//! Commands

use remacs_macros::lisp_fn;
use remacs_sys::{set_point, Fline_beginning_position, Fline_end_position};
use remacs_sys::EmacsInt;

use lisp::LispObject;
use lisp::defsubr;

use threads::ThreadState;


/// Return buffer position N characters after (before if N negative) point.
#[lisp_fn]
pub fn forward_point(n: LispObject) -> LispObject {
    let pt = ThreadState::current_buffer().pt();
    LispObject::from_fixnum(n.as_fixnum_or_error() + pt as EmacsInt)
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
pub fn beginning_of_line(mut n: LispObject) -> LispObject {
    if n.is_nil() {
        n = LispObject::from_fixnum(1);
    }

    unsafe {
        let pos = LispObject::from(Fline_beginning_position(n.to_raw()));
        set_point(pos.as_fixnum_or_error() as isize);
    };

    LispObject::constant_nil()
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
pub fn end_of_line(n: LispObject) -> LispObject {
    let mut num = if n.is_nil() {
        LispObject::from_fixnum(1)
    } else {
        n
    };
    num.as_fixnum_or_error();
    let mut newpos: isize;
    let mut pt: isize;
    let cur_buf = ThreadState::current_buffer();
    loop {
        newpos = unsafe {
            LispObject::from(Fline_end_position(num.to_raw())).as_fixnum_or_error() as isize
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
            num = LispObject::from_fixnum(1)
        } else {
            break;
        }
    }
    LispObject::constant_nil()
}

include!(concat!(env!("OUT_DIR"), "/cmds_exports.rs"));
