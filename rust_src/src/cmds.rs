//! Commands

use libc;
use std;
use std::ffi::CString;

use remacs_macros::lisp_fn;

use crate::{
    character::{self, characterp},
    data::set,
    dispnew::ding_internal,
    editfns::{insert_and_inherit, line_beginning_position, line_end_position, preceding_char},
    frames::selected_frame,
    keymap::{current_global_map, Ctl},
    lisp::LispObject,
    lists::get,
    multibyte::{
        char_to_byte8, single_byte_charp, unibyte_to_char, write_codepoint, Codepoint,
        MAX_MULTIBYTE_LENGTH,
    },
    numbers::MOST_POSITIVE_FIXNUM,
    obarray::intern,
    remacs_sys::EmacsInt,
    remacs_sys::{
        concat2, current_column, del_range, frame_make_pointer_invisible, globals,
        initial_define_key, memory_full, replace_range, run_hook, scan_newline_from_point,
        set_point, set_point_both, syntax_property, syntaxcode, translate_char,
    },
    remacs_sys::{Fchar_width, Fmake_string, Fmove_to_column},
    remacs_sys::{
        Qbeginning_of_buffer, Qend_of_buffer, Qexpand_abbrev, Qinternal_auto_fill,
        Qkill_forward_chars, Qnil, Qoverwrite_mode_binary, Qpost_self_insert_hook,
        Qundo_auto__this_command_amalgamating, Qundo_auto_amalgamate,
    },
    threads::ThreadState,
};

/// Add N to point; or subtract N if FORWARD is false. N defaults to 1.
/// Validate the new location. Return nil.
fn move_point(n: LispObject, forward: bool) {
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

    let buffer = ThreadState::current_buffer_unchecked();
    let mut signal = Qnil;
    let mut new_point = buffer.pt + n;

    if new_point < buffer.begv {
        new_point = buffer.begv;
        signal = Qbeginning_of_buffer;
    } else if new_point > buffer.zv {
        new_point = buffer.zv;
        signal = Qend_of_buffer;
    }

    unsafe { set_point(new_point) };
    if signal.is_not_nil() {
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
pub fn forward_char(n: LispObject) {
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
pub fn backward_char(n: LispObject) {
    move_point(n, false)
}

/// Return buffer position N characters after (before if N negative) point.
#[lisp_fn]
pub fn forward_point(n: EmacsInt) -> EmacsInt {
    let pt = ThreadState::current_buffer_unchecked().pt;
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
pub fn beginning_of_line(n: Option<EmacsInt>) {
    let pos = line_beginning_position(n);

    unsafe {
        set_point(pos as isize);
    }
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
pub fn end_of_line(n: Option<EmacsInt>) {
    let mut num = n.unwrap_or(1);
    let mut newpos: isize;
    let mut pt: isize;
    let cur_buf = ThreadState::current_buffer_unchecked();
    loop {
        newpos = line_end_position(Some(num)) as isize;
        unsafe { set_point(newpos) };
        pt = cur_buf.pt;
        if pt > newpos && cur_buf.fetch_char(pt - 1) == '\n' as i32 {
            // If we skipped over a newline that follows
            // an invisible intangible run,
            // move back to the last tangible position
            // within the line.
            unsafe { set_point(pt - 1) };
            break;
        } else if pt > newpos && pt < cur_buf.zv && cur_buf.fetch_char(newpos) != '\n' as i32 {
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
pub fn forward_line(n: Option<EmacsInt>) -> EmacsInt {
    let count: isize = n.unwrap_or(1) as isize;

    let cur_buf = ThreadState::current_buffer_unchecked();
    let opoint = cur_buf.pt;

    let (mut pos, mut pos_byte) = (0, 0);

    let mut shortage: EmacsInt =
        unsafe { scan_newline_from_point(count, &mut pos, &mut pos_byte) as EmacsInt };

    unsafe { set_point_both(pos, pos_byte) };

    if shortage > 0
        && (count <= 0
            || (cur_buf.zv > cur_buf.begv
                && cur_buf.pt != opoint
                && cur_buf.fetch_byte(cur_buf.pt_byte - 1) != b'\n'))
    {
        shortage -= 1
    }

    if count <= 0 {
        -shortage
    } else {
        shortage
    }
}

/// Delete the following N characters (previous if N is negative).
/// Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
/// Interactively, N is the prefix arg, and KILLFLAG is set if
/// N was explicitly specified.
///
/// The command `delete-forward-char' is preferable for interactive use, e.g.
/// because it respects values of `delete-active-region' and `overwrite-mode'.
#[lisp_fn(min = "1", intspec = "p\nP")]
pub fn delete_char(n: EmacsInt, killflag: bool) {
    if n.abs() < 2 {
        call!(Qundo_auto_amalgamate);
    }

    let buffer = ThreadState::current_buffer_unchecked();
    let pos = buffer.pt + n as isize;
    if killflag {
        call!(Qkill_forward_chars, n.into());
    } else if n < 0 {
        if pos < buffer.begv {
            xsignal!(Qbeginning_of_buffer);
        } else {
            unsafe { del_range(pos, buffer.pt) };
        }
    } else if pos > buffer.zv {
        xsignal!(Qend_of_buffer);
    } else {
        unsafe { del_range(buffer.pt, pos) };
    }
}

// Note that there's code in command_loop_1 which typically avoids
// calling this.

/// Insert the character you type.
/// Whichever character you type to run this command is inserted.
/// The numeric prefix argument N says how many times to repeat the insertion.
/// Before insertion, `expand-abbrev' is executed if the inserted character does
/// not have word syntax and the previous character in the buffer does.
/// After insertion, `internal-auto-fill' is called if
/// `auto-fill-function' is non-nil and if the `auto-fill-chars' table has
/// a non-nil value for the inserted character.  At the end, it runs
/// `post-self-insert-hook'.
#[lisp_fn(intspec = "p")]
pub fn self_insert_command(n: EmacsInt) {
    if n < 0 {
        error!("Negative repetition argument {}", n);
    }

    if n < 2 {
        call!(Qundo_auto_amalgamate);
    }

    // Barf if the key that invoked this was not a character.
    if !characterp(unsafe { globals.last_command_event }, Qnil) {
        ding_internal(true);
    } else {
        let character = unsafe {
            translate_char(
                globals.Vtranslation_table_for_input,
                globals.last_command_event.as_fixnum_or_error() as i32,
            )
        };
        let val = internal_self_insert(character as Codepoint, n as usize);
        if val == 2 {
            set(Qundo_auto__this_command_amalgamating.into(), Qnil);
        }
        unsafe { frame_make_pointer_invisible(selected_frame().as_mut()) };
    }
}

//enum Hairyness {
//    Simple,
//    Maybe,
//    Very,
//}

/// Insert N times character C
///
/// If this insertion is suitable for direct output (completely simple),
/// return 0.  A value of 1 indicates this *might* not have been simple.
/// A value of 2 means this did things that call for an undo boundary.
fn internal_self_insert(mut c: Codepoint, n: usize) -> EmacsInt {
    let mut hairy: EmacsInt = 0;
    let synt: syntaxcode;
    // Length of multi-byte form of C.
    let len: usize;
    // Working buffer and pointer for multi-byte form of C.
    let mut str = [0_u8; MAX_MULTIBYTE_LENGTH];
    let mut chars_to_delete: usize = 0;
    let mut spaces_to_insert: usize = 0;

    let mut current_buffer = ThreadState::current_buffer_unchecked();
    let overwrite = current_buffer.overwrite_mode_;
    if unsafe { globals.Vbefore_change_functions }.is_not_nil()
        || unsafe { globals.Vafter_change_functions }.is_not_nil()
    {
        hairy = 1;
    }

    // At first, get multi-byte form of C in STR.
    if current_buffer.multibyte_characters_enabled() {
        len = write_codepoint(&mut str, c);
        if len == 1 {
            c = Codepoint::from(str[0]);
        }
    } else {
        str[0] = if single_byte_charp(c) {
            c as u8
        } else {
            char_to_byte8(c)
        };
        len = 1;
    }
    if overwrite.is_not_nil() && current_buffer.pt < current_buffer.zv {
        // In overwrite-mode, we substitute a character at point (C2,
        // hereafter) by C.  For that, we delete C2 in advance.  But,
        // just substituting C2 by C may move a remaining text in the
        // line to the right or to the left, which is not preferable.
        // So we insert more spaces or delete more characters in the
        // following cases: if C is narrower than C2, after deleting C2,
        // we fill columns with spaces, if C is wider than C2, we delete
        // C2 and several characters following C2.

        // This is the character after point.
        let c2 = current_buffer.fetch_char(current_buffer.pt_byte) as Codepoint;

        // Overwriting in binary-mode always replaces C2 by C.
        // Overwriting in textual-mode doesn't always do that.
        // It inserts newlines in the usual way,
        // and inserts any character at end of line
        // or before a tab if it doesn't use the whole width of the tab.  */
        if overwrite == Qoverwrite_mode_binary {
            chars_to_delete = n as usize;
        } else if c != '\n' as Codepoint && c2 != '\n' as Codepoint {
            let cwidth = unsafe { Fchar_width(c.into()) }.as_fixnum_or_error() as usize;
            if cwidth > 0 {
                let pos = current_buffer.pt;
                let pos_byte = current_buffer.pt_byte;
                let curcol = unsafe { current_column() } as usize;

                if n <= (MOST_POSITIVE_FIXNUM as usize - curcol) / cwidth {
                    // Column the cursor should be placed at after this insertion.
                    // The value should be calculated only when necessary.
                    let target_clm = curcol + (n * cwidth);

                    // The actual cursor position after the trial of moving
                    // to column TARGET_CLM.  It is greater than TARGET_CLM
                    // if the TARGET_CLM is middle of multi-column
                    // character.  In that case, the new point is set after
                    // that character.
                    let actual_clm = unsafe { Fmove_to_column(target_clm.into(), Qnil) }
                        .as_fixnum_or_error() as usize;
                    chars_to_delete = (current_buffer.pt - pos) as usize;
                    if actual_clm > target_clm {
                        // We will delete too many columns.  Let's fill columns
                        // by spaces so that the remaining text won't move.
                        let actual = unsafe { character::dec_pos(current_buffer.pt_byte) };
                        if current_buffer.fetch_char(actual) as Codepoint == '\t' as Codepoint {
                            // Rather than add spaces, let's just keep the tab.
                            chars_to_delete -= 1;
                        } else {
                            spaces_to_insert = actual_clm - target_clm;
                        }
                    }
                    current_buffer.set_pt_both(pos, pos_byte);
                }
            }
        }
        hairy = 2;
    }
    synt = unsafe { syntax_property(c as i32, true) };

    let previous_char = if current_buffer.multibyte_characters_enabled() {
        preceding_char() as Codepoint
    } else {
        unibyte_to_char(preceding_char() as Codepoint)
    };
    if current_buffer.abbrev_mode_.is_not_nil()
        && synt != syntaxcode::Sword
        && current_buffer.read_only_.is_nil()
        && current_buffer.pt > current_buffer.begv
        && unsafe { syntax_property(previous_char as libc::c_int, true) } == syntaxcode::Sword
    {
        let modiff = unsafe { (*current_buffer.text).modiff };

        let sym = call!(Qexpand_abbrev);

        // If we expanded an abbrev which has a hook,
        // and the hook has a non-nil `no-self-insert' property,
        // return right away--don't really self-insert.  */
        if let Some(s) = sym.as_symbol() {
            if let Some(f) = s.get_function().as_symbol() {
                let prop = get(f, intern("no-self-insert").into());
                if prop.is_not_nil() {
                    return 1;
                }
            }
        }

        if unsafe { (*current_buffer.text).modiff != modiff } {
            hairy = 2;
        }
    }

    if chars_to_delete > 0 {
        let mc = if current_buffer.multibyte_characters_enabled() && single_byte_charp(c) {
            unibyte_to_char(c)
        } else {
            c
        };
        let mut string = unsafe { Fmake_string(n.into(), mc.into(), Qnil) };
        if spaces_to_insert > 0 {
            let tem =
                unsafe { Fmake_string(spaces_to_insert.into(), (' ' as Codepoint).into(), Qnil) };
            string = unsafe { concat2(string, tem) };
        }

        unsafe {
            replace_range(
                current_buffer.pt,
                current_buffer.pt + chars_to_delete as isize,
                string,
                true,
                true,
                true,
                false,
            )
        };
        forward_char(n.into());
    } else if n > 1 {
        let mut strn: Vec<libc::c_uchar> = match n.checked_mul(len) {
            Some(size_bytes) => Vec::with_capacity(size_bytes),
            None => unsafe { memory_full(std::usize::MAX) },
        };
        for _ in 0..n {
            strn.extend_from_slice(&str[0..len]);
        }
        insert_and_inherit(strn.as_slice());
    } else if n > 0 {
        insert_and_inherit(&str[..len]);
    }

    if let Some(t) = unsafe { globals.Vauto_fill_chars }.as_char_table() {
        if t.get(c as isize).is_not_nil()
            && (c == ' ' as Codepoint || c == '\n' as Codepoint)
            && current_buffer.auto_fill_function_.is_not_nil()
        {
            if c == '\n' as Codepoint {
                // After inserting a newline, move to previous line and fill
                // that.  Must have the newline in place already so filling and
                // justification, if any, know where the end is going to be.
                let newpt = current_buffer.pt - 1;
                let newpt_byte = current_buffer.pt_byte - 1;
                current_buffer.set_pt_both(newpt, newpt_byte);
            }
            let auto_fill_result = call!(Qinternal_auto_fill);
            // Test PT < ZV in case the auto-fill-function is strange.
            if c == '\n' as Codepoint && current_buffer.pt < current_buffer.zv {
                let newpt = current_buffer.pt + 1;
                let newpt_byte = current_buffer.pt_byte + 1;
                current_buffer.set_pt_both(newpt, newpt_byte);
            }
            if auto_fill_result.is_not_nil() {
                hairy = 2;
            }
        }
    }

    // Run hooks for electric keys.
    unsafe { run_hook(Qpost_self_insert_hook) };

    hairy
}

// module initialization

#[no_mangle]
pub extern "C" fn keys_of_cmds() {
    let global_map = current_global_map();

    unsafe {
        let sic = CString::new("self-insert-command").unwrap();
        initial_define_key(global_map, Ctl('I'), sic.as_ptr());
        for n in 0x20..0x7f {
            initial_define_key(global_map, n, sic.as_ptr());
        }
        for n in 0xa0..0x100 {
            initial_define_key(global_map, n, sic.as_ptr());
        }

        let bol = CString::new("beginning-of-line").unwrap();
        initial_define_key(global_map, Ctl('A'), bol.as_ptr());
        let bc = CString::new("backward-char").unwrap();
        initial_define_key(global_map, Ctl('B'), bc.as_ptr());
        let eol = CString::new("end-of-line").unwrap();
        initial_define_key(global_map, Ctl('E'), eol.as_ptr());
        let fc = CString::new("forward-char").unwrap();
        initial_define_key(global_map, Ctl('F'), fc.as_ptr());
    }
}

#[no_mangle]
pub extern "C" fn syms_of_cmds() {
    def_lisp_sym!(Qinternal_auto_fill, "internal-auto-fill");
    def_lisp_sym!(Qundo_auto_amalgamate, "undo-auto-amalgamate");
    #[rustfmt::skip]
    def_lisp_sym!(Qundo_auto__this_command_amalgamating, "undo-auto--this-command-amalgamating");
    def_lisp_sym!(Qkill_forward_chars, "kill-forward-chars");
    // A possible value for a buffer's overwrite-mode variable.
    def_lisp_sym!(Qoverwrite_mode_binary, "overwrite-mode-binary");
    def_lisp_sym!(Qexpand_abbrev, "expand-abbrev");
    def_lisp_sym!(Qpost_self_insert_hook, "post-self-insert-hook");

    /// Hook run at the end of `self-insert-command'.
    /// This is run after inserting the character.
    defvar_lisp!(Vpost_self_insert_hook, "post-self-insert-hook", Qnil);
}

include!(concat!(env!("OUT_DIR"), "/cmds_exports.rs"));
