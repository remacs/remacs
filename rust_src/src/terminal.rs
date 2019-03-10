//! Functions related to terminal devices.

use std::{mem, ptr};

use libc::{c_int, c_void};

use remacs_macros::lisp_fn;

use crate::{
    dispnew::LispGlyphRef,
    frames::Fselected_frame,
    frames::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    remacs_sys::build_string,
    remacs_sys::{pvec_type, Lisp_Terminal},
    remacs_sys::{Qnil, Qterminal_live_p},
    vectors::LispVectorlikeRef,
};

pub type LispTerminalRef = ExternalPtr<Lisp_Terminal>;

impl LispTerminalRef {
    pub fn is_live(self) -> bool {
        !self.name.is_null()
    }

    pub fn name(self) -> LispObject {
        if self.name.is_null() {
            Qnil
        } else {
            unsafe { build_string(self.name) }
        }
    }
}

impl LispVectorlikeRef {
    pub fn as_terminal(self) -> Option<LispTerminalRef> {
        if self.is_pseudovector(pvec_type::PVEC_TERMINAL) {
            Some(unsafe { mem::transmute(self) })
        } else {
            None
        }
    }
}

impl LispObject {
    pub fn is_terminal(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_TERMINAL))
    }

    pub fn as_terminal(self) -> Option<LispTerminalRef> {
        self.as_vectorlike()
            .and_then(LispVectorlikeRef::as_terminal)
    }
}

#[repr(transparent)]
pub struct LispLiveTerminal(LispTerminalRef);

impl From<LispObject> for Option<LispLiveTerminal> {
    fn from(obj: LispObject) -> Self {
        let obj = if obj.is_nil() { Fselected_frame() } else { obj };

        let term = if let Some(frame) = obj.as_frame() {
            frame.terminal
        } else if let Some(mut terminal) = obj.as_terminal() {
            terminal.as_mut()
        } else {
            ptr::null_mut()
        };

        if let Some(term_ref) = LispTerminalRef::from_ptr(term as *mut c_void) {
            if term_ref.is_live() {
                return Some(LispLiveTerminal(term_ref));
            }
        }

        None
    }
}

impl From<LispObject> for LispLiveTerminal {
    fn from(obj: LispObject) -> Self {
        let value: Option<LispLiveTerminal> = obj.into();
        value.unwrap_or_else(|| wrong_type!(Qterminal_live_p, obj))
    }
}

#[no_mangle]
pub unsafe extern "C" fn update_begin(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).update_begin_hook {
        hook(f.as_mut())
    }
}

#[no_mangle]
pub unsafe extern "C" fn update_end(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).update_end_hook {
        hook(f.as_mut())
    }
}

// Erase operations.

/// Clear from cursor to end of frame.
#[no_mangle]
pub unsafe extern "C" fn clear_to_end(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).clear_to_end_hook {
        hook(f.as_mut())
    }
}

/// Clear entire frame.
#[no_mangle]
pub unsafe extern "C" fn clear_frame(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).clear_frame_hook {
        hook(f.as_mut())
    }
}

/// Clear from cursor to end of line.
/// Assume that the line is already clear starting at column first_unused_hpos.
///
/// Note that the cursor may be moved, on terminals lacking a `ce' string.
#[no_mangle]
pub unsafe extern "C" fn clear_end_of_line(mut f: LispFrameRef, first_unused_hops: c_int) {
    if let Some(hook) = (*f.terminal).clear_end_of_line_hook {
        hook(f.as_mut(), first_unused_hops)
    }
}

/// Output LEN glyphs starting at STRING at the nominal cursor position.
/// Advance the nominal cursor over the text.
#[no_mangle]
pub unsafe extern "C" fn write_glyphs(mut f: LispFrameRef, mut string: LispGlyphRef, len: c_int) {
    if let Some(hook) = (*f.terminal).write_glyphs_hook {
        hook(f.as_mut(), string.as_mut(), len)
    }
}

/// Insert LEN glyphs from START at the nominal cursor position.
///
/// If start is zero, insert blanks instead of a string at start
#[no_mangle]
pub unsafe extern "C" fn insert_glyphs(mut f: LispFrameRef, mut start: LispGlyphRef, len: c_int) {
    if len <= 0 {
        return;
    }
    if let Some(hook) = (*f.terminal).insert_glyphs_hook {
        hook(f.as_mut(), start.as_mut(), len)
    }
}

/// Delete N glyphs at the nominal cursor position.
#[no_mangle]
pub unsafe extern "C" fn delete_glyphs(mut f: LispFrameRef, n: c_int) {
    if let Some(hook) = (*f.terminal).delete_glyphs_hook {
        hook(f.as_mut(), n)
    }
}

/// Insert N lines at vpos VPOS.  If N is negative, delete -N lines.
#[no_mangle]
pub unsafe extern "C" fn ins_del_lines(mut f: LispFrameRef, vpos: c_int, n: c_int) {
    if let Some(hook) = (*f.terminal).ins_del_lines_hook {
        hook(f.as_mut(), vpos, n)
    }
}

/// Return the terminal object specified by TERMINAL.  TERMINAL may
/// be a terminal object, a frame, or nil for the terminal device of
/// the current frame.  If TERMINAL is neither from the above or the
/// resulting terminal object is deleted, return NULL.
#[no_mangle]
pub extern "C" fn decode_terminal(terminal: LispObject) -> *mut Lisp_Terminal {
    let term: Option<LispLiveTerminal> = terminal.into();
    term.map_or_else(ptr::null_mut, |mut term| term.0.as_mut())
}

/// Like decode_terminal, but throw an error if TERMINAL is not valid or deleted.
#[no_mangle]
pub extern "C" fn decode_live_terminal(terminal: LispObject) -> *mut Lisp_Terminal {
    let mut term: LispLiveTerminal = terminal.into();
    term.0.as_mut()
}

/// Return the name of the terminal device TERMINAL.
/// It is not guaranteed that the returned value is unique among opened devices.
///
/// TERMINAL may be a terminal object, a frame, or nil (meaning the
/// selected frame's terminal).
#[lisp_fn(min = "0")]
pub fn terminal_name(terminal: LispLiveTerminal) -> LispObject {
    terminal.0.name()
}

include!(concat!(env!("OUT_DIR"), "/terminal_exports.rs"));
