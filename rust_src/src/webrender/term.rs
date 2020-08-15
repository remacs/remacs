use std::ptr;

use super::display_info::{DisplayInfo, DisplayInfoRef};

use crate::{
    lisp::{ExternalPtr, LispObject},
    remacs_sys::{
        allocate_kboard, create_terminal, current_kboard, initial_kboard, output_method, terminal,
        xlispstrdup, Fcons, Qnil, Qwr, KBOARD,
    },
};

pub type TerminalRef = ExternalPtr<terminal>;

impl Default for TerminalRef {
    fn default() -> Self {
        Self::new(ptr::null_mut())
    }
}

pub type KboardRef = ExternalPtr<KBOARD>;

impl KboardRef {
    pub fn add_ref(&mut self) {
        (*self).reference_count = (*self).reference_count + 1;
    }
}

fn wr_create_terminal(mut dpyinfo: DisplayInfoRef) -> TerminalRef {
    let terminal_ptr = unsafe { create_terminal(output_method::output_wr, ptr::null_mut()) };
    let mut terminal = TerminalRef::new(terminal_ptr);

    // Link terminal and dpyinfo together
    terminal.display_info.wr = dpyinfo.as_mut();
    dpyinfo.get_inner().terminal = terminal;

    //TODO: add terminal hook
    // Other hooks are NULL by default.

    terminal
}

pub fn wr_term_init(display_name: LispObject) -> DisplayInfoRef {
    let dpyinfo = Box::new(DisplayInfo::new());
    let mut dpyinfo_ref = DisplayInfoRef::new(Box::into_raw(dpyinfo));

    let mut terminal = wr_create_terminal(dpyinfo_ref);

    let mut kboard = KboardRef::new(unsafe { allocate_kboard(Qwr) });
    terminal.kboard = kboard.as_mut();

    // Don't let the initial kboard remain current longer than necessary.
    // That would cause problems if a file loaded on startup tries to
    // prompt in the mini-buffer.
    unsafe {
        if current_kboard == initial_kboard {
            current_kboard = terminal.kboard;
        }
    }

    kboard.add_ref();

    dpyinfo_ref.name_list_element = unsafe { Fcons(display_name, Qnil) };

    // https://lists.gnu.org/archive/html/emacs-devel/2015-11/msg00194.html
    dpyinfo_ref.smallest_font_height = 1;
    dpyinfo_ref.smallest_char_width = 1;

    // Set the name of the terminal.
    terminal.name = unsafe { xlispstrdup(display_name) };

    dpyinfo_ref
}
