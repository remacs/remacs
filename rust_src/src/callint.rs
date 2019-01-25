//! Call a Lisp function interactively.

use crate::{
    eval::{funcall, unbind_to},
    lisp::LispObject,
    remacs_macros::lisp_fn,
    remacs_sys::temporarily_switch_to_single_kboard,
    threads::c_specpdl_index,
};

/// Specify a way of parsing arguments for interactive use of a function.
/// For example, write
///  (defun foo (arg buf) "Doc string" (interactive "P\\nbbuffer: ") .... )
///  to make ARG be the raw prefix argument, and set BUF to an existing buffer,
///  when `foo' is called as a command.
///
/// The "call" to `interactive' is actually a declaration rather than a
///  function; it tells `call-interactively' how to read arguments to pass
///  to the function.  When actually called, `interactive' just returns
///  nil.
///
/// Usually the argument of `interactive' is a string containing a code
///  letter followed optionally by a prompt.  (Some code letters do not
///  use I/O to get the argument and do not use prompts.)  To pass several
///  arguments to the command, concatenate the individual strings,
///  separating them by newline characters.
///
/// Prompts are passed to `format', and may use % escapes to print the
///  arguments that have already been read.
/// If the argument is not a string, it is evaluated to get a list of
///  arguments to pass to the command.
/// Just `(interactive)' means pass no arguments to the command when
///  calling interactively.
///
/// Code letters available are:
/// a -- Function name: symbol with a function definition.
/// b -- Name of existing buffer.
/// B -- Name of buffer, possibly nonexistent.
/// c -- Character (no input method is used).
/// C -- Command name: symbol with interactive function definition.
/// d -- Value of point as number.  Does not do I/O.
/// D -- Directory name.
/// e -- Parameterized event (i.e., one that's a list) that invoked this command.
///      If used more than once, the Nth `e' returns the Nth parameterized event.
///      This skips events that are integers or symbols.
/// f -- Existing file name.
/// F -- Possibly nonexistent file name.
/// G -- Possibly nonexistent file name, defaulting to just directory name.
/// i -- Ignored, i.e. always nil.  Does not do I/O.
/// k -- Key sequence (downcase the last event if needed to get a definition).
/// K -- Key sequence to be redefined (do not downcase the last event).
/// m -- Value of mark as number.  Does not do I/O.
/// M -- Any string.  Inherits the current input method.
/// n -- Number read using minibuffer.
/// N -- Numeric prefix arg, or if none, do like code `n'.
/// p -- Prefix arg converted to number.  Does not do I/O.
/// P -- Prefix arg in raw form.  Does not do I/O.
/// r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
/// s -- Any string.  Does not inherit the current input method.
/// S -- Any symbol.
/// U -- Mouse up event discarded by a previous k or K argument.
/// v -- Variable name: symbol that is `custom-variable-p'.
/// x -- Lisp expression read but not evaluated.
/// X -- Lisp expression read and evaluated.
/// z -- Coding system.
/// Z -- Coding system, nil if no prefix arg.
///
/// In addition, if the string begins with `*', an error is signaled if
///   the buffer is read-only.
/// If `@' appears at the beginning of the string, and if the key sequence
///  used to invoke the command includes any mouse events, then the window
///  associated with the first of those events is selected before the
///  command is run.
/// If the string begins with `^' and `shift-select-mode' is non-nil,
///  Emacs first calls the function `handle-shift-selection'.
/// You may use `@', `*', and `^' together.  They are processed in the
///  order that they appear, before reading any arguments.
/// usage: (interactive &optional ARG-DESCRIPTOR)
#[lisp_fn(min = "0", unevalled = "true")]
pub fn interactive(_args: LispObject) {}

// BEWARE: Calling this directly from C / Rust would defeat the purpose!

/// Like `funcall' but marks the call as interactive.
/// I.e. arrange that within the called function `called-interactively-p' will return non-nil.
/// usage: (funcall-interactively FUNCTION &rest ARGUMENTS)
#[lisp_fn(min = "1")]
pub fn funcall_interactively(args: &mut [LispObject]) -> LispObject {
    let count = c_specpdl_index();

    unsafe { temporarily_switch_to_single_kboard(std::ptr::null_mut()) };

    unbind_to(count, funcall(args))
}

include!(concat!(env!("OUT_DIR"), "/callint_exports.rs"));
