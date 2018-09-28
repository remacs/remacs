//! Minibuffer input and completion.

use remacs_macros::lisp_fn;
use remacs_sys::{make_buffer_string, minibuf_level, minibuf_prompt, minibuf_window, EmacsInt,
                 Fcopy_sequence, Ffuncall};
use remacs_sys::{Qfield, Vminibuffer_list};

use buffers::{current_buffer, get_buffer};
use editfns::field_end;
use lisp::defsubr;
use lisp::LispObject;
use lists::memq;
use obarray::intern;
use symbols::symbol_value;
use textprop::get_char_property;
use threads::ThreadState;

/// Return t if BUFFER is a minibuffer.
/// No argument or nil as argument means use current buffer as BUFFER.
/// BUFFER can be a buffer or a buffer name.
#[lisp_fn(min = "0")]
pub fn minibufferp(object: LispObject) -> bool {
    let buffer = if object.is_nil() {
        current_buffer()
    } else if object.is_string() {
        get_buffer(object)
    } else {
        object.as_buffer_or_error();
        object
    };
    memq(buffer, unsafe { Vminibuffer_list }).is_not_nil()
}

/// Return the currently active minibuffer window, or nil if none.
#[lisp_fn]
pub fn active_minibuffer_window() -> LispObject {
    unsafe {
        if minibuf_level == 0 {
            LispObject::constant_nil()
        } else {
            minibuf_window
        }
    }
}

/// Specify which minibuffer window to use for the minibuffer.
/// This affects where the minibuffer is displayed if you put text in it
/// without invoking the usual minibuffer commands.
#[lisp_fn]
pub fn set_minibuffer_window(window: LispObject) -> LispObject {
    window.as_minibuffer_or_error(); // just for the checks

    unsafe {
        minibuf_window = window;
    }

    window
}

/// Return current depth of activations of minibuffer,
/// a nonnegative integer.
#[lisp_fn]
pub fn minibuffer_depth() -> EmacsInt {
    unsafe { minibuf_level }
}

/// Return the prompt string of the currently active
/// minibuffer. If no minibuffer is active return nil.
#[lisp_fn]
pub fn minibuffer_prompt() -> LispObject {
    unsafe { Fcopy_sequence(minibuf_prompt) }
}

/// Return the buffer position of the end of the minibuffer prompt.
/// Return (point-min) if current buffer is not a minibuffer.
#[lisp_fn]
pub fn minibuffer_prompt_end() -> EmacsInt {
    let beg = ThreadState::current_buffer().beg() as EmacsInt;
    if memq(current_buffer(), unsafe { Vminibuffer_list }).is_nil() {
        return beg;
    }

    let end = field_end(Some(beg), false, None);
    let buffer_end = ThreadState::current_buffer().zv as EmacsInt;
    if end == buffer_end && get_char_property(beg, Qfield, LispObject::constant_nil()).is_nil() {
        beg
    } else {
        end
    }
}

/// Return the user input in a minibuffer as a string.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_contents() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    unsafe { make_buffer_string(prompt_end, ThreadState::current_buffer().zv, true) }
}

/// Return the user input in a minibuffer as a string, without text-properties.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_contents_no_properties() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    unsafe { make_buffer_string(prompt_end, ThreadState::current_buffer().zv, false) }
}

/// Return the user input in a minibuffer before point as a string.
/// That is what completion commands operate on.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_completion_contents() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    let pt = ThreadState::current_buffer().pt;
    if pt < prompt_end {
        error!("Cannot do completion in the prompt");
    }
    unsafe { make_buffer_string(prompt_end, pt, true) }
}

/// Read a string in the minibuffer, with completion.
/// PROMPT is a string to prompt with; normally it ends in a colon and a space.
/// COLLECTION can be a list of strings, an alist, an obarray or a hash table.
/// COLLECTION can also be a function to do the completion itself.
/// PREDICATE limits completion to a subset of COLLECTION.
/// See `try-completion', `all-completions', `test-completion',
/// and `completion-boundaries', for more details on completion,
/// COLLECTION, and PREDICATE.  See also Info nodes `(elisp)Basic Completion'
/// for the details about completion, and `(elisp)Programmed Completion' for
/// expectations from COLLECTION when it's a function.
///
/// REQUIRE-MATCH can take the following values:
/// - t means that the user is not allowed to exit unless
///   the input is (or completes to) an element of COLLECTION or is null.
/// - nil means that the user can exit with any input.
/// - `confirm' means that the user can exit with any input, but she needs
///   to confirm her choice if the input is not an element of COLLECTION.
/// - `confirm-after-completion' means that the user can exit with any
///   input, but she needs to confirm her choice if she called
///   `minibuffer-complete' right before `minibuffer-complete-and-exit'
///   and the input is not an element of COLLECTION.
/// - anything else behaves like t except that typing RET does not exit if it
///   does non-null completion.
///
/// If the input is null, `completing-read' returns DEF, or the first element
/// of the list of default values, or an empty string if DEF is nil,
/// regardless of the value of REQUIRE-MATCH.
///
/// If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
///   with point positioned at the end.
///   If it is (STRING . POSITION), the initial input is STRING, but point
///   is placed at _zero-indexed_ position POSITION in STRING.  (*Note*
///   that this is different from `read-from-minibuffer' and related
///   functions, which use one-indexing for POSITION.)  This feature is
///   deprecated--it is best to pass nil for INITIAL-INPUT and supply the
///   default value DEF instead.  The user can yank the default value into
///   the minibuffer easily using \\<minibuffer-local-map>\\[next-history-element].
///
/// HIST, if non-nil, specifies a history list and optionally the initial
///   position in the list.  It can be a symbol, which is the history list
///   variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
///   that case, HISTVAR is the history list variable to use, and HISTPOS
///   is the initial position (the position in the list used by the
///   minibuffer history commands).  For consistency, you should also
///   specify that element of the history as the value of
///   INITIAL-INPUT.  (This is the only case in which you should use
///   INITIAL-INPUT instead of DEF.)  Positions are counted starting from
///   1 at the beginning of the list.  The variable `history-length'
///   controls the maximum length of a history list.
///
/// DEF, if non-nil, is the default value or the list of default values.
///
/// If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits
///   the current input method and the setting of `enable-multibyte-characters'.
///
/// Completion ignores case if the ambient value of
///   `completion-ignore-case' is non-nil.
///
/// See also `completing-read-function'.
#[lisp_fn(min = "2")]
pub fn completing_read(
    prompt: LispObject,
    collection: LispObject,
    predicate: LispObject,
    require_match: LispObject,
    initial_input: LispObject,
    hist: LispObject,
    def: LispObject,
    inherit_input_method: LispObject,
) -> LispObject {
    callN_raw!(
        Ffuncall,
        symbol_value(intern("completing-read-function")),
        prompt,
        collection,
        predicate,
        require_match,
        initial_input,
        hist,
        def,
        inherit_input_method
    )
}

include!(concat!(env!("OUT_DIR"), "/minibuf_exports.rs"));
