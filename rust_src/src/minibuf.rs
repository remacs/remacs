//! Minibuffer input and completion.

use remacs_macros::lisp_fn;

use crate::{
    buffers::{current_buffer, LispBufferOrName},
    editfns::field_end,
    eval::unbind_to,
    keymap::get_keymap,
    lisp::LispObject,
    lists::{car_safe, cdr_safe, memq},
    multibyte::LispStringRef,
    obarray::{intern, lisp_intern},
    remacs_sys::{
        globals, Qcommandp, Qcustom_variable_p, Qfield, Qminibuffer_completion_table,
        Qminibuffer_history, Qnil, Qt, Vminibuffer_list,
    },
    remacs_sys::{
        make_buffer_string, minibuf_level, minibuf_prompt, minibuf_window, read_minibuf, specbind,
        EmacsInt, Fcopy_sequence,
    },
    symbols::symbol_value,
    textprop::get_char_property,
    threads::{c_specpdl_index, ThreadState},
};

/// Return t if BUFFER is a minibuffer.
/// No argument or nil as argument means use current buffer as BUFFER.
/// BUFFER can be a buffer or a buffer name.
#[lisp_fn(min = "0")]
pub fn minibufferp(buffer_or_name: Option<LispBufferOrName>) -> bool {
    let buffer = buffer_or_name.map_or_else(current_buffer, LispObject::from);
    memq(buffer, unsafe { Vminibuffer_list }).is_not_nil()
}

/// Return the currently active minibuffer window, or nil if none.
#[lisp_fn]
pub fn active_minibuffer_window() -> LispObject {
    unsafe {
        if minibuf_level == 0 {
            Qnil
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
    let buffer = ThreadState::current_buffer_unchecked();
    let beg = buffer.beg() as EmacsInt;
    if memq(buffer.into(), unsafe { Vminibuffer_list }).is_nil() {
        return beg;
    }

    let end = field_end(Some(beg.into()), false, None);
    let buffer_end = buffer.zv as EmacsInt;
    if end == buffer_end && get_char_property(beg, Qfield, Qnil).is_nil() {
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
    unsafe { make_buffer_string(prompt_end, ThreadState::current_buffer_unchecked().zv, true) }
}

/// Return the user input in a minibuffer as a string, without text-properties.
/// If the current buffer is not a minibuffer, return its entire contents.
#[lisp_fn]
pub fn minibuffer_contents_no_properties() -> LispObject {
    let prompt_end = minibuffer_prompt_end() as isize;
    unsafe {
        make_buffer_string(
            prompt_end,
            ThreadState::current_buffer_unchecked().zv,
            false,
        )
    }
}

/// Read a string from the minibuffer, prompting with string PROMPT.
/// The optional second arg INITIAL-CONTENTS is an obsolete alternative to
///   DEFAULT-VALUE.  It normally should be nil in new code, except when
///   HIST is a cons.  It is discussed in more detail below.
///
/// Third arg KEYMAP is a keymap to use whilst reading;
///   if omitted or nil, the default is `minibuffer-local-map'.
///
/// If fourth arg READ is non-nil, interpret the result as a Lisp object
///   and return that object:
///   in other words, do `(car (read-from-string INPUT-STRING))'
///
/// Fifth arg HIST, if non-nil, specifies a history list and optionally
///   the initial position in the list.  It can be a symbol, which is the
///   history list variable to use, or a cons cell (HISTVAR . HISTPOS).
///   In that case, HISTVAR is the history list variable to use, and
///   HISTPOS is the initial position for use by the minibuffer history
///   commands.  For consistency, you should also specify that element of
///   the history as the value of INITIAL-CONTENTS.  Positions are counted
///   starting from 1 at the beginning of the list.
///
/// Sixth arg DEFAULT-VALUE, if non-nil, should be a string, which is used
///   as the default to `read' if READ is non-nil and the user enters
///   empty input.  But if READ is nil, this function does _not_ return
///   DEFAULT-VALUE for empty input!  Instead, it returns the empty string.
///
///   Whatever the value of READ, DEFAULT-VALUE is made available via the
///   minibuffer history commands.  DEFAULT-VALUE can also be a list of
///   strings, in which case all the strings are available in the history,
///   and the first string is the default to `read' if READ is non-nil.
///
/// Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
///  the current input method and the setting of `enable-multibyte-characters'.
///
/// If the variable `minibuffer-allow-text-properties' is non-nil,
///  then the string which is returned includes whatever text properties
///  were present in the minibuffer.  Otherwise the value has no text properties.
///
/// The remainder of this documentation string describes the
/// INITIAL-CONTENTS argument in more detail.  It is only relevant when
/// studying existing code, or when HIST is a cons.  If non-nil,
/// INITIAL-CONTENTS is a string to be inserted into the minibuffer before
/// reading input.  Normally, point is put at the end of that string.
/// However, if INITIAL-CONTENTS is (STRING . POSITION), the initial
/// input is STRING, but point is placed at _one-indexed_ position
/// POSITION in the minibuffer.  Any integer value less than or equal to
/// one puts point at the beginning of the string.  *Note* that this
/// behavior differs from the way such arguments are used in `completing-read'
/// and some related functions, which use zero-indexing for POSITION.
#[lisp_fn(min = "1")]
pub fn read_from_minibuffer(
    prompt: LispStringRef,
    initial_contents: LispObject,
    mut keymap: LispObject,
    read: bool,
    hist: LispObject,
    default_value: LispObject,
    inherit_input_method: bool,
) -> LispObject {
    keymap = if keymap.is_nil() {
        unsafe { globals.Vminibuffer_local_map }
    } else {
        get_keymap(keymap, true, false)
    };

    let (mut histvar, mut histpos) = if hist.is_symbol() {
        (hist, Qnil)
    } else {
        (car_safe(hist), cdr_safe(hist))
    };

    if histvar.is_nil() {
        histvar = Qminibuffer_history
    };
    if histpos.is_nil() {
        histpos = LispObject::from_natnum(0)
    };

    unsafe {
        read_minibuf(
            keymap,
            initial_contents,
            prompt.into(),
            read,
            histvar,
            histpos,
            default_value,
            globals.minibuffer_allow_text_properties,
            inherit_input_method,
        )
    }
}

// Functions that use the minibuffer to read various things.

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
    call!(
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

/// Read a string from the minibuffer, prompting with string PROMPT.
/// If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
///   This argument has been superseded by DEFAULT-VALUE and should normally be nil
///   in new code.  It behaves as INITIAL-CONTENTS in `read-from-minibuffer' (which
///   see).
/// The third arg HISTORY, if non-nil, specifies a history list
///   and optionally the initial position in the list.
/// See `read-from-minibuffer' for details of HISTORY argument.
/// Fourth arg DEFAULT-VALUE is the default value or the list of default values.
///  If non-nil, it is used for history commands, and as the value (or the first
///  element of the list of default values) to return if the user enters the
///  empty string.
/// Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
///  the current input method and the setting of `enable-multibyte-characters'.
#[lisp_fn(min = "1")]
pub fn read_string(
    prompt: LispStringRef,
    initial_input: LispObject,
    history: LispObject,
    default_value: LispObject,
    inherit_input_method: bool,
) -> LispObject {
    let count = c_specpdl_index();

    // Just in case we're in a recursive minibuffer, make it clear that the
    // previous minibuffer's completion table does not apply to the new
    // minibuffer.
    // FIXME: `minibuffer-completion-table' should be buffer-local instead.
    unsafe { specbind(Qminibuffer_completion_table, Qnil) };
    let mut val: LispObject;

    val = read_from_minibuffer(
        prompt,
        initial_input,
        Qnil,
        false,
        history,
        default_value,
        inherit_input_method,
    );

    if let Some(s) = val.as_string() {
        if s.is_empty() && default_value.is_not_nil() {
            val = match default_value.into() {
                None => default_value,
                Some((a, _)) => a,
            }
        }
    }

    unbind_to(count, val)
}

pub fn read_command_or_variable(
    prompt: LispObject,
    default_value: LispObject,
    symbol: LispObject,
) -> LispObject {
    let default_string = if default_value.is_nil() {
        Qnil
    } else if let Some(s) = default_value.as_symbol() {
        s.symbol_name()
    } else {
        default_value
    };

    let name = completing_read(
        prompt,
        unsafe { globals.Vobarray },
        symbol,
        Qt,
        Qnil,
        Qnil,
        default_string,
        Qnil,
    );

    if name.is_nil() {
        name
    } else {
        lisp_intern(name.into(), None)
    }
}

/// Read the name of a command and return as a symbol. */
/// Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element */
/// if it is a list.
#[lisp_fn(min = "1")]
pub fn read_command(prompt: LispObject, default_value: LispObject) -> LispObject {
    read_command_or_variable(prompt, default_value, Qcommandp)
}

/// Read the name of a user option and return it as a symbol.
/// Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
/// if it is a list.
/// A user option, or customizable variable, is one for which
/// `custom-variable-p' returns non-nil.
#[lisp_fn(min = "1")]
pub fn read_variable(prompt: LispObject, default_value: LispObject) -> LispObject {
    read_command_or_variable(prompt, default_value, Qcustom_variable_p)
}

/// Read a string from the terminal, not allowing blanks.
/// Prompt with PROMPT.  Whitespace terminates the input.  If INITIAL is
/// non-nil, it should be a string, which is used as initial input, with
/// point positioned at the end, so that SPACE will accept the input.
/// (Actually, INITIAL can also be a cons of a string and an integer.
/// Such values are treated as in `read-from-minibuffer', but are normally
/// not useful in this function.)
/// Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
/// the current input method and the setting of`enable-multibyte-characters'.
#[lisp_fn(min = "1")]
pub fn read_no_blanks_input(
    prompt: LispStringRef,
    initial: LispObject,
    inherit_input_method: LispObject,
) -> LispObject {
    unsafe {
        read_minibuf(
            globals.Vminibuffer_local_ns_map,
            initial,
            prompt.into(),
            false,
            Qminibuffer_history,
            LispObject::from_fixnum(0),
            Qnil,
            false,
            inherit_input_method.is_not_nil(),
        )
    }
}

include!(concat!(env!("OUT_DIR"), "/minibuf_exports.rs"));
