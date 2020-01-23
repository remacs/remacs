//! keyboard

use remacs_macros::lisp_fn;

use crate::{
    buffers::current_buffer,
    dispnew::ding_internal,
    emacs::is_daemon,
    eval::{record_unwind_protect, unbind_to},
    frame::{selected_frame, window_frame_live_or_selected_with_action},
    lisp::LispObject,
    lists,
    lists::{car_safe, cdr_safe},
    lists::{LispCons, LispConsCircularChecks, LispConsEndChecks},
    multibyte::LispStringRef,
    numbers::IsLispNatnum,
    remacs_sys::globals,
    remacs_sys::{
        clear_message, command_loop_level, get_input_pending, glyph_row_area,
        interrupt_input_blocked, make_lispy_position, message_log_maybe_newline, minibuf_level,
        output_method, print_error_message, process_special_events, read_key_sequence_vs,
        recursive_edit_1, recursive_edit_unwind, temporarily_switch_to_single_kboard,
        totally_unblock_input, update_mode_lines, window_box_left_offset,
    },
    remacs_sys::{Fdiscard_input, Fkill_emacs, Fpos_visible_in_window_p, Fterpri, Fthrow},
    remacs_sys::{
        Qevent_kind, Qexit, Qexternal_debugging_output, Qheader_line, Qhelp_echo, Qmode_line, Qnil,
        Qt, Qtop_level, Qvertical_line,
    },
    threads::c_specpdl_index,
    windows::{selected_window, LispWindowOrSelected},
};

#[cfg(feature = "window-system")]
use crate::remacs_sys::cancel_hourglass;

#[derive(Clone, Copy, Debug)]
pub struct Event(LispObject);

impl Event {
    // replaces EVENT_HAS_PARAMETERS
    /// Checks if the event has data fields describing it (i.e. a mouse click).
    pub fn has_parameters(self) -> bool {
        self.0.is_cons()
    }

    /// Checks if the event's data fields have contents
    pub fn has_data(self) -> bool {
        match self.0.as_cons() {
            Some(cons) => cons.cdr().is_cons(),
            None => false,
        }
    }

    // replaces EVENT_HEAD
    /// Extract the head from an event.
    /// This works on composite and simple events.
    pub fn head(self) -> LispObject {
        match self.0.as_cons() {
            Some(cons) => cons.car(),
            None => self.0,
        }
    }

    // replaces EVENT_HEAD_KIND
    // Note that the C macro expects an event head, this method expects an event
    // which it exctracts the head from itself.
    /// Extracts the kind from an event's head.
    pub fn head_kind(self) -> LispObject {
        lists::get(self.head().into(), Qevent_kind)
    }

    // replaces EVENT_START
    /// Extracts the starting position from a composite event.
    pub fn start(self) -> LispObject {
        car_safe(cdr_safe(self.0))
    }

    // replaces EVENT_END
    /// Extracts the ending position from a composite event.
    pub fn end(self) -> LispObject {
        car_safe(cdr_safe(cdr_safe(self.0)))
    }
}

impl From<LispObject> for Event {
    fn from(o: LispObject) -> Self {
        Self(o)
    }
}

impl From<Event> for LispObject {
    fn from(o: Event) -> Self {
        o.0
    }
}

/// Return position information for buffer position POS in WINDOW.
/// POS defaults to point in WINDOW; WINDOW defaults to the selected window.
///
/// Return nil if POS is not visible in WINDOW.  Otherwise,
/// the return value is similar to that returned by `event-start' for
/// a mouse click at the upper left corner of the glyph corresponding
/// to POS:
///    (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
///     IMAGE (DX . DY) (WIDTH . HEIGHT))
/// The `posn-' functions access elements of such lists.
#[lisp_fn(min = "0")]
pub fn posn_at_point(pos: LispObject, window: LispWindowOrSelected) -> LispObject {
    let window: LispObject = window.into();

    let tem = unsafe { Fpos_visible_in_window_p(pos, window, Qt) };
    if tem.is_nil() {
        return Qnil;
    }

    let mut it = tem.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off);
    let x = it.next().map_or(0, LispObject::as_fixnum_or_error);
    let mut y = it.next().map_or(0, LispObject::as_fixnum_or_error);

    // Point invisible due to hscrolling?  X can be -1 when a
    // newline in a R2L line overflows into the left fringe.
    if x < -1 {
        return Qnil;
    }
    let aux_info = it.rest();
    if aux_info.is_not_nil() && y < 0 {
        let rtop = it.next().map_or(0, LispObject::as_fixnum_or_error);

        y += rtop;
    }

    posn_at_x_y(x.into(), y.into(), window, Qnil)
}

/// Return position information for pixel coordinates X and Y.
/// By default, X and Y are relative to text area of the selected window.
/// Optional third arg FRAME-OR-WINDOW non-nil specifies frame or window.
/// If optional fourth arg WHOLE is non-nil, X is relative to the left
/// edge of the window.
///
/// The return value is similar to a mouse click position:
///    (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
///     IMAGE (DX . DY) (WIDTH . HEIGHT))
/// The `posn-' functions access elements of such lists.
#[lisp_fn(min = "2")]
pub fn posn_at_x_y(
    objx: LispObject,
    objy: LispObject,
    frame_or_window: LispObject,
    whole: LispObject,
) -> LispObject {
    let x = objx.as_fixnum_or_error();
    if x != -1 {
        x.check_natnum();
    }
    let mut x = x as i32;
    let mut y = objy.as_natnum_or_error() as i32;

    let mut frame = window_frame_live_or_selected_with_action(frame_or_window, |mut w| {
        x += w.left_edge_x();

        if whole.is_nil() {
            x += unsafe { window_box_left_offset(w.as_mut(), glyph_row_area::TEXT_AREA) };
        }

        y = w.frame_pixel_y(y);
    });

    unsafe { make_lispy_position(frame.as_mut(), x.into(), y.into(), 0) }
}

/// Return true if EVENT is a list whose elements are all integers or symbols.
/// Such a list is not valid as an event,
/// but it can be a Lucid-style event type list.
pub fn lucid_event_type_list_p(event: Option<LispCons>) -> bool {
    event.map_or(false, |event| {
        let first = event.car();
        if first.eq(Qhelp_echo)
            || first.eq(Qvertical_line)
            || first.eq(Qmode_line)
            || first.eq(Qheader_line)
        {
            return false;
        }

        let mut it = event.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off);

        if !it.all(|elt| elt.is_fixnum() || elt.is_symbol()) {
            return false;
        }

        it.rest().is_nil()
    })
}

pub fn quit_recursive_edit(val: bool) -> ! {
    unsafe {
        if command_loop_level > 0 || minibuf_level > 0 {
            Fthrow(Qexit, val.into());
        }

        user_error!("No recursive edit is in progress");
    }
}

/// Exit from the innermost recursive edit or minibuffer.
#[lisp_fn(intspec = "")]
pub fn exit_recursive_edit() -> ! {
    quit_recursive_edit(false);
}

/// Abort the command that requested this recursive edit or minibuffer input.
#[lisp_fn(intspec = "")]
pub fn abort_recursive_edit() -> ! {
    quit_recursive_edit(true);
}

/// Invoke the editor command loop recursively.
/// To get out of the recursive edit, a command can throw to `exit' -- for
/// instance (throw \\='exit nil).
/// If you throw a value other than t, `recursive-edit' returns normally
/// to the function that called it.  Throwing a t value causes
/// `recursive-edit' to quit, so that control returns to the command loop
/// one level up.
///
/// This function is called by the editor initialization to begin editing.
#[lisp_fn(intspec = "")]
pub fn recursive_edit() {
    let count = c_specpdl_index();

    // If we enter while input is blocked, don't lock up here.
    // This may happen through the debugger during redisplay.
    if unsafe { interrupt_input_blocked } > 0 {
        return;
    }

    let buf = if unsafe { command_loop_level >= 0 }
        && selected_window()
            .as_window()
            .map_or(true, |w| !w.contents.eq(current_buffer()))
    {
        current_buffer()
    } else {
        Qnil
    };

    // Don't do anything interesting between the increment and the
    // record_unwind_protect!  Otherwise, we could get distracted and
    // never decrement the counter again.
    unsafe {
        command_loop_level += 1;
        update_mode_lines = 17;
        record_unwind_protect(Some(recursive_edit_unwind), buf);

        if command_loop_level > 0 {
            temporarily_switch_to_single_kboard(selected_frame().as_mut());
        }

        recursive_edit_1();
        unbind_to(count, Qnil);
    }
}

#[allow(unused_doc_comments)]
#[no_mangle]
pub extern "C" fn rust_syms_of_keyboard() {
    /// The last command executed.
    /// Normally a symbol with a function definition, but can be whatever was found
    /// in the keymap, or whatever the variable `this-command' was set to by that
    /// command.
    ///
    /// The value `mode-exit' is special; it means that the previous command
    /// read an event that told it to exit, and it did so and unread that event.
    /// In other words, the present command is the event that made the previous
    /// command exit.
    ///
    /// The value `kill-region' is special; it means that the previous command
    /// was a kill command.
    ///
    /// `last-command' has a separate binding for each terminal device.
    /// See Info node `(elisp)Multiple Terminals'.
    defvar_kboard!(Vlast_command_, "last-command");

    /// Same as `last-command', but never altered by Lisp code.
    /// Taken from the previous value of `real-this-command'.
    defvar_kboard!(Vreal_last_command_, "real-last-command");

    /// Last command that may be repeated.
    /// The last command executed that was not bound to an input event.
    /// This is the command `repeat' will try to repeat.
    /// Taken from a previous value of `real-this-command'.  */
    defvar_kboard!(Vlast_repeatable_command_, "last-repeatable-command");
}

/// Produce default output for unhandled error message.
/// Default value of `command-error-function'.
#[lisp_fn]
pub fn command_error_default_function(
    data: LispObject,
    context: LispStringRef,
    signal: LispObject,
) {
    let selected_frame = selected_frame();
    // If the window system or terminal frame hasn't been initialized
    // yet, or we're not interactive, write the message to stderr and
    // exit.
    if !selected_frame.glyphs_initialized_p()
        // The initial frame is a special non-displaying frame. It
        // will be current in daemon mode when there are no frames to
        // display, and in non-daemon mode before the real frame has
        // finished initializing.  If an error is thrown in the latter
        // case while creating the frame, then the frame will never be
        // displayed, so the safest thing to do is write to stderr and
        // quit.  In daemon mode, there are many other potential
        // errors that do not prevent frames from being created, so
        // continuing as normal is better in that case.
        || (!is_daemon() && selected_frame.output_method() == output_method::output_initial)
        || unsafe {globals.noninteractive1 }
    {
        unsafe {
            print_error_message(
                data,
                Qexternal_debugging_output,
                context.const_sdata_ptr(),
                signal,
            );
            Fterpri(Qexternal_debugging_output, Qnil);
            Fkill_emacs(LispObject::from(-1));
        }
    } else {
        unsafe {
            clear_message(true, false);
            Fdiscard_input();
            message_log_maybe_newline();
            ding_internal(true);
            print_error_message(data, Qt, context.const_sdata_ptr(), signal);
        }
    }
}

const READABLE_EVENTS_DO_TIMERS_NOW: i32 = 1;
const READABLE_EVENTS_FILTER_EVENTS: i32 = 2;

/// Return t if command input is currently available with no wait.
/// Actually, the value is nil only if we can be sure that no input is available;
/// if there is a doubt, the value is t.
///
/// If CHECK-TIMERS is non-nil, timers that are ready to run will do so.
#[lisp_fn(min = "0")]
pub fn input_pending_p(check_timers: bool) -> bool {
    unsafe {
        if globals.Vunread_command_events.is_cons()
            || globals.Vunread_post_input_method_events.is_not_nil()
            || globals.Vunread_input_method_events.is_not_nil()
        {
            return true;
        }

        // Process non-user-visible events (Bug#10195).
        process_special_events();

        let val = if check_timers {
            READABLE_EVENTS_DO_TIMERS_NOW
        } else {
            0
        };

        get_input_pending(val | READABLE_EVENTS_FILTER_EVENTS)
    }
}

/// Exit all recursive editing levels.
/// This also exits all active minibuffers.
#[lisp_fn(intspec = "")]
pub fn top_level() {
    unsafe {
        #[cfg(feature = "window-system")]
        {
            if globals.display_hourglass_p {
                cancel_hourglass();
            }
        }

        // Unblock input if we enter with input blocked.  This may happen if
        // redisplay traps e.g. during tool-bar update with input blocked.
        totally_unblock_input();

        Fthrow(Qtop_level, Qnil);
    }
}

/// Like `read-key-sequence' but always return a vector.
#[lisp_fn(min = "1")]
pub fn read_key_sequence_vector(
    prompt: LispObject,
    continue_echo: LispObject,
    dont_downcase_last: LispObject,
    can_return_switch_frame: LispObject,
    cmd_loop: LispObject,
) -> LispObject {
    unsafe {
        read_key_sequence_vs(
            prompt,
            continue_echo,
            dont_downcase_last,
            can_return_switch_frame,
            cmd_loop,
            false,
        )
    }
}

/// Read a sequence of keystrokes and return as a string or vector.
/// The sequence is sufficient to specify a non-prefix command in the
/// current local and global maps.
///
/// First arg PROMPT is a prompt string.  If nil, do not prompt specially.
/// Second (optional) arg CONTINUE-ECHO, if non-nil, means this key echos
/// as a continuation of the previous key.
///
/// The third (optional) arg DONT-DOWNCASE-LAST, if non-nil, means do not
/// convert the last event to lower case.  (Normally any upper case event
/// is converted to lower case if the original event is undefined and the lower
/// case equivalent is defined.)  A non-nil value is appropriate for reading
/// a key sequence to be defined.
///
/// A C-g typed while in this function is treated like any other character,
/// and `quit-flag' is not set.
///
/// If the key sequence starts with a mouse click, then the sequence is read
/// using the keymaps of the buffer of the window clicked in, not the buffer
/// of the selected window as normal.
///
/// `read-key-sequence' drops unbound button-down events, since you normally
/// only care about the click or drag events which follow them.  If a drag
/// or multi-click event is unbound, but the corresponding click event would
/// be bound, `read-key-sequence' turns the event into a click event at the
/// drag's starting position.  This means that you don't have to distinguish
/// between click and drag, double, or triple events unless you want to.
///
/// `read-key-sequence' prefixes mouse events on mode lines, the vertical
/// lines separating windows, and scroll bars with imaginary keys
/// `mode-line', `vertical-line', and `vertical-scroll-bar'.
///
/// Optional fourth argument CAN-RETURN-SWITCH-FRAME non-nil means that this
/// function will process a switch-frame event if the user switches frames
/// before typing anything.  If the user switches frames in the middle of a
/// key sequence, or at the start of the sequence but CAN-RETURN-SWITCH-FRAME
/// is nil, then the event will be put off until after the current key sequence.
///
/// `read-key-sequence' checks `function-key-map' for function key
/// sequences, where they wouldn't conflict with ordinary bindings.  See
/// `function-key-map' for more details.
///
/// The optional fifth argument CMD-LOOP, if non-nil, means
/// that this key sequence is being read by something that will
/// read commands one after another.  It should be nil if the caller
/// will read just one key sequence.
#[lisp_fn(min = "1")]
pub fn read_key_sequence(
    prompt: LispObject,
    continue_echo: LispObject,
    dont_downcase_last: LispObject,
    can_return_switch_frame: LispObject,
    cmd_loop: LispObject,
) -> LispObject {
    unsafe {
        read_key_sequence_vs(
            prompt,
            continue_echo,
            dont_downcase_last,
            can_return_switch_frame,
            cmd_loop,
            true,
        )
    }
}

include!(concat!(env!("OUT_DIR"), "/keyboard_exports.rs"));
