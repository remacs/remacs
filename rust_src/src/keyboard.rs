//! keyboard

use remacs_macros::lisp_fn;

use remacs_sys::{Qheader_line, Qhelp_echo, Qmode_line, Qt, Qvertical_line};
use remacs_sys::{make_lispy_position, window_box_left_offset};
use remacs_sys::Fpos_visible_in_window_p;
use remacs_sys::glyph_row_area;

use frames::window_frame_live_or_selected_with_action;
use lisp::{IsLispNatnum, LispCons, LispObject};
use lisp::defsubr;
use windows::window_or_selected_unchecked;

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
pub fn posn_at_point(pos: LispObject, window: LispObject) -> LispObject {
    let window = window_or_selected_unchecked(window);

    let tem = unsafe { Fpos_visible_in_window_p(pos, window, Qt) };
    if tem.is_nil() {
        return LispObject::constant_nil();
    }

    let mut it = tem.iter_cars();
    let x = it.next().unwrap_or_else(|| LispObject::from(0));
    let y = it.next().unwrap_or_else(|| LispObject::from(0));

    let mut y_coord = y.as_fixnum_or_error();
    let x_coord = x.as_fixnum_or_error();

    // Point invisible due to hscrolling?  X can be -1 when a
    // newline in a R2L line overflows into the left fringe.
    if x_coord < -1 {
        return LispObject::constant_nil();
    }
    let aux_info = it.rest();
    if aux_info.is_not_nil() && y_coord < 0 {
        let rtop = it.next()
            .unwrap_or_else(|| LispObject::from(0))
            .as_fixnum_or_error();

        y_coord += rtop;
    }

    posn_at_x_y(
        LispObject::from(x_coord),
        LispObject::from(y_coord),
        window,
        LispObject::constant_nil(),
    )
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

    unsafe { make_lispy_position(frame.as_mut(), LispObject::from(x), LispObject::from(y), 0) }
}

/// Return true if EVENT is a list whose elements are all integers or symbols.
/// Such a list is not valid as an event,
/// but it can be a Lucid-style event type list.
pub fn lucid_event_type_list_p(event: Option<LispCons>) -> bool {
    event.map_or(false, |event| {
        let first = event.car();
        if first.eq(Qhelp_echo) || first.eq(Qvertical_line) || first.eq(Qmode_line)
            || first.eq(Qheader_line)
        {
            return false;
        }

        let mut it = event.as_obj().iter_cars_safe();

        if !it.all(|elt| elt.is_fixnum() || elt.is_symbol()) {
            return false;
        }

        it.rest().is_nil()
    })
}

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

include!(concat!(env!("OUT_DIR"), "/keyboard_exports.rs"));
