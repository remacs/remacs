//! keyboard

use remacs_macros::lisp_fn;
use remacs_sys::{Fpos_visible_in_window_p, Fposn_at_x_y};
use remacs_sys::{Qheader_line, Qhelp_echo, Qmode_line, Qnil, Qt, Qvertical_line};

use lisp::LispObject;
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

    let tem = LispObject::from(unsafe {
        Fpos_visible_in_window_p(pos.to_raw(), window.to_raw(), Qt)
    });
    if tem.is_nil() {
        return LispObject::constant_nil();
    }

    let mut it = tem.iter_cars();
    let x = it.next().unwrap_or_else(|| LispObject::from_fixnum(0));
    let y = it.next().unwrap_or_else(|| LispObject::from_fixnum(0));

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
            .unwrap_or_else(|| LispObject::from_fixnum(0))
            .as_fixnum_or_error();

        y_coord += rtop;
    }

    LispObject::from(unsafe {
        Fposn_at_x_y(
            LispObject::from_fixnum(x_coord).to_raw(),
            LispObject::from_fixnum(y_coord).to_raw(),
            window.to_raw(),
            Qnil,
        )
    })
}

/// Return true if EVENT is a list whose elements are all integers or symbols.
/// Such a list is not valid as an event,
/// but it can be a Lucid-style event type list.
pub fn lucid_event_type_list_p(event: LispObject) -> bool {
    if !event.is_cons() {
        return false;
    }

    let first = event.as_cons_or_error().car();
    if first.eq(LispObject::from(Qhelp_echo)) || first.eq(LispObject::from(Qvertical_line))
        || first.eq(LispObject::from(Qmode_line)) || first.eq(LispObject::from(Qheader_line))
    {
        return false;
    }

    let mut it = event.iter_cars_safe();

    if !it.all(|elt| elt.is_fixnum() || elt.is_symbol()) {
        return false;
    }

    it.rest().is_nil()
}

include!(concat!(env!("OUT_DIR"), "/keyboard_exports.rs"));
