//! keyboard

use remacs_macros::lisp_fn;
use remacs_sys::{Fpos_visible_in_window_p, Fposn_at_x_y};
use remacs_sys::{Qnil, Qt};

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

    let x = tem.as_cons()
        .and_then(|c| Some(c.car()))
        .unwrap_or_else(|| LispObject::from_fixnum(0));
    let y = tem.as_cons()
        .and_then(|c| Some(c.cdr()))
        .and_then(|c| c.as_cons())
        .and_then(|c| Some(c.car()))
        .unwrap_or_else(|| LispObject::from_fixnum(0));
    let aux_info = tem.as_cons()
        .and_then(|c| Some(c.cdr()))
        .and_then(|c| c.as_cons())
        .and_then(|c| Some(c.cdr()))
        .unwrap_or_else(|| LispObject::from_fixnum(0));
    let mut y_coord = y.as_fixnum_or_error();
    let x_coord = x.as_fixnum_or_error();

    // Point invisible due to hscrolling?  X can be -1 when a
    // newline in a R2L line overflows into the left fringe.
    if x_coord < -1 {
        return LispObject::constant_nil();
    }
    if aux_info.is_not_nil() && y_coord < 0 {
        let rtop = aux_info
            .as_cons()
            .and_then(|c| Some(c.car()))
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

include!(concat!(env!("OUT_DIR"), "/keyboard_exports.rs"));
