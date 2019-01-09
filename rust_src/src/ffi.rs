//! Module that is used for FFI exports.These calls should NOT be used in Rust directly.
use crate::{data, keyboard, lisp::LispObject, lists, math, remacs_sys::Lisp_Window, windows};

#[no_mangle]
pub extern "C" fn circular_list(obj: LispObject) -> ! {
    lists::circular_list(obj)
}

#[no_mangle]
pub extern "C" fn merge(l1: LispObject, l2: LispObject, pred: LispObject) -> LispObject {
    lists::merge(l1, l2, pred)
}

#[no_mangle]
pub extern "C" fn indirect_function(object: LispObject) -> LispObject {
    data::indirect_function(object)
}

#[no_mangle]
pub extern "C" fn arithcompare(
    obj1: LispObject,
    obj2: LispObject,
    comparison: math::ArithComparison,
) -> LispObject {
    math::arithcompare(obj1, obj2, comparison).into()
}

#[no_mangle]
pub extern "C" fn lucid_event_type_list_p(event: LispObject) -> bool {
    keyboard::lucid_event_type_list_p(event.into())
}

#[no_mangle]
pub extern "C" fn window_wants_mode_line(window: *mut Lisp_Window) -> bool {
    windows::window_wants_mode_line(windows::LispWindowRef::new(window))
}

#[no_mangle]
pub extern "C" fn window_wants_header_line(window: *mut Lisp_Window) -> bool {
    windows::window_wants_header_line(windows::LispWindowRef::new(window))
}
