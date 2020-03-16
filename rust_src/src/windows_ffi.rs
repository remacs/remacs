use crate::{remacs_sys::Lisp_Window, windows};

#[no_mangle]
pub extern "C" fn window_wants_mode_line(window: *mut Lisp_Window) -> bool {
    windows::window_wants_mode_line(windows::LispWindowRef::new(window))
}

#[no_mangle]
pub extern "C" fn window_wants_header_line(window: *mut Lisp_Window) -> bool {
    windows::window_wants_header_line(windows::LispWindowRef::new(window))
}
