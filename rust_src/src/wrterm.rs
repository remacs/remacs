//! wrterm.rs

use libc;
use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    remacs_sys::{
        font, wr_display_info, wr_output, Display, Pixmap, Qnil, WRImage, Window, XColor,
        XrmDatabase,
    },
};

#[allow(dead_code)]
pub struct DisplayInfoRef(*mut wr_display_info);
unsafe impl Sync for DisplayInfoRef {}

pub type OutputRef = ExternalPtr<wr_output>;
pub type DisplayRef = ExternalPtr<Display>;
pub type ImageRef = ExternalPtr<WRImage>;

#[no_mangle]
pub static tip_frame: LispObject = Qnil;

#[no_mangle]
pub static wr_display_list: DisplayInfoRef = DisplayInfoRef(ptr::null_mut());

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_fontset(output: OutputRef) -> i32 {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_font(output: OutputRef) -> *const font {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_window_desc(output: OutputRef) -> Window {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_display_info(output: OutputRef) -> DisplayInfoRef {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_display(display_info: DisplayInfoRef) -> DisplayRef {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_baseline_offset(output: OutputRef) -> i32 {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_defined_color(
    frame: LispFrameRef,
    color_name: *const libc::c_char,
    color_def: XColor,
) -> bool {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_pixel(ximg: ImageRef, x: i32, y: i32) -> i32 {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_free_pixmap(display: DisplayRef, pixmap: Pixmap) -> i32 {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_get_keysym_name(keysym: i32) -> *mut libc::c_char {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_clear_under_internal_border(frame: LispFrameRef) {}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_implicitly_set_name(frame: LispFrameRef, arg: LispObject, oldval: LispObject) {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_set_scroll_bar_default_width(frame: LispFrameRef) {
    // Currently, the web render based GUI does't support scroll bar.
    // So Do nothing.
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_set_scroll_bar_default_height(frame: LispFrameRef) {
    // Currently, the web render based GUI does't support scroll bar.
    // So Do nothing.
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_get_string_resource(
    rdb: XrmDatabase,
    name: *const libc::c_char,
    class: *const libc::c_char,
) -> *mut libc::c_char {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn check_x_display_info(obj: LispObject) -> DisplayInfoRef {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_bitmap_icon(frame: LispFrameRef, icon: LispObject) -> bool {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_focus_frame(frame: LispFrameRef, noactivate: bool) {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_set_offset(frame: LispFrameRef, xoff: i32, yoff: i32, change_gravity: i32) {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_new_font(
    frame: LispFrameRef,
    font_object: LispObject,
    fontset: i32,
) -> LispObject {
    unimplemented!();
}

/// Hide the current tooltip window, if there is any.
/// Value is t if tooltip was open, nil otherwise.
#[lisp_fn]
pub fn x_hide_tip() -> bool {
    unimplemented!();
}

#[no_mangle]
pub extern "C" fn syms_of_wrterm() {
    def_lisp_sym!(Qwr, "wr");
}

include!(concat!(env!("OUT_DIR"), "/wrterm_exports.rs"));
