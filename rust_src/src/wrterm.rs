//! wrterm.rs

use libc;
use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    remacs_sys::{
        font, hashtest_eql, make_hash_table, wr_display_info, wr_output, Display, Fprovide, Pixmap,
        Qnil, Qt, Qwr, Qx, WRImage, Window, XColor, XrmDatabase, DEFAULT_REHASH_SIZE,
        DEFAULT_REHASH_THRESHOLD,
    },
};

pub type DisplayInfoRef = ExternalPtr<wr_display_info>;
unsafe impl Sync for DisplayInfoRef {}

pub type OutputRef = ExternalPtr<wr_output>;
pub type DisplayRef = ExternalPtr<Display>;
pub type ImageRef = ExternalPtr<WRImage>;

#[no_mangle]
pub static tip_frame: LispObject = Qnil;

#[no_mangle]
pub static wr_display_list: DisplayInfoRef = DisplayInfoRef::new(ptr::null_mut());

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

/// Make a new X window, which is called a "frame" in Emacs terms.
/// Return an Emacs frame object.  PARMS is an alist of frame parameters.
/// If the parameters specify that the frame should not have a minibuffer,
/// and do not specify a specific minibuffer window to use, then
/// `default-minibuffer-frame' must be a frame whose minibuffer can be
/// shared by the new frame.
///
/// This function is an internal primitive--use `make-frame' instead.
#[lisp_fn]
pub fn x_create_frame(_parms: LispObject) {
    unimplemented!();
}

#[no_mangle]
#[allow(unused_doc_comments)]
pub extern "C" fn syms_of_wrterm() {
    // pretend webrender as a X gui backend, so we can reuse the x-win.el logic
    def_lisp_sym!(Qx, "x");
    def_lisp_sym!(Qwr, "wr");
    unsafe {
        Fprovide(Qx, Qnil);
        Fprovide(Qwr, Qnil);
    }

    let x_keysym_table = unsafe {
        make_hash_table(
            hashtest_eql.clone(),
            900,
            DEFAULT_REHASH_SIZE,
            DEFAULT_REHASH_THRESHOLD,
            Qnil,
            false,
        )
    };

    /// Hash table of character codes indexed by X keysym codes.
    #[rustfmt::skip]
    defvar_lisp!(Vx_keysym_table, "x-keysym-table", x_keysym_table);

    /// Which toolkit scroll bars Emacs uses, if any.
    /// A value of nil means Emacs doesn't use toolkit scroll bars.
    /// With the X Window system, the value is a symbol describing the
    /// X toolkit.  Possible values are: gtk, motif, xaw, or xaw3d.
    /// With MS Windows or Nextstep, the value is t.
    #[rustfmt::skip]
    defvar_lisp!(Vx_toolkit_scroll_bars, "x-toolkit-scroll-bars", Qt);
}

include!(concat!(env!("OUT_DIR"), "/wrterm_exports.rs"));
