//! wrterm.rs

use libc;
use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    remacs_sys::{
        block_input, font, hashtest_eql, make_hash_table, unblock_input, wr_display_info,
        wr_output, Display, Fprovide, Pixmap, Qnil, Qt, Qwr, Qx, WRImage, Window, XColor,
        XrmDatabase, DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
    },
};

type DisplayInfo = wr_display_info;

impl DisplayInfo {
    fn get_ref(&mut self) -> DisplayInfoRef {
        DisplayInfoRef::new(self as *mut DisplayInfo)
    }
}

pub type DisplayInfoRef = ExternalPtr<DisplayInfo>;
unsafe impl Sync for DisplayInfoRef {}

impl From<DisplayInfo> for DisplayInfoRef {
    fn from(mut df: DisplayInfo) -> DisplayInfoRef {
        df.get_ref()
    }
}

pub type OutputRef = ExternalPtr<wr_output>;
pub type DisplayRef = ExternalPtr<Display>;
pub type ImageRef = ExternalPtr<WRImage>;

#[no_mangle]
pub static tip_frame: LispObject = Qnil;

#[no_mangle]
pub static mut wr_display_list: DisplayInfoRef = DisplayInfoRef::new(ptr::null_mut());

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

// FRAME is used only to get a handle on the X display.  We don't pass the
// display info directly because we're called from frame.c, which doesn't
// know about that structure.
#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_get_focus_frame(frame: LispFrameRef) -> LispObject {
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
// This tries to wait until the frame is really visible, depending on
// the value of Vx_wait_for_event_timeout.
// However, if the window manager asks the user where to position
// the frame, this will return before the user finishes doing that.
// The frame will not actually be visible at that time,
// but it will become visible later when the window manager
// finishes with it.
#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_make_frame_visible(f: LispFrameRef) {}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_make_frame_invisible(f: LispFrameRef) {}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_iconify_frame(f: LispFrameRef) {}

// Set the pixel height of the tool bar of frame F to HEIGHT.
#[no_mangle]
pub extern "C" fn x_change_tool_bar_height(_f: LispFrameRef, _hight: i32) {
    // Currently, the webrender based GUI does't support tool bar.
    // So Do nothing.
}

// Call this to change the size of frame F's x-window.
// If CHANGE_GRAVITY, change to top-left-corner window gravity
// for this size change and subsequent size changes.
// Otherwise we leave the window gravity unchanged.
#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_set_window_size(
    f: LispFrameRef,
    change_gravity: bool,
    width: i32,
    height: i32,
    pixelwise: bool,
) {
}

// Move the mouse to position pixel PIX_X, PIX_Y relative to frame F.
#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn frame_set_mouse_pixel_position(f: LispFrameRef, pix_x: i32, pix_y: i32) {
    unsafe { block_input() };
    // set mouse
    unsafe { unblock_input() };
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

/// Open a connection to a display server.
/// DISPLAY is the name of the display to connect to.
/// Optional second arg XRM-STRING is a string of resources in xrdb format.
/// If the optional third arg MUST-SUCCEED is non-nil,
/// terminate Emacs if we can't open the connection.
/// \(In the Nextstep version, the last two arguments are currently ignored.)
#[lisp_fn(min = "1")]
pub fn x_open_connection(
    _display: LispObject,
    _xrm_string: LispObject,
    _must_succeed: LispObject,
) -> LispObject {
    let mut display_info = Box::new(DisplayInfo::default());

    display_info.next = unsafe { wr_display_list.as_mut() };

    unsafe {
        wr_display_list = DisplayInfoRef::new(Box::into_raw(display_info));
    }
    println!("x-open-connection");
    Qnil
}

/// Internal function called by `display-color-p', which see.
#[lisp_fn(min = "0")]
pub fn xw_display_color_p(_terminal: LispObject) -> bool {
    unimplemented!();
}

/// Return t if the X display supports shades of gray.
/// Note that color displays do support shades of gray.
/// The optional argument TERMINAL specifies which display to ask about.
/// TERMINAL should be a terminal object, a frame or a display name (a string).
/// If omitted or nil, that stands for the selected frame's display.
#[lisp_fn(min = "0")]
pub fn x_display_grayscale_p(_terminal: LispObject) -> bool {
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
