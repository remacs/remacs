//! wrterm.rs

use libc;
use std::ffi::CString;
use std::ptr;

use crate::{
    fonts::LispFontRef,
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    remacs_sys::globals,
    remacs_sys::resource_types::{RES_TYPE_NUMBER, RES_TYPE_STRING, RES_TYPE_SYMBOL},
    remacs_sys::{
        block_input, fontset_from_font, hashtest_eql, init_frame_faces, make_hash_table,
        register_font_driver, unblock_input, x_get_arg, Display, Fcons, Fcopy_alist, Fprovide,
        Pixmap, Qbackground_color, Qfont, Qfont_backend, Qforeground_color, Qminibuffer, Qname,
        Qnil, Qparent_id, Qt, Qterminal, Qunbound, Qwr, Qx, Vframe_list, WRImage, Window, XColor,
        XrmDatabase, DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
    },
    webrender::{
        font::{FontRef, FONT_DRIVER},
        frame::create_frame,
        output::OutputRef,
        term::wr_term_init,
    },
};
use remacs_macros::lisp_fn;

pub use crate::webrender::display_info::{DisplayInfo, DisplayInfoRef};

pub type DisplayRef = ExternalPtr<Display>;
pub type ImageRef = ExternalPtr<WRImage>;

#[no_mangle]
pub static tip_frame: LispObject = Qnil;

#[no_mangle]
pub static mut wr_display_list: DisplayInfoRef = DisplayInfoRef::new(ptr::null_mut());

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_fontset(output: OutputRef) -> i32 {
    output.get_inner().fontset
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_font(output: OutputRef) -> FontRef {
    output.get_inner().font
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_window_desc(output: OutputRef) -> Window {
    0
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_display_info(output: OutputRef) -> DisplayInfoRef {
    output.get_inner().display_info
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_display(display_info: DisplayInfoRef) -> DisplayRef {
    unimplemented!();
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_baseline_offset(output: OutputRef) -> i32 {
    0
}

#[no_mangle]
pub extern "C" fn wr_defined_color(
    _frame: LispFrameRef,
    _color_name: *mut libc::c_char,
    _color_def: *mut XColor,
    _alloc_p: bool,
) -> bool {
    false
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
    _rdb: XrmDatabase,
    _name: *const libc::c_char,
    _class: *const libc::c_char,
) -> *mut libc::c_char {
    ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn check_x_display_info(obj: LispObject) -> DisplayInfoRef {
    if obj.is_nil() {
        unsafe { wr_display_list }
    } else {
        unimplemented!();
    }
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
#[no_mangle]
pub extern "C" fn x_get_focus_frame(frame: LispFrameRef) -> LispObject {
    let output: OutputRef = unsafe { frame.output_data.wr.into() };
    let dpyinfo = output.get_inner().display_info;

    let focus_frame = dpyinfo.get_inner().focus_frame;

    match focus_frame.is_null() {
        true => Qnil,
        false => focus_frame.into(),
    }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_set_offset(frame: LispFrameRef, xoff: i32, yoff: i32, change_gravity: i32) {
    unimplemented!();
}

#[no_mangle]
pub extern "C" fn x_new_font(
    frame: LispFrameRef,
    font_object: LispObject,
    fontset: i32,
) -> LispObject {
    let font = LispFontRef::from_vectorlike(font_object.as_vectorlike().unwrap()).as_font_mut();
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let fontset = if fontset < 0 {
        unsafe { fontset_from_font(font_object) }
    } else {
        fontset
    };

    output.get_inner().fontset = fontset;

    if output.get_inner().font == font.into() {
        return font_object;
    }

    output.get_inner().font = font.into();

    font_object
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
pub extern "C" fn x_make_frame_visible(mut f: LispFrameRef) {
    f.set_visible(true as u32);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_make_frame_invisible(mut f: LispFrameRef) {
    f.set_visible(false as u32);
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_iconify_frame(mut f: LispFrameRef) {
    f.set_iconified(true);
}

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
pub fn x_create_frame(parms: LispObject) -> LispFrameRef {
    // x_get_arg modifies parms.
    let parms = unsafe { Fcopy_alist(parms) };

    // Use this general default value to start with
    // until we know if this frame has a specified name.
    unsafe {
        globals.Vx_resource_name = globals.Vinvocation_name;
    }

    let mut display = unsafe {
        x_get_arg(
            ptr::null_mut(),
            parms,
            Qterminal,
            ptr::null(),
            ptr::null(),
            RES_TYPE_STRING,
        )
    };

    if display.eq(Qunbound) {
        display = Qnil;
    }

    let mut dpyinfo = check_x_display_info(display);

    if dpyinfo.get_inner().terminal.name == ptr::null_mut() {
        error!("Terminal is not live, can't create new frames on it");
    }

    let kb = dpyinfo.get_inner().terminal.kboard;
    let name = unsafe {
        x_get_arg(
            dpyinfo.as_mut(),
            parms,
            Qname,
            ptr::null(),
            ptr::null(),
            RES_TYPE_STRING,
        )
    };

    if !name.is_string() && !name.eq(Qunbound) && !name.is_nil() {
        error!("Invalid frame name--not a string or nil");
    }

    if name.is_string() {
        unsafe {
            globals.Vx_resource_name = name;
        }
    }

    let mut parent = unsafe {
        x_get_arg(
            dpyinfo.as_mut(),
            parms,
            Qparent_id,
            ptr::null(),
            ptr::null(),
            RES_TYPE_NUMBER,
        )
    };

    if parent.eq(Qunbound) {
        parent = Qnil;
    }

    if parent.is_not_nil() {}
    let tem = unsafe {
        x_get_arg(
            dpyinfo.as_mut(),
            parms,
            Qminibuffer,
            CString::new("minibuffer").unwrap().as_ptr(),
            CString::new("Minibuffer").unwrap().as_ptr(),
            RES_TYPE_SYMBOL,
        )
    };

    let mut frame = create_frame(display, dpyinfo, tem, kb.into());

    unsafe {
        register_font_driver(FONT_DRIVER.clone().as_mut(), frame.as_mut());
    };

    frame.x_default_parameter(
        parms,
        Qfont_backend,
        Qnil,
        "fontBackend",
        "FontBackend",
        RES_TYPE_STRING,
    );

    frame.x_default_parameter(
        parms,
        Qfont,
        "Monospace".into(),
        "font",
        "Font",
        RES_TYPE_STRING,
    );

    frame.x_default_parameter(
        parms,
        Qforeground_color,
        "black".into(),
        "foreground",
        "Foreground",
        RES_TYPE_STRING,
    );
    frame.x_default_parameter(
        parms,
        Qbackground_color,
        "white".into(),
        "background",
        "Background",
        RES_TYPE_STRING,
    );

    unsafe { init_frame_faces(frame.as_mut()) };

    /* Now consider the frame official.  */
    unsafe { Vframe_list = Fcons(frame.into(), Vframe_list) };

    frame
}

/// Open a connection to a display server.
/// DISPLAY is the name of the display to connect to.
/// Optional second arg XRM-STRING is a string of resources in xrdb format.
/// If the optional third arg MUST-SUCCEED is non-nil,
/// terminate Emacs if we can't open the connection.
/// \(In the Nextstep version, the last two arguments are currently ignored.)
#[lisp_fn(min = "1")]
pub fn x_open_connection(
    display: LispObject,
    _xrm_string: LispObject,
    _must_succeed: LispObject,
) -> LispObject {
    let mut display_info = wr_term_init(display);

    // Put this display on the chain.
    unsafe {
        display_info.next = wr_display_list.as_mut();
        wr_display_list = display_info;
    }

    Qnil
}

/// Internal function called by `display-color-p', which see.
#[lisp_fn(min = "0")]
pub fn xw_display_color_p(_terminal: LispObject) -> bool {
    // webrender support color display
    true
}

/// Return t if the X display supports shades of gray.
/// Note that color displays do support shades of gray.
/// The optional argument TERMINAL specifies which display to ask about.
/// TERMINAL should be a terminal object, a frame or a display name (a string).
/// If omitted or nil, that stands for the selected frame's display.
#[lisp_fn(min = "0")]
pub fn x_display_grayscale_p(_terminal: LispObject) -> bool {
    // webrender support shades of gray
    true
}

/// Internal function called by `color-values', which see.
#[lisp_fn(min = "1")]
pub fn xw_color_values(_color: LispObject, _frame: Option<LispFrameRef>) -> LispObject {
    Qnil
}

/// Request that dnd events are made for ClientMessages with ATOM.
/// ATOM can be a symbol or a string.  The ATOM is interned on the display that
/// FRAME is on.  If FRAME is nil, the selected frame is used.
#[lisp_fn(min = "1")]
pub fn x_register_dnd_atom(_atom: LispObject, _frame: LispObject) -> LispObject {
    Qnil
}

/// Change window property PROP to VALUE on the X window of FRAME.
/// PROP must be a string.  VALUE may be a string or a list of conses,
/// numbers and/or strings.  If an element in the list is a string, it is
/// converted to an atom and the value of the atom is used.  If an element
/// is a cons, it is converted to a 32 bit number where the car is the 16
/// top bits and the cdr is the lower 16 bits.
///
/// FRAME nil or omitted means use the selected frame.
/// If TYPE is given and non-nil, it is the name of the type of VALUE.
/// If TYPE is not given or nil, the type is STRING.
/// FORMAT gives the size in bits of each element if VALUE is a list.
/// It must be one of 8, 16 or 32.
/// If VALUE is a string or FORMAT is nil or not given, FORMAT defaults to 8.
/// If OUTER-P is non-nil, the property is changed for the outer X window of
/// FRAME.  Default is to change on the edit X window.
#[lisp_fn(min = "2")]
pub fn x_change_window_property(
    _prop: LispObject,
    value: LispObject,
    _frame: LispObject,
    _type: LispObject,
    _format: LispObject,
    _outer_p: LispObject,
) -> LispObject {
    value
}

fn syms_of_wrfont() {
    unsafe {
        register_font_driver(FONT_DRIVER.clone().as_mut(), ptr::null_mut());
    }
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

    syms_of_wrfont();
}

include!(concat!(env!("OUT_DIR"), "/wrterm_exports.rs"));
