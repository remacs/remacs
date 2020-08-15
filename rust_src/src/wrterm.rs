//! wrterm.rs

use libc;
use std::ffi::CString;
use std::ptr;

use glutin::{event::VirtualKeyCode, monitor::MonitorHandle};

use remacs_macros::lisp_fn;

use crate::{
    fonts::LispFontRef,
    frame::{window_frame_live_or_selected, LispFrameRef},
    lisp::{ExternalPtr, LispObject},
    lists::{LispConsCircularChecks, LispConsEndChecks},
    remacs_sys::globals,
    remacs_sys::resource_types::{RES_TYPE_NUMBER, RES_TYPE_STRING, RES_TYPE_SYMBOL},
    remacs_sys::{
        adjust_frame_size, block_input, fontset_from_font, hashtest_eql, init_frame_faces,
        make_hash_table, make_monitor_attribute_list, register_font_driver, unblock_input,
        x_get_arg, Display, EmacsInt, Fcons, Fcopy_alist, Fmake_vector, Fprovide, MonitorInfo,
        Pixmap, Qbackground_color, Qfont, Qfont_backend, Qforeground_color, Qleft_fringe,
        Qminibuffer, Qname, Qnil, Qparent_id, Qright_fringe, Qt, Qterminal, Qunbound, Qwr, Qx,
        Qx_create_frame_1, Qx_create_frame_2, Vframe_list, WRImage, Window, XColor, XRectangle,
        XrmDatabase, DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
    },
    webrender_backend::{
        color::{color_to_xcolor, lookup_color_by_name_or_hex},
        font::{FontRef, FONT_DRIVER},
        frame::create_frame,
        keyboard::winit_keycode_emacs_key_name,
        output::OutputRef,
        term::wr_term_init,
    },
};

pub use crate::webrender_backend::display_info::{DisplayInfo, DisplayInfoRef};

pub type DisplayRef = ExternalPtr<Display>;
pub type ImageRef = ExternalPtr<WRImage>;

#[no_mangle]
pub static tip_frame: LispObject = Qnil;

#[no_mangle]
pub static mut wr_display_list: DisplayInfoRef = DisplayInfoRef::new(ptr::null_mut());

#[no_mangle]
pub extern "C" fn wr_get_fontset(output: OutputRef) -> i32 {
    output.fontset
}

#[no_mangle]
pub extern "C" fn wr_get_font(output: OutputRef) -> FontRef {
    output.font
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_window_desc(output: OutputRef) -> Window {
    0
}

#[no_mangle]
pub extern "C" fn wr_get_display_info(output: OutputRef) -> DisplayInfoRef {
    output.display_info()
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_display(display_info: DisplayInfoRef) -> DisplayRef {
    DisplayRef::new(ptr::null_mut())
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn wr_get_baseline_offset(output: OutputRef) -> i32 {
    0
}

#[no_mangle]
pub extern "C" fn wr_defined_color(
    _frame: LispFrameRef,
    color_name: *mut libc::c_char,
    color_def: *mut XColor,
    _alloc_p: bool,
) -> bool {
    let c_color = unsafe { CString::from_raw(color_name) };

    let color = c_color
        .to_str()
        .ok()
        .and_then(|color| lookup_color_by_name_or_hex(color));

    // throw back the c pointer
    c_color.into_raw();

    match color {
        Some(c) => {
            color_to_xcolor(c, color_def);
            true
        }
        _ => false,
    }
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

#[no_mangle]
pub extern "C" fn x_get_keysym_name(keysym: i32) -> *mut libc::c_char {
    let name =
        winit_keycode_emacs_key_name(unsafe { std::mem::transmute::<i32, VirtualKeyCode>(keysym) });

    name as *mut libc::c_char
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_clear_under_internal_border(frame: LispFrameRef) {}

// This function should be called by Emacs redisplay code to set the
// name; names set this way will never override names set by the user's
// lisp code.
#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_implicitly_set_name(
    mut frame: LispFrameRef,
    arg: LispObject,
    oldval: LispObject,
) {
    if frame.name.is_nil() {
        frame.name = arg;
    }
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
    let dpyinfo = output.display_info();

    let focus_frame = dpyinfo.get_inner().focus_frame;

    match focus_frame.is_null() {
        true => Qnil,
        false => focus_frame.into(),
    }
}

#[allow(unused_variables)]
#[no_mangle]
pub extern "C" fn x_set_offset(frame: LispFrameRef, xoff: i32, yoff: i32, change_gravity: i32) {}

#[no_mangle]
pub extern "C" fn x_new_font(
    mut frame: LispFrameRef,
    font_object: LispObject,
    fontset: i32,
) -> LispObject {
    let font = LispFontRef::from_vectorlike(font_object.as_vectorlike().unwrap()).as_font_mut();
    let mut output: OutputRef = unsafe { frame.output_data.wr.into() };

    let fontset = if fontset < 0 {
        unsafe { fontset_from_font(font_object) }
    } else {
        fontset
    };

    output.fontset = fontset;

    if output.font == font.into() {
        return font_object;
    }

    output.font = font.into();

    frame.line_height = unsafe { (*font).height };
    frame.column_width = unsafe { (*font).average_width };

    font_object
}

// This tries to wait until the frame is really visible, depending on
// the value of Vx_wait_for_event_timeout.
// However, if the window manager asks the user where to position
// the frame, this will return before the user finishes doing that.
// The frame will not actually be visible at that time,
// but it will become visible later when the window manager
// finishes with it.
#[no_mangle]
pub extern "C" fn x_make_frame_visible(mut f: LispFrameRef) {
    f.set_visible(true as u32);

    let output: OutputRef = unsafe { f.output_data.wr.into() };
    output.show_window()
}

#[no_mangle]
pub extern "C" fn x_make_frame_invisible(mut f: LispFrameRef) {
    f.set_visible(false as u32);

    let output: OutputRef = unsafe { f.output_data.wr.into() };
    output.hide_window()
}

#[no_mangle]
pub extern "C" fn x_iconify_frame(mut f: LispFrameRef) {
    f.set_iconified(true);

    let output: OutputRef = unsafe { f.output_data.wr.into() };
    output.hide_window()
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

    frame.x_default_parameter(
        parms,
        Qleft_fringe,
        Qnil,
        "leftFringe",
        "LeftFringe",
        RES_TYPE_NUMBER,
    );

    frame.x_default_parameter(
        parms,
        Qright_fringe,
        Qnil,
        "rightFringe",
        "RightFringe",
        RES_TYPE_NUMBER,
    );

    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let output_size = output.get_inner_size();

    frame.pixel_width = output_size.width as i32;
    frame.pixel_height = output_size.height as i32;

    frame.text_width = frame.pixel_to_text_width(output_size.width as i32);
    frame.text_height = frame.pixel_to_text_height(output_size.height as i32);

    frame.set_can_x_set_window_size(true);

    unsafe {
        adjust_frame_size(
            frame.as_mut(),
            frame.text_width,
            frame.text_height,
            5,
            true,
            Qx_create_frame_1,
        )
    }

    unsafe {
        adjust_frame_size(
            frame.as_mut(),
            frame.text_width,
            frame.text_height,
            0,
            true,
            Qx_create_frame_2,
        )
    }

    unsafe { init_frame_faces(frame.as_mut()) };

    /* Now consider the frame official.  */
    unsafe { Vframe_list = Fcons(frame.into(), Vframe_list) };

    let output: OutputRef = unsafe { frame.output_data.wr.into() };
    let mut dpyinfo = output.display_info();

    dpyinfo.x_highlight_frame = frame.as_mut();

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

/// Return the number of color cells of the X display TERMINAL.
/// The optional argument TERMINAL specifies which display to ask about.
/// TERMINAL should be a terminal object, a frame or a display name (a string).
/// If omitted or nil, that stands for the selected frame's display.
#[lisp_fn(min = "0")]
pub fn x_display_color_cells(obj: LispObject) -> EmacsInt {
    // FIXME: terminal object or display name (a string) is not implemented
    let frame = window_frame_live_or_selected(obj);

    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let mut color_bits = output.get_color_bits();

    // Truncate color_bits to 24 to avoid integer overflow.
    // Some displays says 32, but only 24 bits are actually significant.
    // There are only very few and rare video cards that have more than
    // 24 significant bits.  Also 24 bits is more than 16 million colors,
    // it "should be enough for everyone".
    if color_bits > 24 {
        color_bits = 24;
    }

    (2 as EmacsInt).pow(color_bits as u32)
}

/// Return the number of bitplanes of the X display TERMINAL.
/// The optional argument TERMINAL specifies which display to ask about.
/// TERMINAL should be a terminal object, a frame or a display name (a string).
/// If omitted or nil, that stands for the selected frame's display.
/// \(On MS Windows, this function does not accept terminal objects.)
#[lisp_fn(min = "0")]
pub fn x_display_planes(obj: LispObject) -> EmacsInt {
    // FIXME: terminal object or display name (a string) is not implemented
    let frame = window_frame_live_or_selected(obj);

    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let color_bits = output.get_color_bits();

    color_bits as EmacsInt
}

/// Send the size hints for frame FRAME to the window manager.
/// If FRAME is omitted or nil, use the selected frame.
/// Signal error if FRAME is not an X frame.
#[lisp_fn(min = "0")]
pub fn x_wm_set_size_hint(_frame: LispObject) {}

/// Return the visual class of the X display TERMINAL.
/// The value is one of the symbols `static-gray', `gray-scale',
/// `static-color', `pseudo-color', `true-color', or `direct-color'.
/// \(On MS Windows, the second and last result above are not possible.)
///
/// The optional argument TERMINAL specifies which display to ask about.
/// TERMINAL should a terminal object, a frame or a display name (a string).
/// If omitted or nil, that stands for the selected frame's display.
/// \(On MS Windows, this function does not accept terminal objects.)
#[lisp_fn(min = "0")]
pub fn x_display_visual_class(_terminal: LispObject) -> LispObject {
    new_unibyte_string!("true-color")
}

pub fn webrender_monitor_to_emacs_monitor(m: MonitorHandle) -> (MonitorInfo, Option<CString>) {
    let dpi_factor = m.scale_factor();

    let physical_pos = m.position();
    let physical_size = m.size();

    let logical_pos = physical_pos.to_logical::<i32>(dpi_factor);
    let logical_size = physical_size.to_logical::<u32>(dpi_factor);

    let geom = XRectangle {
        x: logical_pos.x,
        y: logical_pos.y,
        width: logical_size.width,
        height: logical_size.height,
    };

    let physical_size: (u32, u32) = physical_size.into();

    let name = m.name().and_then(|s| CString::new(s).ok());

    let name_c_ptr = name
        .as_ref()
        .map(|s| s.as_ptr())
        .unwrap_or_else(|| ptr::null_mut());

    let monitor_info = MonitorInfo {
        geom,
        work: geom,
        mm_width: physical_size.0 as i32,
        mm_height: physical_size.1 as i32,
        name: name_c_ptr as *mut i8,
    };

    (monitor_info, name)
}

/// Return a list of physical monitor attributes on the X display TERMINAL.
///
/// The optional argument TERMINAL specifies which display to ask about.
/// TERMINAL should be a terminal object, a frame or a display name (a string).
/// If omitted or nil, that stands for the selected frame's display.
///
/// In addition to the standard attribute keys listed in
/// `display-monitor-attributes-list', the following keys are contained in
/// the attributes:
///
/// source -- String describing the source from which multi-monitor
/// information is obtained, one of \"Gdk\", \"XRandr\",
/// \"Xinerama\", or \"fallback\"
///
/// Internal use only, use `display-monitor-attributes-list' instead.
#[lisp_fn(min = "0")]
pub fn x_display_monitor_attributes_list(terminal: LispObject) -> LispObject {
    let dpyinfo = check_x_display_info(terminal);

    let output = dpyinfo.get_inner().output;

    let monitors: Vec<_> = output.get_available_monitors().collect();
    let primary_monitor = output.get_primary_monitor();

    let mut primary_monitor_index = 0;

    for (i, m) in monitors.iter().enumerate() {
        if m.name() == primary_monitor.name() {
            primary_monitor_index = i;
            break;
        }
    }

    let emacs_monitor_infos: Vec<_> = monitors
        .iter()
        .map(|m| webrender_monitor_to_emacs_monitor(m.clone()))
        .collect();

    let mut emacs_monitors: Vec<_> = emacs_monitor_infos.iter().map(|(m, _)| m.clone()).collect();

    let n_monitors = monitors.len();
    let mut monitor_frames = unsafe { Fmake_vector(n_monitors.into(), Qnil).as_vector_unchecked() };

    for_each_frame!(f => {
        let frame: LispFrameRef = f.into();
        let output: OutputRef = unsafe { frame.output_data.wr.into() };

        let output_pos = output.get_position().unwrap();

        let mut window_at_monitor_index = 0;

        for (i, m) in monitors.iter().enumerate() {

            let monitor_pos = m.position();
            let monitor_size = m.size();

            if (output_pos.x - monitor_pos.x) < monitor_size.width as i32
                && (output_pos.y - monitor_pos.y) < monitor_size.height as i32
            {
                window_at_monitor_index = i;
                break;
            }
        }

        monitor_frames.set(window_at_monitor_index, unsafe {
            Fcons(frame.into(), monitor_frames.get(window_at_monitor_index))
        });

    });

    let source = CString::new("fallback").unwrap();

    unsafe {
        make_monitor_attribute_list(
            emacs_monitors.as_mut_ptr(),
            n_monitors as i32,
            primary_monitor_index as i32,
            monitor_frames.into(),
            source.as_ptr(),
        )
    }
}

/// Assert an X selection of type SELECTION and value VALUE.
/// SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
/// \(Those are literal upper-case symbol names, since that's what X expects.)
/// VALUE is typically a string, or a cons of two markers, but may be
/// anything that the functions on `selection-converter-alist' know about.
///
/// FRAME should be a frame that should own the selection.  If omitted or
/// nil, it defaults to the selected frame.
///
/// On Nextstep, FRAME is unused.
#[lisp_fn(min = "2")]
pub fn x_own_selection_internal(
    _selection: LispObject,
    _value: LispObject,
    _frame: LispObject,
) -> LispObject {
    Qnil
}

/// Return text selected from some X window.
/// SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
/// \(Those are literal upper-case symbol names, since that's what X expects.)
/// TARGET-TYPE is the type of data desired, typically `STRING'.
///
/// TIME-STAMP is the time to use in the XConvertSelection call for foreign
/// selections.  If omitted, defaults to the time for the last event.
///
/// TERMINAL should be a terminal object or a frame specifying the X
/// server to query.  If omitted or nil, that stands for the selected
/// frame's display, or the first available X display.
///
/// On Nextstep, TIME-STAMP and TERMINAL are unused.
#[lisp_fn(min = "2")]
pub fn x_get_selection_internal(
    _selection_symbol: LispObject,
    _target_type: LispObject,
    _time_stamp: LispObject,
    _terminal: LispObject,
) -> LispObject {
    Qnil
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
