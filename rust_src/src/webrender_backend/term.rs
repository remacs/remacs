use std::ptr;

use glutin::event::{ElementState, Event, KeyboardInput, WindowEvent};

use webrender::api::*;

use super::{
    color::{color_to_pixel, lookup_color_by_name_or_hex, pixel_to_color},
    cursor::{draw_bar_cursor, draw_filled_cursor, draw_hollow_box_cursor},
    display_info::{DisplayInfo, DisplayInfoRef},
    event::create_emacs_event,
    glyph::GlyphStringRef,
    output::OutputRef,
};

use crate::{
    dispnew::redraw_frame,
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    lists::{LispConsCircularChecks, LispConsEndChecks},
    remacs_sys::{
        allocate_kboard, create_terminal, current_kboard, draw_fringe_bitmap_params,
        draw_window_fringes, face_id, frame_parm_handler, fullscreen_type, glyph_row,
        glyph_row_area, glyph_string, initial_kboard, input_event, output_method,
        redisplay_interface, run, terminal, text_cursor_kinds, xlispstrdup, Fcons, Lisp_Frame,
        Lisp_Window, Qbackground_color, Qfullscreen, Qmaximized, Qnil, Qwr, Vframe_list, KBOARD,
    },
    remacs_sys::{
        block_input, change_frame_size, display_and_set_cursor, do_pending_window_change,
        kbd_buffer_store_event_hold, store_frame_param, unblock_input,
        update_face_from_frame_parameter, window_box, x_clear_end_of_line,
        x_clear_window_mouse_face, x_draw_right_divider, x_draw_vertical_border,
        x_fix_overlapping_area, x_get_glyph_overhangs, x_produce_glyphs,
        x_set_bottom_divider_width, x_set_font, x_set_font_backend, x_set_left_fringe,
        x_set_right_divider_width, x_set_right_fringe, x_update_cursor, x_write_glyphs,
    },
    windows::LispWindowRef,
};

pub type TerminalRef = ExternalPtr<terminal>;

impl Default for TerminalRef {
    fn default() -> Self {
        Self::new(ptr::null_mut())
    }
}

pub type KboardRef = ExternalPtr<KBOARD>;

impl KboardRef {
    pub fn add_ref(&mut self) {
        (*self).reference_count = (*self).reference_count + 1;
    }
}

type RedisplayInterfaceRef = ExternalPtr<redisplay_interface>;
unsafe impl Sync for RedisplayInterfaceRef {}

fn get_frame_parm_handlers() -> [frame_parm_handler; 45] {
    // Keep this list in the same order as frame_parms in frame.c.
    // Use None for unsupported frame parameters.
    let handlers: [frame_parm_handler; 45] = [
        None,
        None,
        Some(set_background_color),
        None,
        None,
        Some(set_cursor_color),
        None,
        Some(x_set_font),
        None,
        None,
        None,
        None,
        Some(x_set_right_divider_width),
        Some(x_set_bottom_divider_width),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        Some(x_set_left_fringe),
        Some(x_set_right_fringe),
        None,
        None,
        Some(x_set_font_backend),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
    ];

    handlers
}

lazy_static! {
    static ref REDISPLAY_INTERFACE: RedisplayInterfaceRef = {
        let frame_parm_handlers = Box::new(get_frame_parm_handlers());

        let interface = Box::new(redisplay_interface {
            frame_parm_handlers: (Box::into_raw(frame_parm_handlers)) as *mut Option<_>,
            produce_glyphs: Some(x_produce_glyphs),
            write_glyphs: Some(x_write_glyphs),
            insert_glyphs: None,
            clear_end_of_line: Some(x_clear_end_of_line),
            scroll_run_hook: Some(scroll_run),
            after_update_window_line_hook: Some(after_update_window_line),
            update_window_begin_hook: Some(update_window_begin),
            update_window_end_hook: Some(update_window_end),
            flush_display: Some(flush_display),
            clear_window_mouse_face: Some(x_clear_window_mouse_face),
            get_glyph_overhangs: Some(x_get_glyph_overhangs),
            fix_overlapping_area: Some(x_fix_overlapping_area),
            draw_fringe_bitmap: Some(draw_fringe_bitmap),
            define_fringe_bitmap: None,
            destroy_fringe_bitmap: None,
            compute_glyph_string_overhangs: None,
            draw_glyph_string: Some(draw_glyph_string),
            define_frame_cursor: None,
            clear_frame_area: Some(clear_frame_area),
            draw_window_cursor: Some(draw_window_cursor),
            draw_vertical_window_border: Some(draw_vertical_window_border),
            draw_window_divider: Some(draw_window_divider),
            shift_glyphs_for_insert: None,
            show_hourglass: None,
            hide_hourglass: None,
        });

        RedisplayInterfaceRef::new(Box::into_raw(interface))
    };
}

#[allow(unused_variables)]
extern "C" fn update_window_begin(w: *mut Lisp_Window) {}

extern "C" fn update_window_end(
    window: *mut Lisp_Window,
    cursor_no_p: bool,
    _mouse_face_overwritten_p: bool,
) {
    let mut window: LispWindowRef = window.into();

    if window.pseudo_window_p() {
        return;
    }

    unsafe { block_input() };
    if cursor_no_p {
        unsafe {
            display_and_set_cursor(
                window.as_mut(),
                true,
                window.output_cursor.hpos,
                window.output_cursor.vpos,
                window.output_cursor.x,
                window.output_cursor.y,
            )
        };
    }

    if unsafe { draw_window_fringes(window.as_mut(), true) } {
        if window.right_divider_width() > 0 {
            unsafe { x_draw_right_divider(window.as_mut()) }
        } else {
            unsafe { x_draw_vertical_border(window.as_mut()) }
        }
    }
    unsafe { unblock_input() };

    let frame: LispFrameRef = window.get_frame();
    let mut output: OutputRef = unsafe { frame.output_data.wr.into() };

    output.flush();
}

extern "C" fn flush_display(f: *mut Lisp_Frame) {
    let frame: LispFrameRef = f.into();
    let mut output: OutputRef = unsafe { frame.output_data.wr.into() };

    output.flush();
}

#[allow(unused_variables)]
extern "C" fn after_update_window_line(w: *mut Lisp_Window, desired_row: *mut glyph_row) {
    let window: LispWindowRef = w.into();

    if !unsafe { (*desired_row).mode_line_p() } && !window.pseudo_window_p() {
        unsafe { (*desired_row).set_redraw_fringe_bitmaps_p(true) };
    }
}

#[allow(unused_variables)]
extern "C" fn draw_glyph_string(s: *mut glyph_string) {
    let s: GlyphStringRef = s.into();

    let output: OutputRef = {
        let frame: LispFrameRef = s.f.into();
        unsafe { frame.output_data.wr.into() }
    };

    output.canvas().draw_glyph_string(s);
}

extern "C" fn draw_fringe_bitmap(
    window: *mut Lisp_Window,
    row: *mut glyph_row,
    p: *mut draw_fringe_bitmap_params,
) {
    let window: LispWindowRef = window.into();
    let frame: LispFrameRef = window.get_frame();

    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    output.canvas().draw_fringe_bitmap(row, p);
}

extern "C" fn set_cursor_color(f: *mut Lisp_Frame, arg: LispObject, _old_val: LispObject) {
    let frame: LispFrameRef = f.into();

    let color_str = format!("{}", arg.force_string());
    let color = lookup_color_by_name_or_hex(&color_str);

    if let Some(color) = color {
        let mut output: OutputRef = unsafe { frame.output_data.wr.into() };
        output.cursor_color = color;
    }

    if frame.is_visible() {
        unsafe { x_update_cursor(f, false) };
        unsafe { x_update_cursor(f, true) };
    }
}

extern "C" fn draw_window_cursor(
    window: *mut Lisp_Window,
    row: *mut glyph_row,
    _x: i32,
    _y: i32,
    cursor_type: text_cursor_kinds::Type,
    cursor_width: i32,
    on_p: bool,
    _active_p: bool,
) {
    let mut window: LispWindowRef = window.into();

    if !on_p {
        return;
    }

    window.phys_cursor_type = cursor_type;
    window.set_phys_cursor_on_p(true);

    match cursor_type {
        text_cursor_kinds::FILLED_BOX_CURSOR => {
            draw_filled_cursor(window, row);
        }

        text_cursor_kinds::HOLLOW_BOX_CURSOR => {
            draw_hollow_box_cursor(window, row);
        }

        text_cursor_kinds::BAR_CURSOR => {
            draw_bar_cursor(window, row, cursor_width, false);
        }
        text_cursor_kinds::HBAR_CURSOR => {
            draw_bar_cursor(window, row, cursor_width, true);
        }

        text_cursor_kinds::NO_CURSOR => {
            window.phys_cursor_width = 0;
        }
        _ => panic!("invalid cursor type"),
    }
}

extern "C" fn draw_window_divider(window: *mut Lisp_Window, x0: i32, x1: i32, y0: i32, y1: i32) {
    let window: LispWindowRef = window.into();
    let frame: LispFrameRef = window.get_frame();

    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let face = frame.face_from_id(face_id::WINDOW_DIVIDER_FACE_ID);
    let face_first = frame.face_from_id(face_id::WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID);
    let face_last = frame.face_from_id(face_id::WINDOW_DIVIDER_LAST_PIXEL_FACE_ID);

    let color = match face {
        Some(f) => unsafe { (*f).foreground },
        None => frame.foreground_pixel,
    };

    let color_first = match face_first {
        Some(f) => unsafe { (*f).foreground },
        None => frame.foreground_pixel,
    };

    let color_last = match face_last {
        Some(f) => unsafe { (*f).foreground },
        None => frame.foreground_pixel,
    };

    output
        .canvas()
        .draw_window_divider(color, color_first, color_last, x0, x1, y0, y1);
}

extern "C" fn draw_vertical_window_border(window: *mut Lisp_Window, x: i32, y0: i32, y1: i32) {
    let window: LispWindowRef = window.into();
    let frame: LispFrameRef = window.get_frame();

    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let face = frame.face_from_id(face_id::VERTICAL_BORDER_FACE_ID);

    output.canvas().draw_vertical_window_border(face, x, y0, y1);
}

#[allow(unused_variables)]
extern "C" fn clear_frame_area(f: *mut Lisp_Frame, x: i32, y: i32, width: i32, height: i32) {
    let frame: LispFrameRef = f.into();
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let color = pixel_to_color(frame.background_pixel);

    output.canvas().clear_area(color, x, y, width, height);
}

extern "C" fn set_background_color(f: *mut Lisp_Frame, arg: LispObject, _old_val: LispObject) {
    let mut frame: LispFrameRef = f.into();
    let mut output: OutputRef = unsafe { frame.output_data.wr.into() };

    let color = lookup_color_by_name_or_hex(&format!("{}", arg.as_string().unwrap()))
        .unwrap_or_else(|| ColorF::WHITE);

    let pixel = color_to_pixel(color);

    frame.background_pixel = pixel;
    output.background_color = color;

    unsafe { update_face_from_frame_parameter(frame.as_mut(), Qbackground_color, arg) };

    if frame.is_visible() {
        redraw_frame(frame);
    }
}

extern "C" fn clear_frame(f: *mut Lisp_Frame) {
    let frame: LispFrameRef = f.into();
    let mut output: OutputRef = unsafe { frame.output_data.wr.into() };

    output.clear_display_list_builder();

    let width = frame.pixel_width;
    let height = frame.pixel_height;

    clear_frame_area(f, 0, 0, width, height);
}

extern "C" fn scroll_run(w: *mut Lisp_Window, run: *mut run) {
    let window: LispWindowRef = w.into();
    let frame = window.get_frame();
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let (x, y, width, height) = unsafe {
        let mut x: i32 = 0;
        let mut y: i32 = 0;
        let mut width: i32 = 0;
        let mut height: i32 = 0;

        window_box(
            w,
            glyph_row_area::ANY_AREA,
            &mut x,
            &mut y,
            &mut width,
            &mut height,
        );
        (x, y, width, height)
    };

    let from_y = unsafe { (*run).current_y + window.top_edge_y() };
    let to_y = unsafe { (*run).desired_y + window.top_edge_y() };

    let scroll_height = unsafe { (*run).height };

    output
        .canvas()
        .scroll(x, y, width, height, from_y, to_y, scroll_height);
}

extern "C" fn read_input_event(terminal: *mut terminal, hold_quit: *mut input_event) -> i32 {
    let terminal: TerminalRef = terminal.into();
    let dpyinfo: DisplayInfoRef = unsafe { terminal.display_info.wr }.into();

    let mut dpyinfo = dpyinfo.get_inner();
    let mut output = dpyinfo.output;

    let mut top_frame: LispObject = Qnil;

    for_each_frame!(frame => {
        let frame_output: OutputRef = unsafe { frame.output_data.wr.into() };

        if frame_output == output {
            top_frame = frame.into();
            break;
        }
    });

    let mut count = 0;

    output.poll_events(|e| match e {
        Event::WindowEvent {
            event: WindowEvent::ReceivedCharacter(key_code),
            ..
        } => {
            if let Some(mut iev) = dpyinfo.keyboard_processor.receive_char(key_code, top_frame) {
                unsafe { kbd_buffer_store_event_hold(&mut iev, hold_quit) };
                count += 1;
            }
        }

        Event::WindowEvent {
            event: WindowEvent::ModifiersChanged(state),
            ..
        } => {
            dpyinfo.keyboard_processor.change_modifiers(state);
        }

        Event::WindowEvent {
            event:
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            state,
                            virtual_keycode: Some(key_code),
                            ..
                        },
                    ..
                },
            ..
        } => match state {
            ElementState::Pressed => {
                if let Some(mut iev) = dpyinfo.keyboard_processor.key_pressed(key_code, top_frame) {
                    unsafe { kbd_buffer_store_event_hold(&mut iev, hold_quit) };
                    count += 1;
                }
            }
            ElementState::Released => dpyinfo.keyboard_processor.key_released(),
        },

        Event::WindowEvent {
            event: WindowEvent::Focused(is_focused),
            ..
        } => {
            let mut dpyinfo: DisplayInfoRef = unsafe { terminal.display_info.wr }.into();

            let mut top_frame = top_frame.as_frame().unwrap();

            let focus_frame = if !top_frame.focus_frame.eq(Qnil) {
                top_frame.focus_frame.as_frame().unwrap().as_mut()
            } else {
                top_frame.as_mut()
            };

            dpyinfo.x_highlight_frame = if is_focused {
                focus_frame
            } else {
                ptr::null_mut()
            };

            let event_type = if is_focused {
                crate::remacs_sys::event_kind::FOCUS_IN_EVENT
            } else {
                crate::remacs_sys::event_kind::FOCUS_OUT_EVENT
            };

            let mut event = create_emacs_event(event_type, top_frame.into());

            unsafe { kbd_buffer_store_event_hold(&mut event, hold_quit) };
            count += 1;
        }

        Event::WindowEvent {
            event: WindowEvent::Resized(size),
            ..
        } => {
            let mut frame: LispFrameRef = top_frame.into();
            unsafe {
                change_frame_size(
                    frame.as_mut(),
                    size.width as i32,
                    size.height as i32 - frame.menu_bar_height,
                    false,
                    true,
                    false,
                    true,
                );

                do_pending_window_change(false);
            }
        }

        _ => {}
    });

    count
}

extern "C" fn fullscreen(f: *mut Lisp_Frame) {
    let frame: LispFrameRef = f.into();

    if !frame.is_visible() {
        return;
    }

    let output: OutputRef = unsafe { frame.output_data.wr.into() };
    if frame.want_fullscreen() == fullscreen_type::FULLSCREEN_MAXIMIZED {
        output.maximize();

        unsafe { store_frame_param(f, Qfullscreen, Qmaximized) };
    }
}

fn wr_create_terminal(mut dpyinfo: DisplayInfoRef) -> TerminalRef {
    let terminal_ptr = unsafe {
        create_terminal(
            output_method::output_wr,
            REDISPLAY_INTERFACE.clone().as_mut(),
        )
    };

    let mut terminal = TerminalRef::new(terminal_ptr);

    // Link terminal and dpyinfo together
    terminal.display_info.wr = dpyinfo.as_mut();
    dpyinfo.get_inner().terminal = terminal;

    // Terminal hooks
    // Other hooks are NULL by default.
    terminal.clear_frame_hook = Some(clear_frame);
    terminal.read_socket_hook = Some(read_input_event);
    terminal.fullscreen_hook = Some(fullscreen);

    terminal
}

pub fn wr_term_init(display_name: LispObject) -> DisplayInfoRef {
    let dpyinfo = Box::new(DisplayInfo::new());
    let mut dpyinfo_ref = DisplayInfoRef::new(Box::into_raw(dpyinfo));

    let mut terminal = wr_create_terminal(dpyinfo_ref);

    let mut kboard = KboardRef::new(unsafe { allocate_kboard(Qwr) });
    terminal.kboard = kboard.as_mut();

    // Don't let the initial kboard remain current longer than necessary.
    // That would cause problems if a file loaded on startup tries to
    // prompt in the mini-buffer.
    unsafe {
        if current_kboard == initial_kboard {
            current_kboard = terminal.kboard;
        }
    }

    kboard.add_ref();

    dpyinfo_ref.name_list_element = unsafe { Fcons(display_name, Qnil) };

    // https://lists.gnu.org/archive/html/emacs-devel/2015-11/msg00194.html
    dpyinfo_ref.smallest_font_height = 1;
    dpyinfo_ref.smallest_char_width = 1;

    dpyinfo_ref.resx = 1.0;
    dpyinfo_ref.resy = 1.0;

    // Set the name of the terminal.
    terminal.name = unsafe { xlispstrdup(display_name) };

    dpyinfo_ref
}
