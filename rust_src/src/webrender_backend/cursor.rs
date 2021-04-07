use super::output::OutputRef;

use crate::{
    remacs_sys::{
        draw_glyphs_face, draw_phys_cursor_glyph, get_phys_cursor_geometry, get_phys_cursor_glyph,
        glyph_row, glyph_type,
    },
    windows::LispWindowRef,
};

pub fn draw_filled_cursor(mut window: LispWindowRef, row: *mut glyph_row) {
    unsafe { draw_phys_cursor_glyph(window.as_mut(), row, draw_glyphs_face::DRAW_CURSOR) };
}

pub fn draw_hollow_box_cursor(mut window: LispWindowRef, row: *mut glyph_row) {
    let cursor_glyph = unsafe { get_phys_cursor_glyph(window.as_mut()) };

    if cursor_glyph.is_null() {
        return;
    }

    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut height: i32 = 0;
    unsafe {
        get_phys_cursor_geometry(
            window.as_mut(),
            row,
            cursor_glyph,
            &mut x,
            &mut y,
            &mut height,
        )
    };
    let width = window.phys_cursor_width;

    let frame = window.get_frame();
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    output.canvas().draw_hollow_box_cursor(x, y, width, height);
}

pub fn draw_bar_cursor(
    mut window: LispWindowRef,
    row: *mut glyph_row,
    cursor_width: i32,
    is_hbar: bool,
) {
    let frame = window.get_frame();
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let cursor_glyph = unsafe { get_phys_cursor_glyph(window.as_mut()) };

    if cursor_glyph.is_null() {
        return;
    }

    if unsafe {
        (*cursor_glyph).type_() == glyph_type::XWIDGET_GLYPH
            || (*cursor_glyph).type_() == glyph_type::IMAGE_GLYPH
    } {
        return;
    }

    let face = unsafe {
        let face_id = (*cursor_glyph).face_id();
        let face_id = std::mem::transmute::<u32, crate::remacs_sys::face_id>(face_id);

        &*frame.face_from_id(face_id).unwrap()
    };

    let (x, y, width, height) = if !is_hbar {
        let mut x = window.text_to_frame_pixel_x(window.phys_cursor.x);
        let y = window.frame_pixel_y(window.phys_cursor.y);

        let width = if cursor_width < 0 {
            frame.cursor_width
        } else {
            cursor_width
        };

        let width = std::cmp::min(unsafe { (*cursor_glyph).pixel_width } as i32, width);

        window.phys_cursor_width = width;
        // If the character under cursor is R2L, draw the bar cursor
        //  on the right of its glyph, rather than on the left.
        if (unsafe { (*cursor_glyph).resolved_level() } & 1) != 0 {
            x += unsafe { (*cursor_glyph).pixel_width } as i32 - width;
        }

        let height = unsafe { (*row).height };

        (x, y, width, height)
    } else {
        let row_height = unsafe { (*row).height } as i32;
        let mut x = window.text_to_frame_pixel_x(window.phys_cursor.x);

        let height = if cursor_width < 0 {
            unsafe { (*row).height }
        } else {
            cursor_width
        };

        let height = std::cmp::min(row_height, height);

        if (unsafe { (*cursor_glyph).resolved_level() } & 1) != 0
            && unsafe { (*cursor_glyph).pixel_width } as i32 > window.phys_cursor_width - 1
        {
            x += unsafe { (*cursor_glyph).pixel_width } as i32 - window.phys_cursor_width + 1;
        }

        let y = window.frame_pixel_y(window.phys_cursor.y + row_height - height);

        let width = window.phys_cursor_width - 1;

        (x, y, width, height)
    };

    output.canvas().draw_bar_cursor(face, x, y, width, height);
}
