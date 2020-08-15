use webrender::{self, api::units::*, api::*};

use super::{
    color::pixel_to_color, font::WRFontRef, glyph::GlyphStringRef, output::OutputRef,
    util::HandyDandyRectBuilder,
};

use crate::remacs_sys::{
    draw_fringe_bitmap_params, face, face_underline_type, glyph_row, glyph_type,
    prepare_face_for_display,
};

impl OutputRef {
    pub fn canvas(self) -> DrawCanvas {
        DrawCanvas::new(self)
    }
}

pub struct DrawCanvas {
    output: OutputRef,
}

impl DrawCanvas {
    pub fn new(output: OutputRef) -> DrawCanvas {
        DrawCanvas { output }
    }

    pub fn draw_glyph_string(&mut self, mut s: GlyphStringRef) {
        unsafe { prepare_face_for_display(s.f, s.face) };

        let face = unsafe { &*s.face };
        s.gc = face.gc;
        s.set_stippled_p(face.stipple != 0);

        let type_ = s.first_glyph().type_();

        match type_ {
            glyph_type::CHAR_GLYPH => self.draw_char_glyph_string(s),
            glyph_type::STRETCH_GLYPH => self.draw_char_glyph_string(s),
            _ => {}
        }
    }

    fn draw_char_glyph_string(&mut self, s: GlyphStringRef) {
        let font: WRFontRef = s.font.into();

        let x_start = s.x;
        let y_start = s.y + (font.font.ascent + (s.height - font.font.height) / 2);

        let from = 0 as usize;
        let to = s.nchars as usize;

        let gc = s.gc;

        self.output.display(|builder, space_and_clip| {
            let glyph_indices: Vec<u32> =
                s.get_chars()[from..to].iter().map(|c| *c as u32).collect();

            let glyph_dimensions = font.get_glyph_dimensions(glyph_indices.clone());

            let mut glyph_instances: Vec<GlyphInstance> = vec![];

            for (i, index) in glyph_indices.into_iter().enumerate() {
                let previous_char_width = if i == 0 {
                    0.0
                } else {
                    let dimension = glyph_dimensions[i - 1];
                    match dimension {
                        Some(d) => d.advance,
                        None => 0.0,
                    }
                };

                let previous_char_start = if i == 0 {
                    x_start as f32
                } else {
                    glyph_instances[i - 1].point.x
                };

                let start = previous_char_start + previous_char_width;

                let glyph_instance = GlyphInstance {
                    index,
                    point: LayoutPoint::new(start, y_start as f32),
                };

                glyph_instances.push(glyph_instance);
            }

            let x = s.x;
            let y = s.y;

            let face = s.face;

            let visible_height = unsafe { (*s.row).visible_height };

            // draw background
            if !s.background_filled_p() {
                let background_bounds = (x, y).by(s.background_width as i32, visible_height);

                let background_color = pixel_to_color(unsafe { (*gc).background } as u64);

                builder.push_rect(
                    &CommonItemProperties::new(background_bounds, space_and_clip),
                    background_color,
                );
            }

            let foreground_color = pixel_to_color(unsafe { (*gc).foreground });

            // draw underline
            if unsafe { (*face).underline_p() } {
                Self::draw_underline(builder, s, font, foreground_color, face, space_and_clip);
            }

            // draw foreground
            if !glyph_instances.is_empty() {
                let visible_rect = (x, y).by(s.width as i32, visible_height);

                builder.push_text(
                    &CommonItemProperties::new(visible_rect, space_and_clip),
                    visible_rect,
                    &glyph_instances,
                    font.font_instance_key,
                    foreground_color,
                    None,
                );
            }
        });
    }

    fn draw_underline(
        builder: &mut DisplayListBuilder,
        s: GlyphStringRef,
        font: WRFontRef,
        foreground_color: ColorF,
        face: *mut face,
        space_and_clip: SpaceAndClipInfo,
    ) {
        let x = s.x;
        let y = s.y;

        let underline_color = if unsafe { (*face).underline_defaulted_p() } {
            foreground_color
        } else {
            pixel_to_color(unsafe { (*face).underline_color })
        };

        let thickness = if font.font.underline_thickness > 0 {
            font.font.underline_thickness
        } else if unsafe { (*face).underline_type() } == face_underline_type::FACE_UNDER_WAVE {
            2
        } else {
            1
        };

        let position = if font.font.underline_position > 0 {
            font.font.underline_position
        } else {
            y + s.height - thickness
        };

        let line_type =
            if unsafe { (*face).underline_type() } == face_underline_type::FACE_UNDER_WAVE {
                LineStyle::Wavy
            } else {
                LineStyle::Solid
            };

        let info =
            CommonItemProperties::new((x, position).by(s.width as i32, thickness), space_and_clip);

        let visible_height = unsafe { (*s.row).visible_height };

        let visible_rect = (x, y).by(s.width as i32, visible_height);

        builder.push_line(
            &info,
            &visible_rect,
            1.0,
            LineOrientation::Horizontal,
            &underline_color,
            line_type,
        );
    }

    pub fn draw_fringe_bitmap(&mut self, _row: *mut glyph_row, p: *mut draw_fringe_bitmap_params) {
        let pos_x = unsafe { (*p).bx };
        let pos_y = unsafe { (*p).by };

        let width = unsafe { (*p).nx };
        let height = unsafe { (*p).ny };

        let face = unsafe { (*p).face };

        let visible_rect = (pos_x, pos_y).by(width, height);

        let background_color = pixel_to_color(unsafe { (*face).background });

        self.output.display(|builder, space_and_clip| {
            builder.push_rect(
                &CommonItemProperties::new(visible_rect, space_and_clip),
                background_color,
            );
        });
    }

    pub fn draw_vertical_window_border(
        &mut self,
        face: Option<*mut face>,
        x: i32,
        y0: i32,
        y1: i32,
    ) {
        // Fix the border height
        // Don't known why the height is short than expected.
        let y1 = y1 + 1;

        let visible_rect = (x, y0).by(1, y1 - y0);

        let color = match face {
            Some(f) => pixel_to_color(unsafe { (*f).foreground }),
            None => ColorF::BLACK,
        };

        self.output.display(|builder, space_and_clip| {
            builder.push_rect(
                &CommonItemProperties::new(visible_rect, space_and_clip),
                color,
            );
        });
    }
}
