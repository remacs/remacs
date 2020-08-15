use webrender::{self, api::units::*, api::*};

use super::{
    color::pixel_to_color, font::WRFontRef, glyph::GlyphStringRef, output::OutputRef,
    util::HandyDandyRectBuilder,
};

use crate::remacs_sys::{glyph_type, prepare_face_for_display};

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

            let text_bounds = (x, y).by(s.width as i32, s.height as i32);
            let layout = CommonItemProperties::new(text_bounds, space_and_clip);

            let face = s.face;

            // draw background
            if !s.background_filled_p() {
                let background_bounds = (x, y).by(s.background_width as i32, s.height as i32);

                let background_color = pixel_to_color(unsafe { (*gc).background } as u64);

                builder.push_rect(
                    &CommonItemProperties::new(background_bounds, space_and_clip),
                    background_color,
                );
            }

            // draw foreground
            if !glyph_instances.is_empty() {
                let foreground_color = pixel_to_color(unsafe { (*gc).foreground });
                builder.push_text(
                    &layout,
                    layout.clip_rect,
                    &glyph_instances,
                    font.font_instance_key,
                    foreground_color,
                    None,
                );
            }
        });
    }
}
