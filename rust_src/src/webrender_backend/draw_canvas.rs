use webrender::{self, api::units::*, api::*};

use super::{
    font::WRFontRef, glyph::GlyphStringRef, output::OutputRef, util::HandyDandyRectBuilder,
};

use crate::remacs_sys::glyph_type;

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

    pub fn draw_glyph_string(&mut self, s: GlyphStringRef) {
        let type_ = s.first_glyph().type_();

        match type_ {
            glyph_type::CHAR_GLYPH => self.draw_char_glyph_string(s),
            glyph_type::STRETCH_GLYPH => self.draw_char_glyph_string(s),
            _ => {}
        }
    }

    fn draw_char_glyph_string(&mut self, s: GlyphStringRef) {
        let font: WRFontRef = s.font.into();
        let font_key = font.font_key;

        let x_start = s.x;
        let y_start = s.y + (font.font.ascent + (s.height - font.font.height) / 2);

        let from = 0 as usize;
        let to = s.nchars as usize;

        let text_count = to - from;

        let font_width = s.width as f32 / (text_count) as f32;

        self.output.display(|builder, api, txn, space_and_clip| {
            let glyph_indices: Vec<u32> =
                s.get_chars()[from..to].iter().map(|c| *c as u32).collect();

            let font_instance_key = api.generate_font_instance_key();

            let glyph_instances = glyph_indices
                .into_iter()
                .enumerate()
                .map(|(i, index)| GlyphInstance {
                    index,
                    point: LayoutPoint::new(x_start as f32 + font_width * i as f32, y_start as f32),
                })
                .collect::<Vec<_>>();

            let pixel_size = unsafe { (*s.font).pixel_size };

            txn.add_font_instance(
                font_instance_key,
                font_key,
                app_units::Au::from_px(pixel_size),
                None,
                None,
                vec![],
            );

            let x = s.x;
            let y = s.y;

            let text_bounds = (x, y).by(s.width as i32, s.height as i32);
            let layout = CommonItemProperties::new(text_bounds, space_and_clip);

            if !glyph_instances.is_empty() {
                builder.push_text(
                    &layout,
                    layout.clip_rect,
                    &glyph_instances,
                    font_instance_key,
                    ColorF::new(0.0, 0.0, 0.0, 1.0),
                    None,
                );
            }
        });
    }
}
