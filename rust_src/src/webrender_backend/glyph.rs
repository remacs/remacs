use std::slice;

use crate::{
    lisp::ExternalPtr,
    remacs_sys::{glyph, glyph_string},
};

pub type XChar2b = u16;

pub type GlyphRef = ExternalPtr<glyph>;
pub type GlyphStringRef = ExternalPtr<glyph_string>;

impl<'a> GlyphStringRef {
    pub fn get_chars(&self) -> &'a [XChar2b] {
        let len = self.nchars as usize;

        unsafe { slice::from_raw_parts(self.char2b, len) }
    }

    pub fn first_glyph(&self) -> GlyphRef {
        self.first_glyph.into()
    }
}

impl IntoIterator for GlyphStringRef {
    type Item = GlyphStringRef;
    type IntoIter = GlyphStringIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        GlyphStringIntoIterator {
            next_glyph_string: Some(self),
        }
    }
}

pub struct GlyphStringIntoIterator {
    next_glyph_string: Option<GlyphStringRef>,
}

impl Iterator for GlyphStringIntoIterator {
    type Item = GlyphStringRef;

    fn next(&mut self) -> Option<GlyphStringRef> {
        let new_next = self.next_glyph_string.and_then(|n| {
            if n.next.is_null() {
                None
            } else {
                Some(GlyphStringRef::from(n.next))
            }
        });

        let result = self.next_glyph_string;
        self.next_glyph_string = new_next;

        result
    }
}
