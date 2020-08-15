use std::ptr;

use font_kit::{family_name::FamilyName, properties::Properties, source::SystemSource};

use crate::{
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    multibyte::LispStringRef,
    remacs_sys::{
        font, font_driver, font_make_entity, font_make_object, font_metrics, font_property_index,
        frame, glyph_string, Fcons, Fmake_symbol, Qnil, Qwr,
    },
    symbols::LispSymbolRef,
};

use super::output::OutputRef;

pub type FontRef = ExternalPtr<font>;
impl Default for FontRef {
    fn default() -> Self {
        FontRef::new(ptr::null_mut())
    }
}

type FontDriverRef = ExternalPtr<font_driver>;
unsafe impl Sync for FontDriverRef {}

lazy_static! {
    pub static ref FONT_DRIVER: FontDriverRef = {
        let mut font_driver = Box::new(font_driver::default());

        font_driver.type_ = Qwr;
        font_driver.case_sensitive = true;
        font_driver.get_cache = Some(get_cache);
        font_driver.list = Some(list);
        font_driver.match_ = Some(match_);
        font_driver.list_family = Some(list_family);
        font_driver.open = Some(open);
        font_driver.close = Some(close);
        font_driver.encode_char = Some(encode_char);
        font_driver.text_extents = Some(text_extents);
        font_driver.draw = Some(draw);

        FontDriverRef::new(Box::into_raw(font_driver))
    };
}

/// A newtype for objects we know are font_spec.
#[derive(Clone, Copy)]
pub struct LispFontLike(LispObject);

impl LispFontLike {
    fn aref(&self, index: font_property_index::Type) -> LispObject {
        let vl = self.0.as_vectorlike().unwrap();
        let v = unsafe { vl.as_vector_unchecked() };
        unsafe { v.get_unchecked(index as usize) }
    }

    fn get_family(&self) -> Option<FamilyName> {
        let tem = self.aref(font_property_index::FONT_FAMILY_INDEX);

        if tem.is_nil() {
            None
        } else {
            let symbol_or_string = tem.as_symbol_or_string();
            let string: LispStringRef = symbol_or_string.into();
            match string.to_string().as_ref() {
                "Serif" => Some(FamilyName::Serif),
                "Sans Serif" => Some(FamilyName::SansSerif),
                "Monospace" => Some(FamilyName::Monospace),
                "Cursive" => Some(FamilyName::Cursive),
                "Fantasy" => Some(FamilyName::Fantasy),
                f => Some(FamilyName::Title(f.to_string())),
            }
        }
    }

    fn aset(&self, index: font_property_index::Type, val: LispObject) {
        let vl = self.0.as_vectorlike().unwrap();
        let mut v = unsafe { vl.as_vector_unchecked() };
        unsafe { v.set_unchecked(index as usize, val) };
    }

    fn as_lisp_object(self) -> LispObject {
        self.0
    }
}

impl From<LispObject> for LispFontLike {
    fn from(v: LispObject) -> LispFontLike {
        LispFontLike(v)
    }
}

extern "C" fn get_cache(f: *mut frame) -> LispObject {
    let frame = LispFrameRef::new(f);
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let dpyinfo = output.display_info();

    dpyinfo.name_list_element
}

#[allow(unused_variables)]
extern "C" fn draw(
    s: *mut glyph_string,
    from: i32,
    to: i32,
    x: i32,
    y: i32,
    with_backgroud: bool,
) -> i32 {
    unimplemented!();
}

extern "C" fn list(frame: *mut frame, font_spec: LispObject) -> LispObject {
    // FIXME: implment the real list in future
    match_(frame, font_spec)
}

extern "C" fn match_(_f: *mut frame, spec: LispObject) -> LispObject {
    let font_spec = LispFontLike(spec);
    let family = font_spec.get_family();

    let font = family
        .and_then(|f| {
            SystemSource::new()
                .select_best_match(&[f], &Properties::new())
                .ok()
        })
        .and_then(|h| h.load().ok());

    match font {
        Some(f) => {
            let entity: LispFontLike = unsafe { font_make_entity() }.into();

            // set type
            entity.aset(font_property_index::FONT_TYPE_INDEX, Qwr);

            let family_name: &str = &f.family_name();
            // set family
            entity.aset(font_property_index::FONT_FAMILY_INDEX, unsafe {
                Fmake_symbol(LispObject::from(family_name))
            });

            let full_name: &str = &f.full_name();
            // set name
            entity.aset(
                font_property_index::FONT_NAME_INDEX,
                LispObject::from(full_name),
            );

            unsafe { Fcons(entity.as_lisp_object(), Qnil) }
        }
        None => Qnil,
    }
}

#[allow(unused_variables)]
extern "C" fn list_family(f: *mut frame) -> LispObject {
    unimplemented!();
}

struct WRFont {
    _font: font,
    _i: i32,
}

extern "C" fn open(_f: *mut frame, font_entity: LispObject, pixel_size: i32) -> LispObject {
    let font_entity: LispFontLike = font_entity.into();

    let font_object: LispFontLike = unsafe {
        font_make_object(
            vecsize!(WRFont) as i32,
            font_entity.as_lisp_object(),
            pixel_size,
        )
    }
    .into();

    // set type
    font_object.aset(font_property_index::FONT_TYPE_INDEX, Qwr);

    // set name
    font_object.aset(
        font_property_index::FONT_NAME_INDEX,
        LispSymbolRef::from(font_entity.aref(font_property_index::FONT_FAMILY_INDEX)).symbol_name(),
    );

    let font = font_object
        .as_lisp_object()
        .as_font()
        .unwrap()
        .as_font_mut();

    unsafe {
        (*font).average_width = 10;
        (*font).height = 10;
        (*font).ascent = 10;
        (*font).descent = 10;
    }

    unsafe {
        (*font).driver = FONT_DRIVER.clone().as_mut();
    }

    font_object.as_lisp_object()
}

extern "C" fn close(_font: *mut font) {}

extern "C" fn encode_char(_font: *mut font, c: i32) -> u32 {
    c as u32
}

#[allow(unused_variables)]
extern "C" fn text_extents(
    font: *mut font,
    code: *mut u32,
    nglyphs: i32,
    metrics: *mut font_metrics,
) {
    unsafe {
        (*metrics).lbearing = 10;
        (*metrics).rbearing = 10;
        (*metrics).width = 10;
        (*metrics).ascent = 10;
        (*metrics).descent = 10;
    }
}
