use std::ptr;

use crate::{
    frame::LispFrameRef,
    lisp::{ExternalPtr, LispObject},
    remacs_sys::{font, font_driver, font_metrics, frame, glyph_string, Qwr},
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
        font_driver.has_char = Some(has_char);
        font_driver.encode_char = Some(encode_char);
        font_driver.text_extents = Some(text_extents);
        font_driver.draw = Some(draw);

        FontDriverRef::new(Box::into_raw(font_driver))
    };
}

#[allow(unused_variables)]
extern "C" fn get_cache(f: *mut frame) -> LispObject {
    let frame = LispFrameRef::new(f);
    let output: OutputRef = unsafe { frame.output_data.wr.into() };

    let dpyinfo = output.get_inner().display_info;

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

#[allow(unused_variables)]
extern "C" fn list(frame: *mut frame, font_spec: LispObject) -> LispObject {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn match_(f: *mut frame, spec: LispObject) -> LispObject {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn list_family(f: *mut frame) -> LispObject {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn open(f: *mut frame, font_entity: LispObject, pixel_size: i32) -> LispObject {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn close(font: *mut font) {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn has_char(font: LispObject, c: i32) -> i32 {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn encode_char(font: *mut font, c: i32) -> u32 {
    unimplemented!();
}

#[allow(unused_variables)]
extern "C" fn text_extents(
    font: *mut font,
    code: *mut u32,
    nglyphs: i32,
    metrics: *mut font_metrics,
) {
    unimplemented!();
}
