//! font support

use remacs_macros::lisp_fn;

use std::{ffi::CString, mem};

use crate::{
    data,
    frames::{LispFrameLiveOrSelected, LispFrameRef},
    lisp::defsubr,
    lisp::{ExternalPtr, LispObject},
    obarray::intern,
    remacs_sys::font_match_p as c_font_match_p,
    remacs_sys::font_property_index::FONT_TYPE_INDEX,
    remacs_sys::Flist_fonts,
    remacs_sys::{font_add_log, Lisp_Font_Object, Lisp_Type},
    remacs_sys::{pvec_type, FONT_ENTITY_MAX, FONT_OBJECT_MAX, FONT_SPEC_MAX},
    remacs_sys::{EmacsInt, Qfont, Qfont_entity, Qfont_object, Qfont_spec, Qnil},
    vectors::LispVectorlikeRef,
};

// A font is not a type in and of itself, it's just a group of three kinds of
// pseudovector. This newtype allows us to define methods that yield the actual
// font types: Spec, Entity, and Object.
#[repr(transparent)]
pub struct LispFontRef(LispVectorlikeRef);

impl LispFontRef {
    pub fn from_vectorlike(v: LispVectorlikeRef) -> LispFontRef {
        LispFontRef(v)
    }

    pub fn is_font_spec(&self) -> bool {
        self.0.pseudovector_size() == EmacsInt::from(FONT_SPEC_MAX)
    }

    pub fn is_font_entity(&self) -> bool {
        self.0.pseudovector_size() == EmacsInt::from(FONT_ENTITY_MAX)
    }

    pub fn is_font_object(&self) -> bool {
        self.0.pseudovector_size() == EmacsInt::from(FONT_OBJECT_MAX)
    }
}

impl LispObject {
    pub fn is_font(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_FONT))
    }

    pub fn as_font(self) -> Option<LispFontRef> {
        self.as_vectorlike().and_then(|v| {
            if v.is_pseudovector(pvec_type::PVEC_FONT) {
                Some(LispFontRef::from_vectorlike(v))
            } else {
                None
            }
        })
    }

    pub fn is_font_entity(self) -> bool {
        self.is_font()
            && self.as_vectorlike().map_or(false, |vec| {
                vec.pseudovector_size() == EmacsInt::from(FONT_ENTITY_MAX)
            })
    }

    pub fn is_font_object(self) -> bool {
        self.is_font()
            && self.as_vectorlike().map_or(false, |vec| {
                vec.pseudovector_size() == EmacsInt::from(FONT_OBJECT_MAX)
            })
    }

    pub fn is_font_spec(self) -> bool {
        self.is_font()
            && self.as_vectorlike().map_or(false, |vec| {
                vec.pseudovector_size() == EmacsInt::from(FONT_SPEC_MAX)
            })
    }
}

pub enum FontExtraType {
    Spec,
    Entity,
    Object,
}

impl FontExtraType {
    // Needed for wrong_type! that is using a safe predicate. This may change in the future.
    #[allow(unused_unsafe)]
    pub fn from_symbol_or_error(extra_type: LispObject) -> FontExtraType {
        if extra_type.eq(unsafe { Qfont_spec }) {
            FontExtraType::Spec
        } else if extra_type.eq(unsafe { Qfont_entity }) {
            FontExtraType::Entity
        } else if extra_type.eq(unsafe { Qfont_object }) {
            FontExtraType::Object
        } else {
            wrong_type!(LispObject::from(intern("font-extra-type")), extra_type);
        }
    }
}

pub type LispFontObjectRef = ExternalPtr<Lisp_Font_Object>;

impl LispFontObjectRef {
    pub fn as_lisp_obj(self) -> LispObject {
        LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
    }

    pub fn add_log(self, action: &str, result: LispObject) {
        unsafe {
            font_add_log(
                CString::new(action).unwrap().as_ptr(),
                self.as_lisp_obj(),
                result,
            )
        }
    }

    pub fn close(mut self, mut frame: LispFrameRef) {
        if data::aref(self.as_lisp_obj(), FONT_TYPE_INDEX.into()).is_nil() {
            // Already closed
            return;
        }
        self.add_log("close", LispObject::from(false));
        unsafe {
            if let Some(f) = (*self.driver).close {
                f(self.as_mut())
            }
            let mut display_info = &mut *(*frame.output_data.x).display_info;
            debug_assert!(display_info.n_fonts > 0);
            display_info.n_fonts -= 1;
        }
    }
}

impl From<LispFontObjectRef> for LispObject {
    fn from(f: LispFontObjectRef) -> Self {
        f.as_lisp_obj()
    }
}

impl From<LispObject> for LispFontObjectRef {
    fn from(o: LispObject) -> Self {
        o.as_font_object_or_error()
    }
}

impl From<LispObject> for Option<LispFontObjectRef> {
    fn from(o: LispObject) -> Self {
        o.as_font_object()
    }
}

impl LispObject {
    pub fn as_font_object(self) -> Option<LispFontObjectRef> {
        self.as_vectorlike().and_then(|v| {
            if v.is_pseudovector(pvec_type::PVEC_FONT) && self.is_font_object() {
                Some(unsafe { mem::transmute(self) })
            } else {
                None
            }
        })
    }

    pub fn as_font_object_or_error(self) -> LispFontObjectRef {
        self.as_font_object()
            .unwrap_or_else(|| wrong_type!(Qfont_object, self))
    }
}

/// Return t if OBJECT is a font-spec, font-entity, or font-object.
/// Return nil otherwise.
/// Optional 2nd argument EXTRA-TYPE, if non-nil, specifies to check
/// which kind of font it is.  It must be one of `font-spec', `font-entity',
/// `font-object'.
#[lisp_fn(min = "1")]
pub fn fontp(object: LispObject, extra_type: LispObject) -> bool {
    // For compatibility with the C version, checking that object is a font
    // takes priority over checking that extra_type is well-formed.
    object.as_font().map_or(false, |f| {
        if extra_type.is_nil() {
            true
        } else {
            match FontExtraType::from_symbol_or_error(extra_type) {
                FontExtraType::Spec => f.is_font_spec(),
                FontExtraType::Entity => f.is_font_entity(),
                FontExtraType::Object => f.is_font_object(),
            }
        }
    })
}

/// Return t if and only if font-spec SPEC matches with FONT.  FONT is a font-spec, font-entity,
/// or font-object.
#[lisp_fn]
pub fn font_match_p(spec: LispObject, font: LispObject) -> bool {
    if !spec.is_font_spec() {
        wrong_type!(Qfont_spec, spec)
    }
    if !font.is_font() {
        wrong_type!(Qfont, font)
    }
    unsafe { c_font_match_p(spec, font) }
}

/// Return a font-entity matching with FONT-SPEC on the current frame.
/// Optional 2nd argument FRAME, if non-nil, specifies the target frame.
#[lisp_fn(min = "1")]
pub fn find_font(spec: LispObject, frame: LispObject) -> LispObject {
    let val = unsafe { Flist_fonts(spec, frame, LispObject::from(1), Qnil) };
    match val.as_cons() {
        Some(cons) => cons.car(),
        None => val,
    }
}

/// Close FONT-OBJECT
#[lisp_fn]
pub fn close_font(object: LispFontObjectRef, frame: LispFrameLiveOrSelected) {
    let frame: LispFrameRef = frame.into();
    //unsafe { font_close_object(frame.as_mut(), object) }
    object.close(frame)
}

include!(concat!(env!("OUT_DIR"), "/fonts_exports.rs"));
