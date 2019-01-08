//! font support

use remacs_macros::lisp_fn;

use crate::{
    lisp::defsubr,
    lisp::LispObject,
    obarray::intern,
    remacs_sys::font_match_p as c_font_match_p,
    remacs_sys::Flist_fonts,
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
            wrong_type!(intern("font-extra-type"), extra_type);
        }
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
    match val.into() {
        Some((a, _)) => a,
        None => val,
    }
}

include!(concat!(env!("OUT_DIR"), "/fonts_exports.rs"));
