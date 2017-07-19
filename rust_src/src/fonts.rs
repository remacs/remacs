use remacs_macros::lisp_fn;
use remacs_sys::{font, Qfont_spec, Qfont_entity, Qfont_object, Qsymbolp, wrong_type_argument};
use lisp::LispObject;
use vectors::LispVectorlikeRef;

// A font is not a type in and of itself, it's just a group of three kinds of
// pseudovector. This newtype allows us to define methods that yield the actual
// font types: Spec, Entity, and Object.
pub struct LispFontRef(LispVectorlikeRef);

impl LispFontRef {
    #[inline]
    pub fn from_vectorlike(v: LispVectorlikeRef) -> LispFontRef {
        LispFontRef(v)
    }

    pub fn is_font_spec(self) -> bool {
        self.0.pseudovector_size() == font::FONT_SPEC_MAX as i64
    }

    pub fn is_font_entity(self) -> bool {
        self.0.pseudovector_size() == font::FONT_ENTITY_MAX as i64
    }

    pub fn is_font_object(self) -> bool {
        self.0.pseudovector_size() == font::FONT_OBJECT_MAX as i64
    }
}

pub enum FontExtraType {
    Spec,
    Entity,
    Object,
}

impl FontExtraType {
    pub fn from_symbol_or_error(extra_type: LispObject) -> FontExtraType {
        if extra_type.eq(LispObject::from_raw(unsafe { Qfont_spec })) {
            FontExtraType::Spec
        } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_entity })) {
            FontExtraType::Entity
        } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_object })) {
            FontExtraType::Object
        } else {
            // TODO: This should actually be equivalent to
            // intern("font-extra-type"), not Qsymbolp.
            unsafe { wrong_type_argument(Qsymbolp, extra_type.to_raw()) }
        }
    }
}

/// Return t if OBJECT is a font-spec, font-entity, or font-object.
/// Return nil otherwise.
/// Optional 2nd argument EXTRA-TYPE, if non-nil, specifies to check
/// which kind of font it is.  It must be one of `font-spec', `font-entity',
/// `font-object'.
#[lisp_fn(min = "1")]
pub fn fontp(object: LispObject, extra_type: LispObject) -> LispObject {
    // For compatibility with the C version, checking that object is a font
    // takes priority over checking that extra_type is well-formed.
    object.as_font().map_or(LispObject::constant_nil(), |f| {
        if extra_type.is_nil() {
            LispObject::constant_t()
        } else {
            match FontExtraType::from_symbol_or_error(extra_type) {
                FontExtraType::Spec => LispObject::from_bool(f.is_font_spec()),
                FontExtraType::Entity => LispObject::from_bool(f.is_font_entity()),
                FontExtraType::Object => LispObject::from_bool(f.is_font_object()),
            }
        }
    })
}
