use libc::c_int;
use remacs_macros::lisp_fn;
use remacs_sys::{Qfont_spec, Qfont_entity, Qfont_object};
use lisp::LispObject;
use symbols::intern;
use vectors::LispVectorlikeRef;

/// Represents the indices of font properties in the contents of a font
/// vector.
///
/// # C Porting Notes
///
/// The equivalent C enum is `font_property_index`. Since it is meant to
/// represent indices for three different length vectors, the C definition
/// contains duplicate variants, e.g `FONT_OBJLIST_INDEX = FONT_SPEC_MAX`,
/// to represent sizes. These have been moved out of this enum and are
/// available as constant `c_int` values on this module.
#[allow(non_camel_case_types, dead_code)]
#[repr(C)]
pub enum FontPropertyIndex {
    FONT_TYPE_INDEX,
    FONT_FOUNDRY_INDEX,
    FONT_FAMILY_INDEX,
    FONT_ADSTYLE_INDEX,
    FONT_REGISTRY_INDEX,
    FONT_WEIGHT_INDEX,
    FONT_SLANT_INDEX,
    FONT_WIDTH_INDEX,
    FONT_SIZE_INDEX,
    FONT_DPI_INDEX,
    FONT_SPACING_INDEX,
    FONT_AVGWIDTH_INDEX,
    FONT_EXTRA_INDEX,
    // In C, we have FONT_SPEC_MAX, FONT_OBJLIST_INDEX = FONT_SPEC_MAX here.
    FONT_OBJLIST_INDEX,
    // In C, we have FONT_ENTITY_MAX, FONT_NAME_INDEX = FONT_ENTITY_MAX here.
    FONT_NAME_INDEX,
    FONT_FULLNAME_INDEX,
    FONT_FILE_INDEX,
    // In C, we have FONT_OBJECT_MAX here.
}

pub const FONT_SPEC_MAX: c_int = FontPropertyIndex::FONT_OBJLIST_INDEX as c_int;
pub const FONT_ENTITY_MAX: c_int = FontPropertyIndex::FONT_NAME_INDEX as c_int;
pub const FONT_OBJECT_MAX: c_int = (FontPropertyIndex::FONT_FILE_INDEX as c_int) + 1;

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
        self.0.pseudovector_size() == FONT_SPEC_MAX as i64
    }

    pub fn is_font_entity(self) -> bool {
        self.0.pseudovector_size() == FONT_ENTITY_MAX as i64
    }

    pub fn is_font_object(self) -> bool {
        self.0.pseudovector_size() == FONT_OBJECT_MAX as i64
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
        if extra_type.eq(LispObject::from_raw(unsafe { Qfont_spec })) {
            FontExtraType::Spec
        } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_entity })) {
            FontExtraType::Entity
        } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_object })) {
            FontExtraType::Object
        } else {
            wrong_type!(intern("font-extra-type").to_raw(), extra_type);
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
