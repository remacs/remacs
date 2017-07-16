use remacs_macros::lisp_fn;
use remacs_sys::{PseudovecType, Qfont_spec, Qfont_entity, Qfont_object, Qsymbolp,
                 wrong_type_argument};
use lisp::{LispObject, Qnil};

// See font_property_index in font.h for details.
#[allow(non_camel_case_types, dead_code)]
#[repr(C)]
enum font_property_index {
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

const FONT_SPEC_MAX: i32 = font_property_index::FONT_OBJLIST_INDEX as i32;
const FONT_ENTITY_MAX: i32 = font_property_index::FONT_NAME_INDEX as i32;
const FONT_OBJECT_MAX: i32 = (font_property_index::FONT_FILE_INDEX as i32) + 1;

/// Return t if OBJECT is a font-spec, font-entity, or font-object.
/// Return nil otherwise.
/// Optional 2nd argument EXTRA-TYPE, if non-nil, specifies to check
/// which kind of font it is.  It must be one of `font-spec', `font-entity',
/// `font-object'.
#[lisp_fn(min = "1")]
pub fn fontp(object: LispObject, extra_type: LispObject) -> LispObject {
    object.as_vectorlike().map_or(Qnil, |v| {
        if v.is_pseudovector(PseudovecType::PVEC_FONT) {
            if extra_type.eq(Qnil) {
                LispObject::constant_t()
            } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_spec })) {
                LispObject::from_bool(v.pseudovector_size() == FONT_SPEC_MAX as i64)
            } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_entity })) {
                LispObject::from_bool(v.pseudovector_size() == FONT_ENTITY_MAX as i64)
            } else if extra_type.eq(LispObject::from_raw(unsafe { Qfont_object })) {
                LispObject::from_bool(v.pseudovector_size() == FONT_OBJECT_MAX as i64)
            } else {
                // TODO: This should actually be equivalent to
                // intern("font-extra-type"), not Qsymbolp.
                unsafe { wrong_type_argument(Qsymbolp, extra_type.to_raw()) }
            }
        } else {
            // As with the C version, checking that object is a font takes priority
            // over checking that extra_type is well-formed.
            Qnil
        }
    })
}
