//! Composite sequence support.

use remacs_macros::lisp_fn;

use crate::{
    fns::validate_subarray_rust,
    lisp::LispObject,
    multibyte::LispStringRef,
    remacs_sys::{EmacsInt, Fput_text_property, Qcomposition},
};

/// Internal use only.
///
/// Compose text between indices START and END of STRING, where
/// START and END are treated as in `substring'.  Optional 4th
/// and 5th arguments are COMPONENTS and MODIFICATION-FUNC
/// for the composition.  See `compose-string' for more details.
#[lisp_fn(min = "3")]
pub fn compose_string_internal(
    string: LispStringRef,
    start: Option<EmacsInt>,
    end: Option<EmacsInt>,
    components: LispObject,
    modification_function: LispObject,
) -> LispStringRef {
    let (from, to) = validate_subarray_rust(string.into(), start, end, string.len_chars());

    compose_text_rust(from, to, components, modification_function, string);

    string
}

/// Make text in the region between START and END a composition that
/// has COMPONENTS and MODIFICATION-FUNC.
///
/// If STRING is non-nil, then operate on characters contained between
/// indices START and END in STRING.
pub fn compose_text_rust(
    start: EmacsInt,
    end: EmacsInt,
    components: LispObject,
    modification_function: LispObject,
    string: LispStringRef,
) {
    let prop = (
        (LispObject::from(end - start), components),
        modification_function,
    );

    unsafe {
        Fput_text_property(
            start.into(),
            end.into(),
            Qcomposition,
            prop.into(),
            string.into(),
        );
    }
}

#[no_mangle]
pub extern "C" fn compose_text(
    start: isize,
    end: isize,
    components: LispObject,
    modification_function: LispObject,
    string: LispObject,
) {
    compose_text_rust(
        start as EmacsInt,
        end as EmacsInt,
        components,
        modification_function,
        string.into(),
    );
}

include!(concat!(env!("OUT_DIR"), "/composite_exports.rs"));
