//! Composite sequence support.

use std::convert::TryInto;

use remacs_macros::lisp_fn;

use crate::{
    fns::validate_subarray_rust,
    lisp::LispObject,
    multibyte::LispStringRef,
    remacs_sys::{EmacsInt, Fcons, Fput_text_property, Qcomposition},
};

/// Internal use only.
///
/// Compose text between indices START and END of STRING, where
/// START and END are treated as in `substring'.  Optional 4th
/// and 5th arguments are COMPONENTS and MODIFICATION-FUNC
/// for the composition.  See `compose-string' for more details.
#[lisp_fn(min = "3")]
pub fn compose_string_internal(
    string: LispObject,
    start: LispObject,
    end: LispObject,
    components: LispObject,
    modification_func: LispObject,
) -> LispObject {
    let string = LispStringRef::from(string);

    let (from, to) =
        validate_subarray_rust(LispObject::from(string), start, end, string.len_chars());

    compose_text_rust(
        from,
        to,
        components,
        modification_func,
        LispObject::from(string),
    );

    LispObject::from(string)
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
    string: LispObject,
) {
    unsafe {
        let prop = Fcons(
            Fcons(LispObject::from(end - start), components),
            modification_function,
        );

        Fput_text_property(
            LispObject::from(start),
            LispObject::from(end),
            Qcomposition,
            prop,
            string,
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
        start.try_into().unwrap(),
        end.try_into().unwrap(),
        components,
        modification_function,
        string,
    );
}

include!(concat!(env!("OUT_DIR"), "/composite_exports.rs"));
