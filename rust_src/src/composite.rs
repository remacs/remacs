//! Composite sequence support.

use libc::ptrdiff_t;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::{compose_text, validate_subarray, CHECK_STRING, SCHARS},
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
    let mut from: ptrdiff_t = 0;
    let mut to: ptrdiff_t = 0;

    unsafe {
        CHECK_STRING(string);
        validate_subarray(string, start, end, SCHARS(string), &mut from, &mut to);
        compose_text(from, to, components, modification_func, string);
    }

    return string;
}

include!(concat!(env!("OUT_DIR"), "/composite_exports.rs"));
