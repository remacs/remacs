//! Case conversion functions.
use remacs_macros::lisp_fn;

use lisp::LispObject;
use lisp::defsubr;
use remacs_sys::{casify_object, CaseAction};

/// Convert argument to capitalized form and return that.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest to lower case.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.  If argument
/// is a character, characters which map to multiple code points when
/// cased, e.g. ﬁ, are returned unchanged.
#[lisp_fn]
pub fn capitalize(object: LispObject) -> LispObject {
    unsafe { casify_object(CaseAction::CaseCapitalize, object) }
}

/// Convert argument to lower case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.
#[lisp_fn]
pub fn downcase(object: LispObject) -> LispObject {
    unsafe { casify_object(CaseAction::CaseDown, object) }
}

/// Convert argument to upper case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.  If argument
/// is a character, characters which map to multiple code points when
/// cased, e.g. ﬁ, are returned unchanged.
/// See also `capitalize', `downcase' and `upcase-initials'.
#[lisp_fn]
pub fn upcase(object: LispObject) -> LispObject {
    unsafe { casify_object(CaseAction::CaseUp, object) }
}

include!(concat!(env!("OUT_DIR"), "/casefiddle_exports.rs"));
