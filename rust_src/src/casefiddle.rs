//! Case conversion functions.
use lisp::defsubr;
use lisp::LispObject;
use remacs_macros::lisp_fn;
use remacs_sys::{casify_object, CaseAction};

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

/// Convert argument to lower case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.
#[lisp_fn]
pub fn downcase(object: LispObject) -> LispObject {
    unsafe { casify_object(CaseAction::CaseDown, object) }
}

include!(concat!(env!("OUT_DIR"), "/casefiddle_exports.rs"));
