//! Case conversion functions.
use remacs_macros::lisp_fn;

use lisp::defsubr;
use lisp::LispObject;
use remacs_sys::{case_action, casify_object};

/// Convert argument to capitalized form and return that.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest to lower case.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.  If argument
/// is a character, characters which map to multiple code points when
/// cased, e.g. ﬁ, are returned unchanged.
#[lisp_fn]
pub fn capitalize(object: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_CAPITALIZE, object) }
}

/// Convert argument to lower case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.
#[lisp_fn]
pub fn downcase(object: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_DOWN, object) }
}

/// Convert argument to upper case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.  If argument
/// is a character, characters which map to multiple code points when
/// cased, e.g. ﬁ, are returned unchanged.
/// See also `capitalize', `downcase' and `upcase-initials'.
#[lisp_fn]
pub fn upcase(object: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_UP, object) }
}

/* Like Fcapitalize but change only the initials.  */

/// Convert the initial of each word in the argument to upper case.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest are left unchanged.  The
/// argument may be a character or string.  The result has the same
/// type.  The argument object is not altered--the value is a copy.
/// If argument is a character, characters which map to multiple code
/// points when cased, e.g. ﬁ, are returned unchanged.
#[lisp_fn]
pub fn upcase_initials(obj: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_CAPITALIZE_UP, obj) }
}

include!(concat!(env!("OUT_DIR"), "/casefiddle_exports.rs"));
