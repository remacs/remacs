//! Operations on characters.

use lisp::LispObject;
use multibyte::MAX_CHAR;
use remacs_macros::lisp_fn;
use remacs_sys::EmacsInt;

/// Return the character of the maximum code.
#[lisp_fn]
fn max_char() -> LispObject {
    LispObject::from_fixnum(MAX_CHAR as EmacsInt)
}

/// Return non-nil if OBJECT is a character.
/// In Emacs Lisp, characters are represented by character codes, which
/// are non-negative integers.  The function `max-char' returns the
/// maximum character code.
/// usage: (fn OBJECT)
#[lisp_fn(min = "1")]
fn characterp(object: LispObject, _ignore: LispObject) -> LispObject {
    LispObject::from_bool(object.is_character())
}
