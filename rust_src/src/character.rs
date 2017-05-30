//! Operations on characters.

use lisp::LispObject;
use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, CHARACTERBITS};

/// Maximum character code
pub const MAX_CHAR: EmacsInt = (1 << CHARACTERBITS as EmacsInt) - 1;

/// Return the character of the maximum code.
/// (fn)
#[lisp_fn]
fn max_char() -> LispObject {
    LispObject::from_fixnum(MAX_CHAR)
}

/// Return non-nil if OBJECT is a character.
/// In Emacs Lisp, characters are represented by character codes, which
/// are non-negative integers.  The function `max-char' returns the
/// maximum character code.
/// (fn OBJECT)
#[lisp_fn(min = "1")]
fn characterp(object: LispObject, _ignore: LispObject) -> LispObject {
    LispObject::from_bool(object.is_character())
}
