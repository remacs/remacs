use std::ptr;
use lisp::{self, LispObject};
use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, CHARACTERBITS};

/// Maximum character code
pub const MAX_CHAR: EmacsInt = (1 << CHARACTERBITS as EmacsInt) - 1;

/// Return the character of the maximum code.
#[lisp_fn(name = "max-char")]
fn max_char() -> LispObject {
    lisp::make_number(MAX_CHAR)
}

/// Nonzero iff X is a character.
pub fn CHARACTERP(x: LispObject) -> bool {
    lisp::NATNUMP(x) && lisp::XFASTINT(x) <= MAX_CHAR
}

fn characterp(object: LispObject, _ignore: LispObject) -> LispObject {
    if CHARACTERP(object) {
        LispObject::constant_t()
    } else {
        LispObject::constant_nil()
    }
}

defun!("characterp",
       Fcharacterp(x, y),
       Scharacterp,
       characterp,
       1,
       2,
       ptr::null(),
       "Return non-nil if OBJECT is a character.
In Emacs Lisp, characters are represented by character codes, which
are non-negative integers.  The function `max-char' returns the
maximum character code.
usage: (characterp OBJECT)");
