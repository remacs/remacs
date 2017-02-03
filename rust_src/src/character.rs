use std::os::raw::c_char;
use std::ptr;

use lisp;

extern crate libc;

use lisp::{LispObject, LispSubr, EmacsInt, CharBits};

/// Maximum character code
pub const MaxChar: EmacsInt = (1 << CharBits::CHARACTERBITS as EmacsInt) - 1;

fn Fmax_char() -> LispObject {
    lisp::make_number(MaxChar)
}

defun!("max-char",
       Fmax_char,
       Smax_char,
       0,
       0,
       ptr::null(),
       "Return the character of the maximum code.");

/// Is this LispObject a character?
fn CHARACTERP(obj: LispObject) -> bool {
    lisp::NATNUMP(obj) && lisp::XFASTINT(obj) <= MaxChar
}

fn Fcharacterp(obj: LispObject, _ignore: LispObject) -> LispObject {
    if CHARACTERP(obj) {
        unsafe { lisp::Qt }
    } else {
        lisp::Qnil
    }
}

defun!("characterp",
       Fcharacterp,
       Scharacterp,
       1,
       2,
       ptr::null(),
       "Return non-nil if OBJECT is a character.
In Emacs Lisp, characters are represented by character codes, which
are non-negative integers.  The function `max-char' returns the
maximum character code.
usage: (characterp OBJECT)");
