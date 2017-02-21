use std::ptr;

use lisp::{self, LispObject, CharBits};
use remacs_sys::EmacsInt;

/// Maximum character code
pub const MaxChar: EmacsInt = (1 << CharBits::CHARACTERBITS as EmacsInt) - 1;

fn max_char() -> LispObject {
    lisp::make_number(MaxChar)
}

defun!("max-char",
       Fmax_char(),
       Smax_char,
       max_char,
       0,
       0,
       ptr::null(),
       "Return the character of the maximum code.");
