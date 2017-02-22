use std::ptr;

use lisp::{self, LispObject};
use remacs_sys::{EmacsInt, CHARACTERBITS};

/// Maximum character code
pub const MAX_CHAR: EmacsInt = (1 << CHARACTERBITS as EmacsInt) - 1;

fn max_char() -> LispObject {
    lisp::make_number(MAX_CHAR)
}

defun!("max-char",
       Fmax_char(),
       Smax_char,
       max_char,
       0,
       0,
       ptr::null(),
       "Return the character of the maximum code.");
