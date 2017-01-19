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
