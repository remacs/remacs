use lisp::{self, LispObject};
use remacs_macros::{lisp_fn, lisp_doc};
use remacs_sys::{EmacsInt, CHARACTERBITS};

/// Maximum character code
pub const MAX_CHAR: EmacsInt = (1 << CHARACTERBITS as EmacsInt) - 1;

#[lisp_fn(name = "max-char")]
#[lisp_doc("Return the character of the maximum code.")]
fn max_char() -> LispObject {
    lisp::make_number(MAX_CHAR)
}
