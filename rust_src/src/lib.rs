#![feature(const_fn)]

extern crate libc;

// Use Emacs naming conventions.
#[allow(non_upper_case_globals)]

// EMACS_INT is defined as 'long int' in lisp.h.
type EmacsInt = libc::c_longlong;

// TODO: typedef EMACS_INT to long int
//
// note this is dependent on platform and compiler flags passed when
// compiling emacs.

const fn builtin_lisp_symbol(index: EmacsInt) -> EmacsInt {
    index
}

// First, we need a reference to Qt, the t symbol.
// TODO: what generates globals.h, and where does the number 926 come from?
static Qt: i64 = builtin_lisp_symbol(926);
