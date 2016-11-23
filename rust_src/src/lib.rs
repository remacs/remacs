#![feature(const_fn)]

extern crate libc;


// EMACS_INT is defined as 'long int' in lisp.h.
type EmacsInt = libc::c_longlong;

// This is dependent on CHECK_LISP_OBJECT_TYPE, a compile time flag,
// but it's usually false.
type LispObject = EmacsInt;

// Use Emacs naming conventions.
#[allow(non_upper_case_globals)]
// TODO: load this from globals.h somehow.
static Qt: i64 = 123;

#[allow(non_snake_case)]
pub unsafe extern "C" fn Freturn_t() -> LispObject {
    Qt
}
