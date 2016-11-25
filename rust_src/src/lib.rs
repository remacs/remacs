extern crate libc;

use std::os::raw::c_char;

// EMACS_INT is defined as 'long int' in lisp.h.
type EmacsInt = libc::c_longlong;

// This is dependent on CHECK_LISP_OBJECT_TYPE, a compile time flag,
// but it's usually false.
type LispObject = EmacsInt;

extern {
    static Qt: LispObject;
}

#[no_mangle]
pub unsafe extern "C" fn rust_return_t() -> LispObject {
    println!("hello from rust!");
    Qt
}
