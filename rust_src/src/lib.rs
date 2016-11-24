#![allow(dead_code)]

extern crate libc;

use std::os::raw::c_char;

// EMACS_INT is defined as 'long int' in lisp.h.
type EmacsInt = libc::c_longlong;

// This is dependent on CHECK_LISP_OBJECT_TYPE, a compile time flag,
// but it's usually false.
type LispObject = EmacsInt;

const PSEUDOVECTOR_SIZE_BITS: libc::c_int = 12;
const PSEUDOVECTOR_SIZE_MASK: libc::c_int = (1 << PSEUDOVECTOR_SIZE_BITS) - 1;
const PSEUDOVECTOR_REST_BITS: libc::c_int = 12;
const PSEUDOVECTOR_REST_MASK: libc::c_int = (((1 << PSEUDOVECTOR_REST_BITS) - 1) << PSEUDOVECTOR_SIZE_BITS);
const PSEUDOVECTOR_AREA_BITS: libc::c_int = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS;
const PVEC_TYPE_MASK: libc::c_int = 0x3f << PSEUDOVECTOR_AREA_BITS;

#[repr(C)]
struct VectorLikeHeader {
    size: libc::ptrdiff_t
}

#[repr(C)]
struct LispSubr {
    header: VectorLikeHeader,
    // TODO: lisp.h has an elaborate union here.
    function: *mut libc::c_void,
    min_args: libc::c_short,
    max_args: libc::c_short,
    symbol_name: *const c_char,
    intspec: *const c_char,
    doc: *const c_char,
}

// Use Emacs naming conventions.
#[allow(non_upper_case_globals)]
// TODO: load this from globals.h somehow.
static Qt: i64 = 123;

#[allow(non_snake_case)]
pub unsafe extern "C" fn Freturn_t() -> LispObject {
    Qt
}
