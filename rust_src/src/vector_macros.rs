#![allow(unused)]

macro_rules! offset_of {
    ($ty:ty, $field:ident) => {
        &(*(ptr::null() as *const $ty)).$field as *const _ as usize
    };
}

// Equivalent to VECSIZE in C
/// If a struct is made to look like a vector, this macro returns the length
/// of the shortest vector that would hold that struct.
macro_rules! vecsize {
    ($ty: ty) => {
        ((::std::mem::size_of::<$ty>() - *crate::vectors::HEADER_SIZE + *crate::vectors::WORD_SIZE
            - 1)
            / *crate::vectors::WORD_SIZE)
    };
}

// Equivalent to PSEUDOVECSIZE in C
/// Like VECSIZE, but used when the pseudo-vector has non-Lisp_Object fields
/// at the end and we need to compute the number of Lisp_Object fields (the
/// ones that the GC needs to trace).
macro_rules! pseudovecsize {
    ($ty: ty, $field: ident) => {
        ((offset_of!($ty, $field) - *crate::vectors::HEADER_SIZE) / *crate::vectors::WORD_SIZE)
    };
}

// Equivalent to `ALLOCATE_PSEUDOVECTOR` in C
/// Allocate partially initialized pseudovector where all Lisp_Object
/// slots are set to Qnil but the rest (if any) is left uninitialized.
macro_rules! allocate_pseudovector {
    ($ty: ty, $field: ident, $vectype: expr) => {
        unsafe {
            crate::remacs_sys::allocate_pseudovector(
                vecsize!($ty) as ::libc::c_int,
                pseudovecsize!($ty, $field) as ::libc::c_int,
                pseudovecsize!($ty, $field) as ::libc::c_int,
                $vectype,
            ) as *mut $ty
        }
    };
}

// Equivalent to `ALLOCATE_ZEROED_PSEUDOVECTOR` in C
/// Allocate fully initialized pseudovector where all Lisp_Object
/// slots are set to Qnil and the rest (if any) is zeroed.
macro_rules! allocate_zeroed_pseudovector {
    ($ty: ty, $field: ident, $vectype: expr) => {
        unsafe {
            crate::remacs_sys::allocate_pseudovector(
                vecsize!($ty) as ::libc::c_int,
                pseudovecsize!($ty, $field) as ::libc::c_int,
                vecsize!($ty) as ::libc::c_int,
                $vectype,
            ) as *mut $ty
        }
    };
}
