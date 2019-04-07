macro_rules! set_vector_type {
    ($var:ident, $vectype:expr) => {
        let _cast_tmp = ($var).as_mut() as *mut crate::remacs_sys::Lisp_Vector;
        unsafe {
            (*_cast_tmp).header.size = (*_cast_tmp).header.size
                | (crate::remacs_sys::PSEUDOVECTOR_FLAG as isize)
                | (($vectype as isize)
                    << crate::remacs_sys::More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS);
        }
    };
}

macro_rules! offset_of {
    ($ty:ty, $field:ident) => {
        &(*(ptr::null() as *const $ty)).$field as *const _ as usize
    };
}

/// Equivalent to PSEUDOVECSIZE in C
macro_rules! pseudovecsize {
    ($ty: ty, $field: ident) => {
        ((offset_of!($ty, $field) - *crate::vectors::HEADER_SIZE) / *crate::vectors::WORD_SIZE)
    };
}

/// Equivalent to VECSIZE in C
macro_rules! vecsize {
    ($ty: ty) => {
        ((::std::mem::size_of::<$ty>() - *crate::vectors::HEADER_SIZE + *crate::vectors::WORD_SIZE
            - 1)
            / *crate::vectors::WORD_SIZE)
    };
}

/// Equivalent to `ALLOCATE_PSEUDOVECTOR` in C
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
