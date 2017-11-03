macro_rules! offset_of {
    ($ty:ty, $field:ident) => {
        &(*(0 as *const $ty)).$field as *const _ as usize
    }
}

/// Equivalent to PSEUDOVECSIZE in C
macro_rules! pseudovecsize {
    ($ty: ty, $field: ident) => {
        ((offset_of!($ty, $field) - *::vectors::HEADER_SIZE) / *::vectors::WORD_SIZE)
    }
}

/// Equivalent to VECSIZE in C
macro_rules! vecsize {
    ($ty: ty) => {
        ((::std::mem::size_of::<$ty>()
          - *::vectors::HEADER_SIZE + *::vectors::WORD_SIZE - 1) / *::vectors::WORD_SIZE)
    }
}

/// Equivalent to `ALLOCATE_PSEUDOVECTOR` in C
macro_rules! allocate_pseudovector {
    ($ty: ty, $field: ident, $vectype: expr) => {
        unsafe { ::remacs_sys::allocate_pseudovector(vecsize!($ty) as ::libc::c_int,
                                       pseudovecsize!($ty, $field) as ::libc::c_int,
                                       pseudovecsize!($ty, $field) as ::libc::c_int,
                                       $vectype) as *mut $ty}
    }
}
