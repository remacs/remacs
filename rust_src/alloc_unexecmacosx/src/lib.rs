#![feature(allocator)]
#![allocator]

#![no_std]

// // Would be nice to do this, but saying "default-features = false"
// // locally gets cancelled out by the other imports of libc globally.
// // Instead we will just cut-and-paste type definitions (boo!).
// //
// // See also https://github.com/rust-lang/rust/issues/39262
//
// extern crate libc;

mod libc {
    #![allow(non_camel_case_types)]

    pub type size_t = usize;

    // Use repr(u8) as LLVM expects `void*` to be the same as `i8*` to help enable
    // more optimization opportunities around it recognizing things like
    // malloc/free.
    #[repr(u8)]
    pub enum c_void {
        // Two dummy variants so the #[repr] attribute can be used.
        #[doc(hidden)]
        __variant1,
        #[doc(hidden)]
        __variant2,
    }
}

/// To adhere to the rule that all calls to malloc, realloc, and free
/// be redirected to their "unexec_"-prefixed variants, this crate
/// provides a custom system allocator that performs such a mapping.

extern "C" {
    fn unexec_malloc(size: libc::size_t) -> *mut libc::c_void;
    fn unexec_realloc(old_ptr: *mut libc::c_void, new_size: libc::size_t) -> *mut libc::c_void;
    fn unexec_free(ptr: *mut libc::c_void);
}

// Listed below are the five allocation functions currently required by custom
// allocators. Their signatures and symbol names are not currently typechecked
// by the compiler, but this is a future extension and are required to match
// what is found below.
//
// Note that the standard `malloc` and `realloc` functions do not provide a way
// to communicate alignment. While a more ambitious implementor might have
// attempted to support custom alignments, this simple code simply asserts that
// we never request an alignment that is not already satisfied by the underlying
// malloc call.

#[no_mangle]
pub extern "C" fn __rust_allocate(size: usize, align: usize) -> *mut u8 {
    unsafe {
        let addr = unexec_malloc(size as libc::size_t) as usize;
        assert_eq!(addr & (align - 1), 0);
        addr as *mut u8
    }
}

#[no_mangle]
pub extern "C" fn __rust_allocate_zeroed(size: usize, align: usize) -> *mut u8 {
    unsafe {
        let addr = unexec_malloc(size as libc::size_t) as usize;
        assert_eq!(addr & (align - 1), 0);
        addr as *mut u8
    }
}

#[no_mangle]
pub extern "C" fn __rust_deallocate(ptr: *mut u8, _old_size: usize, align: usize) {
    assert_eq!(ptr as usize & (align - 1), 0);
    unsafe { unexec_free(ptr as *mut libc::c_void) }
}

#[no_mangle]
pub extern "C" fn __rust_reallocate(
    ptr: *mut u8,
    _old_size: usize,
    size: usize,
    align: usize,
) -> *mut u8 {
    unsafe {
        let addr = unexec_realloc(ptr as *mut libc::c_void, size as libc::size_t) as usize;
        assert_eq!(addr & (align - 1), 0);
        addr as *mut u8
    }
}

#[no_mangle]
pub extern "C" fn __rust_reallocate_inplace(
    _ptr: *mut u8,
    old_size: usize,
    _size: usize,
    _align: usize,
) -> usize {
    old_size // this api is not supported by libc
}

#[no_mangle]
pub extern "C" fn __rust_usable_size(size: usize, _align: usize) -> usize {
    size
}
