#![feature(allocator_api)]
extern crate libc;

use std::alloc::GlobalAlloc;
use std::alloc::Layout;

/// To adhere to the rule that all calls to malloc, realloc, and free
/// be redirected to their `unexec_`-prefixed variants, this crate
/// provides a custom system allocator that performs such a mapping.

extern "C" {
    fn unexec_malloc(size: libc::size_t) -> *mut libc::c_void;
    fn unexec_realloc(old_ptr: *mut libc::c_void, new_size: libc::size_t) -> *mut libc::c_void;
    fn unexec_free(ptr: *mut libc::c_void);
}

pub struct OsxUnexecAlloc;

unsafe impl GlobalAlloc for OsxUnexecAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let addr = unexec_malloc(layout.size() as libc::size_t);
        if addr.is_null() {
            return addr as *mut u8;
        }

        assert_eq!(addr as usize & (layout.align() - 1), 0);
        addr as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        assert_eq!(ptr as usize & (layout.align() - 1), 0);
        unexec_free(ptr as *mut libc::c_void)
    }

    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        let addr = unexec_realloc(ptr as *mut libc::c_void, new_size);
        if addr.is_null() {
            return addr as *mut u8;
        }

        assert_eq!(addr as usize & (_layout.align() - 1), 0);
        addr as *mut u8
    }
}
