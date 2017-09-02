#![feature(allocator_api)]

extern crate libc;

use std::heap::Alloc;
use std::heap::Layout;
use std::heap::AllocErr;

/// To adhere to the rule that all calls to malloc, realloc, and free
/// be redirected to their "unexec_"-prefixed variants, this crate
/// provides a custom system allocator that performs such a mapping.

macro_rules! alloc_functions {
    ($alloc: ident, $free: ident, $realloc: ident) => (
        #[inline]
        unsafe fn remacs_alloc(size: libc::size_t) -> *mut libc::c_void {
            $alloc(size)
        }

        #[inline]
        unsafe fn remacs_free(ptr: *mut libc::c_void) {
            $free(ptr)
        }

        #[inline]
        unsafe fn remacs_realloc(old_ptr: *mut libc::c_void, new_size: libc::size_t) -> *mut libc::c_void {
            $realloc(old_ptr, new_size)
        }
    )

}

#[cfg(not(target_os = "macos"))]
extern "C" {
    fn malloc(size: libc::size_t) -> *mut libc::c_void;
    fn realloc(old_ptr: *mut libc::c_void, new_size: libc::size_t) -> *mut libc::c_void;
    fn free(ptr: *mut libc::c_void);
}

#[cfg(not(target_os = "macos"))]
alloc_functions!(malloc, free, realloc);

#[cfg(target_os = "macos")]
extern "C" {
    fn unexec_malloc(size: libc::size_t) -> *mut libc::c_void;
    fn unexec_realloc(old_ptr: *mut libc::c_void, new_size: libc::size_t) -> *mut libc::c_void;
    fn unexec_free(ptr: *mut libc::c_void);
}

#[cfg(target_os = "macos")]
alloc_functions!(unexec_malloc, unexec_free, unexec_realloc);

pub struct UnexecAlloc;

unsafe impl<'a> Alloc for &'a UnexecAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        let addr = remacs_alloc(layout.size() as libc::size_t);
        if addr.is_null() {
            return Err(AllocErr::Exhausted { request: layout });
        }

        debug_assert!(addr as usize & (layout.align() - 1) == 0);
        Ok(addr as *mut u8)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        debug_assert!(ptr as usize & (layout.align() - 1) == 0);
        remacs_free(ptr as *mut libc::c_void)
    }

    unsafe fn realloc(
        &mut self,
        ptr: *mut u8,
        _layout: Layout,
        new_layout: Layout,
    ) -> Result<*mut u8, AllocErr> {
        let addr = remacs_realloc(ptr as *mut libc::c_void, new_layout.size() as libc::size_t);
        if addr.is_null() {
            return Err(AllocErr::Exhausted { request: new_layout });
        }

        debug_assert!(addr as usize & (new_layout.align() - 1) == 0);
        Ok(addr as *mut u8)
    }
}
