#![feature(allocator_api)]
extern crate libc;

use std::alloc::AllocErr;
use std::heap::Alloc;
use std::heap::Layout;

/// To adhere to the rule that all calls to malloc, realloc, and free
/// be redirected to their `unexec_`-prefixed variants, this crate
/// provides a custom system allocator that performs such a mapping.

extern "C" {
    fn unexec_malloc(size: libc::size_t) -> *mut libc::c_void;
    fn unexec_realloc(old_ptr: *mut libc::c_void, new_size: libc::size_t) -> *mut libc::c_void;
    fn unexec_free(ptr: *mut libc::c_void);
}

pub struct OsxUnexecAlloc;

unsafe impl<'a> Alloc for &'a OsxUnexecAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<std::ptr::NonNull<std::heap::Opaque>, AllocErr> {
        let addr = unexec_malloc(layout.size() as libc::size_t);
        if addr.is_null() {
            return Err(AllocErr);
        }

        assert_eq!(addr as usize & (layout.align() - 1), 0);
        Ok(std::ptr::NonNull::new_unchecked(addr as *mut std::heap::Opaque))
    }

    unsafe fn dealloc(&mut self, ptr:std::ptr::NonNull<std::heap::Opaque>, layout: Layout) {
        let mut_ptr = ptr.as_ptr();
        assert_eq!(mut_ptr as usize & (layout.align() - 1), 0);
        unexec_free(mut_ptr as *mut libc::c_void)
    }

    unsafe fn realloc(
        &mut self,
        ptr: std::ptr::NonNull<std::heap::Opaque>,
        _layout: Layout,
        new_size: usize,
    ) -> Result<std::ptr::NonNull<std::heap::Opaque>, AllocErr> {
        let mut_ptr = ptr.as_ptr();
        let addr = unexec_realloc(mut_ptr as *mut libc::c_void, new_size);
        if addr.is_null() {
            return Err(AllocErr);
        }

        assert_eq!(addr as usize & (_layout.align() - 1), 0);
        Ok(std::ptr::NonNull::new_unchecked(addr as *mut std::heap::Opaque))
    }
}
