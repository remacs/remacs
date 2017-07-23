use libc;
use lisp::LispObject;
use remacs_sys::{pure_write_error, pure_, PURESIZE};

// checks to see if the pointer is inside the pure_ array
#[inline]
pub fn pure_p(ptr: *mut libc::c_void) -> bool {
    let ptr_addr = ptr as libc::uintptr_t;
    let pure_addr = unsafe { pure_ }.as_ptr() as libc::uintptr_t;
    if ptr_addr >= pure_addr {
        (ptr_addr - pure_addr) <= PURESIZE as usize
    } else {
        false
    }
}

// signals an error if the object is pure
#[inline]
pub fn check_impure(obj: LispObject, ptr: *mut libc::c_void) {
    if pure_p(ptr) {
        unsafe { pure_write_error(obj.to_raw()) }
    }
}
