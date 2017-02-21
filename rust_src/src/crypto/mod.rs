extern crate libc;

pub mod sha1;

use std::{ptr, slice};
use libc::{c_char, c_void, size_t};
use ring::digest;

#[no_mangle]
pub extern "C" fn sha256_buffer(buffer: *const c_char,
                                len: size_t,
                                resblock: *mut c_void)
                                -> *mut c_void {
    let buffer_slice = unsafe { slice::from_raw_parts(buffer as *const u8, len) };
    let hash = digest::digest(&digest::SHA256, buffer_slice);
    let v = hash.as_ref();
    unsafe { ptr::copy(v.as_ptr(), resblock as *mut u8, v.len()) };
    resblock
}
