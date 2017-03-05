extern crate libc;

pub mod sha1;

use std::{ptr, slice};
use libc::{c_char, c_void, size_t};
use ring::digest;

fn hash_buffer(alg: &'static digest::Algorithm,
               buffer: *const c_char,
               len: size_t,
               resblock: *mut c_void)
               -> *mut c_void {
    let buffer_slice = unsafe { slice::from_raw_parts(buffer as *const u8, len) };
    let hash = digest::digest(&alg, buffer_slice);
    let v = hash.as_ref();
    unsafe { ptr::copy(v.as_ptr(), resblock as *mut u8, v.len()) };
    resblock
}


#[no_mangle]
pub extern "C" fn sha256_buffer(buffer: *const c_char,
                                len: size_t,
                                resblock: *mut c_void)
                                -> *mut c_void {
    hash_buffer(&digest::SHA256, buffer, len, resblock)
}

#[no_mangle]
pub extern "C" fn sha1_buffer(buffer: *const c_char,
                              len: size_t,
                              resblock: *mut c_void)
                              -> *mut c_void {
    hash_buffer(&digest::SHA1, buffer, len, resblock)
}

#[no_mangle]
pub extern "C" fn sha512_buffer(buffer: *const c_char,
                                len: size_t,
                                resblock: *mut c_void)
                                -> *mut c_void {
    hash_buffer(&digest::SHA512, buffer, len, resblock)
}

#[no_mangle]
pub extern "C" fn sha384_buffer(buffer: *const c_char,
                                len: size_t,
                                resblock: *mut c_void)
                                -> *mut c_void {
    hash_buffer(&digest::SHA384, buffer, len, resblock)
}
