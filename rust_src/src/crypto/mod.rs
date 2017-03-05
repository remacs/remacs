extern crate libc;

pub mod sha1_ctx;

use sha1;
use sha2::{Sha224, Digest, Sha256, Sha384, Sha512};
use std::{ptr, slice};
use libc::{c_char, c_void, size_t};

#[no_mangle]
pub unsafe extern "C" fn sha1_buffer(buffer: *const c_char,
                                     len: size_t,
                                     dest_buf: *mut c_void)
                                     -> *mut c_void {
    let mut hasher = sha1::Sha1::new();
    let buffer_slice = slice::from_raw_parts(buffer as *const u8, len);
    hasher.update(buffer_slice);
    let output = hasher.digest().bytes();
    ptr::copy_nonoverlapping(output.as_ptr(), dest_buf as *mut u8, output.len());
    dest_buf
}

/// Given an instance of `Digest`, and `buffer` with length `len`,
/// write its hash to `dest_buf`.
unsafe fn sha2_hash_buffer<D>(hasher: D, buffer: *const c_char, len: size_t, dest_buf: *mut c_void)
    where D: Digest
{
    let mut hasher = hasher;
    let buffer_slice = slice::from_raw_parts(buffer as *const u8, len);
    hasher.input(buffer_slice);
    let output = hasher.result();
    ptr::copy_nonoverlapping(output.as_ptr(), dest_buf as *mut u8, output.len());
}

#[no_mangle]
pub unsafe extern "C" fn sha224_buffer(buffer: *const c_char,
                                       len: size_t,
                                       resblock: *mut c_void)
                                       -> *mut c_void {
    sha2_hash_buffer(Sha224::new(), buffer, len, resblock);
    resblock
}

#[no_mangle]
pub unsafe extern "C" fn sha256_buffer(buffer: *const c_char,
                                       len: size_t,
                                       resblock: *mut c_void)
                                       -> *mut c_void {
    sha2_hash_buffer(Sha256::new(), buffer, len, resblock);
    resblock
}


#[no_mangle]
pub unsafe extern "C" fn sha384_buffer(buffer: *const c_char,
                                       len: size_t,
                                       resblock: *mut c_void)
                                       -> *mut c_void {
    sha2_hash_buffer(Sha384::new(), buffer, len, resblock);
    resblock
}
#[no_mangle]
pub unsafe extern "C" fn sha512_buffer(buffer: *const c_char,
                                       len: size_t,
                                       resblock: *mut c_void)
                                       -> *mut c_void {
    sha2_hash_buffer(Sha512::new(), buffer, len, resblock);
    resblock
}
