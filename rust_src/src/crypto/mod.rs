extern crate libc;

use sha1;
use sha2::{Sha224, Digest, Sha256, Sha384, Sha512};
use std::{ptr, slice};
use libc::{c_char, c_void, size_t};

use lisp::LispObject;
use remacs_sys::{nsberror, Fcurrent_buffer, Fget_buffer, EmacsInt, make_uninit_string};
use remacs_macros::lisp_fn;

#[no_mangle]
pub unsafe extern "C" fn sha1_buffer(
    buffer: *const c_char,
    len: size_t,
    dest_buf: *mut c_void,
) -> *mut c_void {
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
where
    D: Digest,
{
    let mut hasher = hasher;
    let buffer_slice = slice::from_raw_parts(buffer as *const u8, len);
    hasher.input(buffer_slice);
    let output = hasher.result();
    ptr::copy_nonoverlapping(output.as_ptr(), dest_buf as *mut u8, output.len());
}

#[no_mangle]
pub unsafe extern "C" fn sha224_buffer(
    buffer: *const c_char,
    len: size_t,
    resblock: *mut c_void,
) -> *mut c_void {
    sha2_hash_buffer(Sha224::new(), buffer, len, resblock);
    resblock
}

#[no_mangle]
pub unsafe extern "C" fn sha256_buffer(
    buffer: *const c_char,
    len: size_t,
    resblock: *mut c_void,
) -> *mut c_void {
    sha2_hash_buffer(Sha256::new(), buffer, len, resblock);
    resblock
}


#[no_mangle]
pub unsafe extern "C" fn sha384_buffer(
    buffer: *const c_char,
    len: size_t,
    resblock: *mut c_void,
) -> *mut c_void {
    sha2_hash_buffer(Sha384::new(), buffer, len, resblock);
    resblock
}
#[no_mangle]
pub unsafe extern "C" fn sha512_buffer(
    buffer: *const c_char,
    len: size_t,
    resblock: *mut c_void,
) -> *mut c_void {
    sha2_hash_buffer(Sha512::new(), buffer, len, resblock);
    resblock
}

/// Return a hash of the contents of BUFFER-OR-NAME.
/// This hash is performed on the raw internal format of the buffer,
/// disregarding any coding systems.
/// If nil, use the current buffer.
#[lisp_fn(min = "0")]
fn buffer_hash(buffer_or_name: LispObject) -> LispObject {
    let buffer = LispObject::from_raw(if buffer_or_name.is_nil() {
        unsafe { Fcurrent_buffer() }
    } else {
        unsafe { Fget_buffer(buffer_or_name.to_raw()) }
    });
    if buffer.is_nil() {
        unsafe { nsberror(buffer_or_name.to_raw()) };
    }
    let b = buffer.as_vectorlike().unwrap().as_buffer().unwrap();
    let mut ctx = sha1::Sha1::new();

    ctx.update(unsafe {
        slice::from_raw_parts(b.beg_addr(), (b.gpt_byte() - b.beg_byte()) as usize)
    });
    if b.gpt_byte() < b.z_byte() {
        ctx.update(unsafe {
            slice::from_raw_parts(
                b.gap_end_addr(),
                (b.z_addr() as usize - b.gap_end_addr() as usize),
            )
        });
    }

    let formatted = ctx.digest().to_string();
    let digest = LispObject::from_raw(unsafe { make_uninit_string(formatted.len() as EmacsInt) });
    digest.as_string().unwrap().as_mut_slice().copy_from_slice(
        formatted
            .as_bytes(),
    );
    digest
}
