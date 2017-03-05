use libc::{c_void, size_t};
use std::mem;
use std::slice;
use std::ptr;

use sha1;

/// Define functions needed to compute a SHA1 from C, when you don't
/// have a contiguous buffer of bytes. This is necessary for the elisp
/// function `buffer-hash`.

// TODO: There's no reason why `buffer-hash` couldn't do the same
// thing as `secure-hash` and build a byte buffer of the entire elisp
// buffer. This would save us defining this code.

#[allow(non_camel_case_types)]
type sha1_ctx_ptr = *const ();

/// Return a new `Sha1` struct as an opaque pointer.
#[no_mangle]
pub extern "C" fn sha1_ctx_new() -> sha1_ctx_ptr {
    let ctx: Box<sha1::Sha1> = Box::new(sha1::Sha1::new());
    unsafe { mem::transmute(ctx) }
}

/// Using the given `Sha1` instance, add these bytes to the hash. May
/// be called multiple times (otherwise we'd just use `sha1_buffer`).
#[no_mangle]
pub extern "C" fn sha1_process_bytes(buffer: *const c_void, len: size_t, ctx: sha1_ctx_ptr) {
    let ctx: &mut sha1::Sha1 = unsafe { mem::transmute(ctx) };
    let buffer = buffer as *const u8;
    let bytes = unsafe { slice::from_raw_parts(buffer, len) };
    ctx.update(bytes);
}

/// Write the SHA1 digest to `dest_buf`, and drop the `Sha1` struct.
#[no_mangle]
pub extern "C" fn sha1_finish_ctx(ctx: sha1_ctx_ptr, dest_buf: *mut c_void) -> *mut c_void {
    let ctx: Box<sha1::Sha1> = unsafe { mem::transmute(ctx) };
    let output = ctx.digest().bytes();
    unsafe {
        ptr::copy_nonoverlapping(output.as_ptr(), dest_buf as *mut u8, output.len());
    }
    dest_buf
}
