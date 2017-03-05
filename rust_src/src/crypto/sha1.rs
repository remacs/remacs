use libc::{c_void, size_t};
use ring::digest;
use std::mem;
use std::slice;
use std::ptr;

#[allow(non_camel_case_types)]
type sha1_ctx_ptr = *const ();

#[no_mangle]
pub extern "C" fn sha1_ctx_new() -> sha1_ctx_ptr {
    let ctx: Box<digest::Context> = Box::new(digest::Context::new(&digest::SHA1));
    unsafe { mem::transmute(ctx) }
}

#[no_mangle]
pub extern "C" fn sha1_process_bytes(buffer: *const c_void, len: size_t, ctx: sha1_ctx_ptr) {
    let ctx: &mut digest::Context = unsafe { mem::transmute(ctx) };
    let buffer = buffer as *const u8;
    let bytes = unsafe { slice::from_raw_parts(buffer, len) };
    ctx.update(bytes);
}

#[no_mangle]
pub extern "C" fn sha1_finish_ctx(ctx: sha1_ctx_ptr, resbuf: *mut c_void) -> *mut c_void {
    let ctx: Box<digest::Context> = unsafe { mem::transmute(ctx) };
    let out = ctx.finish();
    let out = out.as_ref();
    unsafe {
        ptr::copy_nonoverlapping(out.as_ptr(), resbuf as *mut u8, out.len());
    }
    resbuf
}
