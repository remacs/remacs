//! Lisp functions for making directory listings.

use std::ffi::CStr;

use libc;

use remacs_macros::lisp_fn;
use remacs_sys::Fuser_real_login_name;

use lisp::{defsubr, LispObject};
use lists::list;

/// Return a list of user names currently registered in the system.
/// If we don't know how to determine that on this platform, just
/// return a list with one element, taken from `user-real-login-name'.
#[lisp_fn]
pub fn system_users() -> LispObject {
    let mut done = false;
    let mut unames = Vec::new();

    while !done {
        let pw: *mut libc::passwd = unsafe { libc::getpwent() };
        if pw.is_null() {
            done = true;
        } else {
            let pwnam_string =
                unsafe { CStr::from_ptr((*pw).pw_name).to_string_lossy().into_owned() };
            let pwnam_str = &*pwnam_string;
            unames.push(LispObject::from(pwnam_str));
        }
    }
    unsafe { libc::endpwent() };

    if unames.len() < 1 {
        unames.push(LispObject::from_raw(unsafe { Fuser_real_login_name() }));
    }

    list(&mut unames)
}

include!(concat!(env!("OUT_DIR"), "/dired_exports.rs"));
