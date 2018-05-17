//! Lisp functions for making directory listings.

use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{build_string, globals};

use lisp::{defsubr, LispObject};
use lists::list;

fn get_user_real_login_name() -> LispObject {
    LispObject::from_raw(unsafe { globals.f_Vuser_real_login_name })
}

#[cfg(windows)]
fn get_users() -> LispObject {
    get_user_real_login_name()
}

#[cfg(not(windows))]
fn get_users() -> LispObject {
    let mut done = false;
    let mut unames = Vec::new();

    while !done {
        let pw: *mut libc::passwd = unsafe { libc::getpwent() };
        if pw.is_null() {
            done = true;
        } else {
            unames.push(LispObject::from_raw(unsafe { build_string((*pw).pw_name) }))
        }
    }
    unsafe { libc::endpwent() };

    if unames.is_empty() {
        unames.push(get_user_real_login_name());
    }

    list(&mut unames)
}

/// Return a list of user names currently registered in the system.
/// If we don't know how to determine that on this platform, just
/// return a list with one element, taken from `user-real-login-name'.
#[lisp_fn]
pub fn system_users() -> LispObject {
    get_users()
}

include!(concat!(env!("OUT_DIR"), "/dired_exports.rs"));
