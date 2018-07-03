//! Lisp functions for making directory listings.

#[cfg(not(windows))]
use libc;

use remacs_macros::lisp_fn;
#[cfg(not(windows))]
use remacs_sys::build_string;
use remacs_sys::globals;

use lisp::{defsubr, LispObject};
use lists::{car, list};
use strings::string_lessp;

/// Return t if first arg file attributes list is less than second.
/// Comparison is in lexicographic order and case is significant.
#[lisp_fn]
pub fn file_attributes_lessp(f1: LispObject, f2: LispObject) -> LispObject {
    LispObject::from_bool(string_lessp(car(f1), car(f2)))
}

fn get_user_real_login_name() -> LispObject {
    unsafe { globals.Vuser_real_login_name }
}

#[cfg(windows)]
fn get_users() -> LispObject {
    let mut unames = Vec::new();
    unames.push(get_user_real_login_name());
    list(&mut unames)
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
            unames.push(unsafe { build_string((*pw).pw_name) })
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
