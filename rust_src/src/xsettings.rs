//! Functions for handling font and other changes dynamically.
//!
//! All of the functions defined in this file and on xsettings.c are
//! only defined when Remacs is compiled with support for the X window
//! system.

#[cfg(feature = "window-system-x11")]
use remacs_macros::lisp_fn;

#[cfg(feature = "window-system-x11")]
use crate::{
    lisp::LispObject,
    remacs_sys::build_string,
    remacs_sys::Qnil,
    remacs_sys::{current_font, current_mono_font},
};

/// Get the system default application font.
#[cfg(feature = "window-system-x11")]
#[lisp_fn]
pub fn font_get_system_normal_font() -> LispObject {
    if unsafe { current_font.is_null() } {
        Qnil
    } else {
        unsafe { build_string(current_font) }
    }
}

/// Get the system default fixed width font.
#[cfg(feature = "window-system-x11")]
#[lisp_fn]
pub fn font_get_system_font() -> LispObject {
    if unsafe { current_mono_font.is_null() } {
        Qnil
    } else {
        unsafe { build_string(current_mono_font) }
    }
}

include!(concat!(env!("OUT_DIR"), "/xsettings_exports.rs"));
