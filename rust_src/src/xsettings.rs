//! Functions for handling font and other changes dynamically.

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject, remacs_sys::build_string, remacs_sys::current_mono_font, remacs_sys::Qnil,
};

/// Get the system default application font.
#[lisp_fn]
pub fn font_get_system_normal_font() -> LispObject {
    if unsafe { current_font.is_nul() } {
        Qnil
    } else {
        unsafe { build_string(current_font) }
    }
}

/// Get the system default fixed width font.
#[lisp_fn]
pub fn font_get_system_font() -> LispObject {
    if unsafe { current_mono_font.is_null() } {
        Qnil
    } else {
        unsafe { build_string(current_mono_font) }
    }
}

include!(concat!(env!("OUT_DIR"), "/xsettings_exports.rs"));
