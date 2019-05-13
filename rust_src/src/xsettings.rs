//! Functions for handling font and other changes dynamically.
//!
//! All of the functions defined in this file and on xsettings.c are
//! only defined when Remacs is compiled with support for the X window
//! system.
use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::build_string,
    remacs_sys::globals,
    remacs_sys::{current_font, current_mono_font, current_tool_bar_style},
    remacs_sys::{Qboth, Qboth_horiz, Qimage, Qnil, Qtext, Qtext_image_horiz},
};

/// Get the system default application font.
#[lisp_fn]
pub fn font_get_system_normal_font() -> LispObject {
    if unsafe { current_font.is_null() } {
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

/// Get the system tool bar style.
/// If no system tool bar style is known, return `tool-bar-style' if set to a
/// known style.  Otherwise return image.
#[lisp_fn]
pub fn tool_bar_get_system_style() -> LispObject {
    let style = unsafe { globals.Vtool_bar_style };
    let current_style = unsafe { current_tool_bar_style };
    match style {
        Qimage | Qtext | Qboth | Qboth_horiz | Qtext_image_horiz => style,
        _ if current_style.is_nil() => Qimage,
        _ => current_style,
    }
}

include!(concat!(env!("OUT_DIR"), "/xsettings_exports.rs"));
