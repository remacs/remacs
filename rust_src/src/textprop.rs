//! Text property support

use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    numbers::LispNumber,
    remacs_sys::Ftext_properties_at,
    remacs_sys::{get_char_property_and_overlay, textget},
};

/// Return the value of POSITION's property PROP, in OBJECT.
/// Both overlay properties and text properties are checked.
/// OBJECT is optional and defaults to the current buffer.
/// If POSITION is at the end of OBJECT, the value is nil.
/// If OBJECT is a buffer, then overlay properties are considered as well as
/// text properties.
/// If OBJECT is a window, then that window's buffer is used, but window-specific
/// overlays are considered only if they are associated with OBJECT.
#[lisp_fn(min = "2")]
pub fn get_char_property(position: LispNumber, prop: LispObject, object: LispObject) -> LispObject {
    unsafe { get_char_property_and_overlay(position.into(), prop, object, ptr::null_mut()) }
}

/// Return the value of POSITION's property PROP, in OBJECT.
/// OBJECT should be a buffer or a string; if omitted or nil, it defaults
/// to the current buffer.
/// If POSITION is at the end of OBJECT, the value is nil.
#[lisp_fn(min = "2")]
pub fn get_text_property(position: LispNumber, prop: LispObject, object: LispObject) -> LispObject {
    unsafe { textget(Ftext_properties_at(position.into(), object), prop) }
}

include!(concat!(env!("OUT_DIR"), "/textprop_exports.rs"));
