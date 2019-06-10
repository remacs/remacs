//! Text property support

use std::ptr;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    numbers::LispNumber,
    remacs_sys::Ftext_properties_at,
    remacs_sys::Qt,
    remacs_sys::{get_char_property_and_overlay, set_text_properties, textget},
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

/// Completely replace properties of the text from START to END
/// The third argument PROPERTIES is the new property list.
/// If the optional fourth argument OBJECT is a buffer (or nil, which
/// means the current buffer), START and END are buffer positions (integers
/// or markers). If OBJECT is a string, START and END are 0-based indices into
/// it. If PROPERTIES is nil, the effect is to remove all properties from the
/// designated part of OBJECT
#[lisp_fn(
    c_name = "set_text_properties",
    name = "set-text-properties",
    min = "3"
)]
pub fn set_text_properties_lisp(
    start: LispObject,
    end: LispObject,
    properties: LispObject,
    object: LispObject,
) -> LispObject {
    unsafe { set_text_properties(start, end, properties, object, Qt) }
}

include!(concat!(env!("OUT_DIR"), "/textprop_exports.rs"));
