//! Case conversion functions.
use std::ffi::CString;

use remacs_macros::lisp_fn;

use crate::{
    keymap::Ctl,
    lisp::LispObject,
    lists::put,
    lists::{LispConsCircularChecks, LispConsEndChecks},
    obarray::intern,
    remacs_sys::EmacsInt,
    remacs_sys::{case_action, casify_object, casify_region},
    remacs_sys::{control_x_map, initial_define_key, meta_map, scan_words, set_point},
    remacs_sys::{Qdisabled, Qt},
    symbols::symbol_value,
    threads::ThreadState,
};

fn casify_word(flag: case_action, words: EmacsInt) {
    let buffer_ref = ThreadState::current_buffer_unchecked();

    let far_end = match unsafe { scan_words(buffer_ref.pt, words) } {
        0 => {
            if words <= 0 {
                buffer_ref.begv
            } else {
                buffer_ref.zv
            }
        }
        n => n,
    };

    let new_pos = unsafe { casify_region(flag, buffer_ref.pt.into(), far_end.into()) };

    unsafe { set_point(new_pos) };
}

/// Convert argument to capitalized form and return that.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest to lower case.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.  If argument
/// is a character, characters which map to multiple code points when
/// cased, e.g. ﬁ, are returned unchanged.
#[lisp_fn]
pub fn capitalize(object: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_CAPITALIZE, object) }
}

/// Convert the region to capitalized form.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest to lower case.  In
/// programs, give two arguments, the starting and ending character
/// positions to operate on.
#[lisp_fn(intspec = "r")]
pub fn capitalize_region(beg: LispObject, end: LispObject) {
    unsafe { casify_region(case_action::CASE_CAPITALIZE, beg, end) };
}

/// Capitalize from point to the end of word, moving over.
/// With numerical argument ARG, capitalize the next ARG-1 words as
/// well.  This gives the word(s) a first character in upper case and
/// the rest lower case.
///
/// If point is in the middle of a word, the part of that word before
/// point is ignored when moving forward.
///
/// With negative argument, capitalize previous words but do not move.
#[lisp_fn(intspec = "p")]
pub fn capitalize_word(words: EmacsInt) {
    casify_word(case_action::CASE_CAPITALIZE, words);
}

/// Convert argument to lower case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.
#[lisp_fn]
pub fn downcase(object: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_DOWN, object) }
}

/// Convert the region to lower case.  In programs, wants two arguments.
/// These arguments specify the starting and ending character numbers
/// of the region to operate on.  When used as a command, the text
/// between point and the mark is operated on.
#[lisp_fn(
    min = "2",
    intspec = "(list (region-beginning) (region-end) (region-noncontiguous-p))"
)]
pub fn downcase_region(beg: LispObject, end: LispObject, region_noncontiguous_p: bool) {
    casefiddle_region(beg, end, region_noncontiguous_p, case_action::CASE_DOWN);
}

/// Convert to lower case from point to end of word, moving over.
///
/// If point is in the middle of a word, the part of that word before
/// point is ignored when moving forward.
///
/// With negative argument, convert previous words but do not move.
#[lisp_fn(intspec = "p")]
pub fn downcase_word(words: EmacsInt) {
    casify_word(case_action::CASE_DOWN, words);
}

/// Convert argument to upper case and return that.
/// The argument may be a character or string.  The result has the same type.
/// The argument object is not altered--the value is a copy.  If argument
/// is a character, characters which map to multiple code points when
/// cased, e.g. ﬁ, are returned unchanged.
/// See also `capitalize', `downcase' and `upcase-initials'.
#[lisp_fn]
pub fn upcase(object: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_UP, object) }
}

/* Like Fcapitalize but change only the initials.  */

/// Convert the initial of each word in the argument to upper case.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest are left unchanged.  The
/// argument may be a character or string.  The result has the same
/// type.  The argument object is not altered--the value is a copy.
/// If argument is a character, characters which map to multiple code
/// points when cased, e.g. ﬁ, are returned unchanged.
#[lisp_fn]
pub fn upcase_initials(obj: LispObject) -> LispObject {
    unsafe { casify_object(case_action::CASE_CAPITALIZE_UP, obj) }
}

// Like Fcapitalize_region but change only the initials.

/// Upcase the initial of each word in the region.
/// This means that each word's first character is converted to either
/// title case or upper case, and the rest are left unchanged.  In
/// programs, give two arguments, the starting and ending character
/// positions to operate on.
#[lisp_fn(intspec = "r")]
pub fn upcase_initials_region(beg: LispObject, end: LispObject) {
    unsafe { casify_region(case_action::CASE_CAPITALIZE_UP, beg, end) };
}

/// Convert the region to upper case.  In programs, wants two arguments.
/// These arguments specify the starting and ending character numbers
/// of the region to operate on.  When used as a command, the text
/// between point and the mark is operated on.
/// See also `capitalize-region'.
#[lisp_fn(
    min = "2",
    intspec = "(list (region-beginning) (region-end) (region-noncontiguous-p))"
)]
pub fn upcase_region(beg: LispObject, end: LispObject, region_noncontiguous_p: bool) {
    casefiddle_region(beg, end, region_noncontiguous_p, case_action::CASE_UP);
}

/// Convert to upper case from point to end of word, moving over.
///
/// If point is in the middle of a word, the part of that word before
/// point is ignored when moving forward.
///
/// With negative argument, convert previous words but do not move.
/// See also `capitalize-word'.
#[lisp_fn(intspec = "p")]
pub fn upcase_word(words: EmacsInt) {
    casify_word(case_action::CASE_UP, words);
}

// Fiddle with the case of a whole region. Used as a helper by
// downcase_region and upcase_region.
fn casefiddle_region(
    beg: LispObject,
    end: LispObject,
    region_noncontiguous_p: bool,
    action: case_action,
) {
    if !region_noncontiguous_p {
        unsafe { casify_region(action, beg, end) };
    } else {
        let bounds = call!(
            symbol_value(intern("region-extract-function")),
            intern("bounds").into()
        );

        for elt in bounds.iter_cars(LispConsEndChecks::off, LispConsCircularChecks::off) {
            let (car, cdr) = elt.into();
            unsafe { casify_region(action, car, cdr) };
        }
    }
}

#[no_mangle]
pub extern "C" fn syms_of_casefiddle() {
    def_lisp_sym!(Qidentity, "identity");
    def_lisp_sym!(Qtitlecase, "titlecase");
    def_lisp_sym!(Qspecial_uppercase, "special-uppercase");
    def_lisp_sym!(Qspecial_lowercase, "special-lowercase");
    def_lisp_sym!(Qspecial_titlecase, "special-titlecase");
}

#[no_mangle]
pub extern "C" fn keys_of_casefiddle() {
    unsafe {
        let downcase_region = CString::new("downcase-region").unwrap();
        initial_define_key(control_x_map, Ctl('L'), downcase_region.as_ptr());
        let upcase_region = CString::new("upcase-region").unwrap();
        initial_define_key(control_x_map, Ctl('U'), upcase_region.as_ptr());
        let capitalize_word = CString::new("capitalize-word").unwrap();
        initial_define_key(meta_map, 'c' as i32, capitalize_word.as_ptr());
        let downcase_word = CString::new("downcase-word").unwrap();
        initial_define_key(meta_map, 'l' as i32, downcase_word.as_ptr());
        let upcase_word = CString::new("upcase-word").unwrap();
        initial_define_key(meta_map, 'u' as i32, upcase_word.as_ptr());
    }

    put(intern("upcase-region"), Qdisabled, Qt);
    put(intern("downcase-region"), Qdisabled, Qt);
}

include!(concat!(env!("OUT_DIR"), "/casefiddle_exports.rs"));
