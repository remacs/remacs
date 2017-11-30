//* Random utility Lisp functions.

use remacs_macros::lisp_fn;
use remacs_sys::{Fcons, Fmapc, Qfuncall, Qlistp, Qprovide, Qsubfeatures, Vautoload_queue};
use remacs_sys::globals;

use lisp::LispObject;
use lisp::defsubr;
use lists::{assq, get, member, memq, put};


/// Return t if FEATURE is present in this Emacs.
///
/// Use this to conditionalize execution of lisp code based on the
/// presence or absence of Emacs or environment extensions.
/// Use `provide' to declare that a feature is available.  This function
/// looks at the value of the variable `features'.  The optional argument
/// SUBFEATURE can be used to check a specific subfeature of FEATURE.
#[lisp_fn(min = "1")]
pub fn featurep(feature: LispObject, subfeature: LispObject) -> LispObject {
    feature.as_symbol_or_error();
    let mut tem = memq(feature, LispObject::from(unsafe { globals.f_Vfeatures }));
    if tem.is_not_nil() && subfeature.is_not_nil() {
        tem = member(subfeature, get(feature, LispObject::from(Qsubfeatures)));
    }
    if tem.is_nil() {
        LispObject::constant_nil()
    } else {
        LispObject::constant_t()
    }
}

/// Announce that FEATURE is a feature of the current Emacs.
/// The optional argument SUBFEATURES should be a list of symbols listing
/// particular subfeatures supported in this version of FEATURE.
#[lisp_fn(min = "1")]
pub fn provide(feature: LispObject, subfeature: LispObject) -> LispObject {
    feature.as_symbol_or_error();
    if !subfeature.is_list() {
        wrong_type!(Qlistp, subfeature)
    }
    unsafe {
        if LispObject::from(Vautoload_queue).is_not_nil() {
            Vautoload_queue = Fcons(
                Fcons(LispObject::from_fixnum(0).to_raw(), globals.f_Vfeatures),
                Vautoload_queue,
            );
        }
    }
    if memq(feature, LispObject::from(unsafe { globals.f_Vfeatures })).is_nil() {
        unsafe {
            globals.f_Vfeatures = Fcons(feature.to_raw(), globals.f_Vfeatures);
        }
    }
    if subfeature.is_not_nil() {
        put(feature, LispObject::from(Qsubfeatures), subfeature);
    }
    unsafe {
        globals.f_Vcurrent_load_list = Fcons(
            Fcons(Qprovide, feature.to_raw()),
            globals.f_Vcurrent_load_list,
        );
    }
    // Run any load-hooks for this file.
    unsafe {
        if let Some(c) = assq(feature, LispObject::from(globals.f_Vafter_load_alist)).as_cons() {
            Fmapc(Qfuncall, c.cdr().to_raw());
        }
    }
    feature
}

include!(concat!(env!("OUT_DIR"), "/fns_exports.rs"));
