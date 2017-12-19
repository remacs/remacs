//* Random utility Lisp functions.

use remacs_macros::lisp_fn;
use remacs_sys::{Fcons, Fmapc, Vautoload_queue};
use remacs_sys::{Qfuncall, Qlistp, Qprovide, Qquote, Qsubfeatures, Qwrong_number_of_arguments};
use remacs_sys::globals;

use lisp::{LispCons, LispObject};
use lisp::defsubr;
use lists::{assq, get, member, memq, put};
use symbols::LispSymbolRef;
use vectors::length;

/// Return t if FEATURE is present in this Emacs.
///
/// Use this to conditionalize execution of lisp code based on the
/// presence or absence of Emacs or environment extensions.
/// Use `provide' to declare that a feature is available.  This function
/// looks at the value of the variable `features'.  The optional argument
/// SUBFEATURE can be used to check a specific subfeature of FEATURE.
#[lisp_fn(min = "1")]
pub fn featurep(feature: LispSymbolRef, subfeature: LispObject) -> LispObject {
    let mut tem = memq(
        feature.as_lisp_obj(),
        LispObject::from(unsafe { globals.f_Vfeatures }),
    );
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
pub fn provide(feature: LispSymbolRef, subfeature: LispObject) -> LispObject {
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
    if memq(
        feature.as_lisp_obj(),
        LispObject::from(unsafe { globals.f_Vfeatures }),
    ).is_nil()
    {
        unsafe {
            globals.f_Vfeatures = Fcons(feature.as_lisp_obj().to_raw(), globals.f_Vfeatures);
        }
    }
    if subfeature.is_not_nil() {
        put(
            feature.as_lisp_obj(),
            LispObject::from(Qsubfeatures),
            subfeature,
        );
    }
    unsafe {
        globals.f_Vcurrent_load_list = Fcons(
            Fcons(Qprovide, feature.as_lisp_obj().to_raw()),
            globals.f_Vcurrent_load_list,
        );
    }
    // Run any load-hooks for this file.
    unsafe {
        if let Some(c) = assq(
            feature.as_lisp_obj(),
            LispObject::from(globals.f_Vafter_load_alist),
        ).as_cons()
        {
            Fmapc(Qfuncall, c.cdr().to_raw());
        }
    }
    feature.as_lisp_obj()
}

/// Return the argument, without evaluating it.  `(quote x)' yields `x'.
/// Warning: `quote' does not construct its return value, but just returns
/// the value that was pre-constructed by the Lisp reader (see info node
/// `(elisp)Printed Representation').
/// This means that \\='(a . b) is not identical to (cons \\='a \\='b): the former
/// does not cons.  Quoting should be reserved for constants that will
/// never be modified by side-effects, unless you like self-modifying code.
/// See the common pitfall in info node `(elisp)Rearrangement' for an example
/// of unexpected results when a quoted object is modified.
/// usage: (quote ARG)
#[lisp_fn(unevalled = "true")]
pub fn quote(args: LispCons) -> LispObject {
    if args.cdr().is_not_nil() {
        xsignal!(
            Qwrong_number_of_arguments,
            LispObject::from(Qquote),
            length(args.as_obj())
        );
    }

    args.car()
}

include!(concat!(env!("OUT_DIR"), "/fns_exports.rs"));
