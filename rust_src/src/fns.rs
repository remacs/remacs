//* Random utility Lisp functions.

use remacs_macros::lisp_fn;
use remacs_sys::{Fcons, Fload, Fmapc};
use remacs_sys::{Qfuncall, Qlistp, Qnil, Qprovide, Qquote, Qrequire, Qsubfeatures, Qt,
                 Qwrong_number_of_arguments};
use remacs_sys::{globals, record_unwind_protect, unbind_to};
use remacs_sys::Lisp_Object;
use remacs_sys::Vautoload_queue;

use eval::un_autoload;
use lisp::{LispCons, LispObject};
use lisp::defsubr;
use lists::{assq, car, get, member, memq, put};
use obarray::loadhist_attach;
use objects::equal;
use symbols::LispSymbolRef;
use threads::c_specpdl_index;
use vectors::length;

/// Return t if FEATURE is present in this Emacs.
///
/// Use this to conditionalize execution of lisp code based on the
/// presence or absence of Emacs or environment extensions.
/// Use `provide' to declare that a feature is available.  This function
/// looks at the value of the variable `features'.  The optional argument
/// SUBFEATURE can be used to check a specific subfeature of FEATURE.
#[lisp_fn(min = "1")]
pub fn featurep(feature: LispSymbolRef, subfeature: LispObject) -> bool {
    let mut tem = memq(
        feature.as_lisp_obj(),
        LispObject::from_raw(unsafe { globals.f_Vfeatures }),
    );
    if tem.is_not_nil() && subfeature.is_not_nil() {
        tem = member(subfeature, get(feature, LispObject::from_raw(Qsubfeatures)));
    }
    tem.is_not_nil()
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
        if LispObject::from_raw(Vautoload_queue).is_not_nil() {
            Vautoload_queue = Fcons(
                Fcons(LispObject::from_fixnum(0).to_raw(), globals.f_Vfeatures),
                Vautoload_queue,
            );
        }
    }
    if memq(
        feature.as_lisp_obj(),
        LispObject::from_raw(unsafe { globals.f_Vfeatures }),
    ).is_nil()
    {
        unsafe {
            globals.f_Vfeatures = Fcons(feature.as_lisp_obj().to_raw(), globals.f_Vfeatures);
        }
    }
    if subfeature.is_not_nil() {
        put(
            feature.as_lisp_obj(),
            LispObject::from_raw(Qsubfeatures),
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
            LispObject::from_raw(globals.f_Vafter_load_alist),
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
            LispObject::from_raw(Qquote),
            length(args.as_obj())
        );
    }

    args.car()
}

/* List of features currently being require'd, innermost first.  */

declare_GC_protected_static!(require_nesting_list, Qnil);

unsafe extern "C" fn require_unwind(old_value: Lisp_Object) {
    require_nesting_list = old_value;
}

/// If feature FEATURE is not loaded, load it from FILENAME.
/// If FEATURE is not a member of the list `features', then the feature is
/// not loaded; so load the file FILENAME.
///
/// If FILENAME is omitted, the printname of FEATURE is used as the file
/// name, and `load' will try to load this name appended with the suffix
/// `.elc', `.el', or the system-dependent suffix for dynamic module
/// files, in that order.  The name without appended suffix will not be
/// used.  See `get-load-suffixes' for the complete list of suffixes.
///
/// The directories in `load-path' are searched when trying to find the
/// file name.
///
/// If the optional third argument NOERROR is non-nil, then return nil if
/// the file is not found instead of signaling an error.  Normally the
/// return value is FEATURE.
///
/// The normal messages at start and end of loading FILENAME are
/// suppressed.
#[lisp_fn(min = "1")]
pub fn require(feature: LispObject, filename: LispObject, noerror: LispObject) -> LispObject {
    let mut from_file = unsafe { globals.f_load_in_progress };

    let feature_sym = feature.as_symbol_or_error();

    // Record the presence of `require' in this file
    // even if the feature specified is already loaded.
    // But not more than once in any file,
    // and not when we aren't loading or reading from a file.
    if !from_file {
        if LispObject::from_raw(unsafe { globals.f_Vcurrent_load_list })
            .iter_tails_safe()
            .any(|tail| {
                let (first, rest) = tail.as_tuple();
                rest.is_nil() && first.is_string()
            }) {
            from_file = true;
        }
    }

    if from_file {
        let tem = LispObject::cons(LispObject::from_raw(Qrequire), feature);
        if member(
            tem,
            LispObject::from_raw(unsafe { globals.f_Vcurrent_load_list }),
        ).is_nil()
        {
            loadhist_attach(tem.to_raw());
        }
    }

    if memq(
        feature,
        LispObject::from_raw(unsafe { globals.f_Vfeatures }),
    ).is_not_nil()
    {
        return feature;
    }

    let count = c_specpdl_index();

    // This is to make sure that loadup.el gives a clear picture
    // of what files are preloaded and when.
    if unsafe { globals.f_Vpurify_flag != Qnil } {
        error!(
            "(require {}) while preparing to dump",
            feature_sym.symbol_name().as_string_or_error()
        );
    }

    // A certain amount of recursive `require' is legitimate,
    // but if we require the same feature recursively 3 times,
    // signal an error.
    let nesting = LispObject::from_raw(unsafe { require_nesting_list })
        .iter_cars()
        .filter(|elt| equal(feature, *elt))
        .count();

    if nesting > 3 {
        error!(
            "Recursive `require' for feature `{}'",
            feature_sym.symbol_name().as_string_or_error()
        );
    }

    unsafe {
        // Update the list for any nested `require's that occur.
        record_unwind_protect(require_unwind, require_nesting_list);
        require_nesting_list = Fcons(feature.to_raw(), require_nesting_list);

        // Value saved here is to be restored into Vautoload_queue
        record_unwind_protect(un_autoload, Vautoload_queue);
        Vautoload_queue = Qt;

        // Load the file.
        let tem = Fload(
            if filename.is_nil() {
                feature_sym.symbol_name().to_raw()
            } else {
                filename.to_raw()
            },
            noerror.to_raw(),
            Qt,
            Qnil,
            if filename.is_nil() { Qt } else { Qnil },
        );

        // If load failed entirely, return nil.
        if tem == Qnil {
            return LispObject::from_raw(unbind_to(count, Qnil));
        }
    }

    let tem = memq(
        feature,
        LispObject::from_raw(unsafe { globals.f_Vfeatures }),
    );
    if tem.is_nil() {
        let tem3 = car(car(LispObject::from_raw(unsafe {
            globals.f_Vload_history
        })));

        if tem3.is_nil() {
            error!(
                "Required feature `{}' was not provided",
                feature.as_string_or_error()
            );
        } else {
            // Cf autoload-do-load.
            error!(
                "Loading file {} failed to provide feature `{}'",
                tem3.as_string_or_error(),
                feature.as_string_or_error()
            );
        }
    }

    // Once loading finishes, don't undo it.
    unsafe {
        Vautoload_queue = Qt;
    }

    LispObject::from_raw(unsafe { unbind_to(count, feature.to_raw()) })
}
def_lisp_sym!(Qrequire, "require");

include!(concat!(env!("OUT_DIR"), "/fns_exports.rs"));
