//* Random utility Lisp functions.

use remacs_macros::lisp_fn;
use remacs_sys::{globals, Qsubfeatures};
use lisp::LispObject;
use lists::{memq, get, member};


/// Return t if FEATURE is present in this Emacs.
///
/// Use this to conditionalize execution of lisp code based on the
/// presence or absence of Emacs or environment extensions.
/// Use `provide' to declare that a feature is available.  This function
/// looks at the value of the variable `features'.  The optional argument
/// SUBFEATURE can be used to check a specific subfeature of FEATURE.
#[lisp_fn(min = "1")]
fn featurep(feature: LispObject, subfeature: LispObject) -> LispObject {
    feature.as_symbol_or_error();
    let mut tem = memq(
        feature,
        LispObject::from(unsafe { globals.f_Vfeatures }),
    );
    if tem.is_not_nil() && subfeature.is_not_nil() {
        tem = member(
            subfeature,
            get(feature, LispObject::from(unsafe { Qsubfeatures })),
        );
    }
    if tem.is_nil() {
        LispObject::constant_nil()
    } else {
        LispObject::constant_t()
    }
}
