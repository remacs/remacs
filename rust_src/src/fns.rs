//* Random utility Lisp functions.

use libc;

use remacs_macros::lisp_fn;

use crate::{
    eval::{un_autoload, unbind_to},
    lisp::defsubr,
    lisp::LispObject,
    lists::{assq, car, get, member, memq, put, LispCons},
    obarray::loadhist_attach,
    objects::equal,
    remacs_sys::Vautoload_queue,
    remacs_sys::{concat as lisp_concat, globals, record_unwind_protect},
    remacs_sys::{equal_kind, Lisp_Type},
    remacs_sys::{Fload, Fmapc},
    remacs_sys::{
        Qfuncall, Qlistp, Qnil, Qprovide, Qquote, Qrequire, Qsubfeatures, Qt,
        Qwrong_number_of_arguments,
    },
    symbols::LispSymbolRef,
    threads::c_specpdl_index,
    vectors::length,
};

/// Return t if FEATURE is present in this Emacs.
///
/// Use this to conditionalize execution of lisp code based on the
/// presence or absence of Emacs or environment extensions.
/// Use `provide' to declare that a feature is available.  This function
/// looks at the value of the variable `features'.  The optional argument
/// SUBFEATURE can be used to check a specific subfeature of FEATURE.
#[lisp_fn(min = "1")]
pub fn featurep(feature: LispSymbolRef, subfeature: LispObject) -> bool {
    let mut tem = memq(feature.as_lisp_obj(), unsafe { globals.Vfeatures });
    if tem.is_not_nil() && subfeature.is_not_nil() {
        tem = member(subfeature, get(feature, Qsubfeatures));
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
        if Vautoload_queue.is_not_nil() {
            Vautoload_queue = LispObject::cons(
                LispObject::cons(LispObject::from(0), globals.Vfeatures),
                Vautoload_queue,
            );
        }
    }
    if memq(feature.as_lisp_obj(), unsafe { globals.Vfeatures }).is_nil() {
        unsafe {
            globals.Vfeatures = LispObject::cons(feature.as_lisp_obj(), globals.Vfeatures);
        }
    }
    if subfeature.is_not_nil() {
        put(feature, Qsubfeatures, subfeature);
    }
    unsafe {
        globals.Vcurrent_load_list = LispObject::cons(
            LispObject::cons(Qprovide, feature.as_lisp_obj()),
            globals.Vcurrent_load_list,
        );
    }
    // Run any load-hooks for this file.
    unsafe {
        if let Some(c) = assq(feature.as_lisp_obj(), globals.Vafter_load_alist).as_cons() {
            Fmapc(Qfuncall, c.cdr());
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
        xsignal!(Qwrong_number_of_arguments, Qquote, length(args.as_obj()));
    }

    args.car()
}

/* List of features currently being require'd, innermost first.  */

declare_GC_protected_static!(require_nesting_list, Qnil);

unsafe extern "C" fn require_unwind(old_value: LispObject) {
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
    let feature_sym = feature.as_symbol_or_error();
    let current_load_list = unsafe { globals.Vcurrent_load_list };

    // Record the presence of `require' in this file
    // even if the feature specified is already loaded.
    // But not more than once in any file,
    // and not when we aren't loading or reading from a file.
    let from_file = unsafe { globals.load_in_progress }
        || current_load_list
            .iter_cars_safe()
            .last()
            .map_or(false, |elt| elt.is_string());

    if from_file {
        let tem = LispObject::cons(Qrequire, feature);
        if member(tem, current_load_list).is_nil() {
            loadhist_attach(tem);
        }
    }

    if memq(feature, unsafe { globals.Vfeatures }).is_not_nil() {
        return feature;
    }

    let count = c_specpdl_index();

    // This is to make sure that loadup.el gives a clear picture
    // of what files are preloaded and when.
    if unsafe { globals.Vpurify_flag != Qnil } {
        error!(
            "(require {}) while preparing to dump",
            feature_sym.symbol_name().as_string_or_error()
        );
    }

    // A certain amount of recursive `require' is legitimate,
    // but if we require the same feature recursively 3 times,
    // signal an error.
    let nesting = unsafe { require_nesting_list }
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
        record_unwind_protect(Some(require_unwind), require_nesting_list);
        require_nesting_list = LispObject::cons(feature, require_nesting_list);

        // Value saved here is to be restored into Vautoload_queue
        record_unwind_protect(Some(un_autoload), Vautoload_queue);
        Vautoload_queue = Qt;

        // Load the file.
        let tem = Fload(
            if filename.is_nil() {
                feature_sym.symbol_name()
            } else {
                filename
            },
            noerror,
            Qt,
            Qnil,
            if filename.is_nil() { Qt } else { Qnil },
        );

        // If load failed entirely, return nil.
        if tem == Qnil {
            return unbind_to(count, Qnil);
        }
    }

    let tem = memq(feature, unsafe { globals.Vfeatures });
    if tem.is_nil() {
        let tem3 = car(car(unsafe { globals.Vload_history }));

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

    unbind_to(count, feature)
}
def_lisp_sym!(Qrequire, "require");

/// Concatenate all the arguments and make the result a list.
/// The result is a list whose elements are the elements of all the arguments.
/// Each argument may be a list, vector or string.
/// The last argument is not copied, just used as the tail of the new list.
/// usage: (append &rest SEQUENCES)
#[lisp_fn]
pub fn append(args: &mut [LispObject]) -> LispObject {
    unsafe {
        lisp_concat(
            args.len() as isize,
            args.as_mut_ptr() as *mut LispObject,
            Lisp_Type::Lisp_Cons,
            true,
        )
    }
}

/// Concatenate all the arguments and make the result a string.
/// The result is a string whose elements are the elements of all the arguments.
/// Each argument may be a string or a list or vector of characters (integers).
/// usage: (concat &rest SEQUENCES)
#[lisp_fn]
pub fn concat(args: &mut [LispObject]) -> LispObject {
    unsafe {
        lisp_concat(
            args.len() as isize,
            args.as_mut_ptr() as *mut LispObject,
            Lisp_Type::Lisp_String,
            false,
        )
    }
}

#[no_mangle]
pub extern "C" fn internal_equal_cons(
    o1: LispObject,
    o2: LispObject,
    kind: equal_kind::Type,
    depth: i32,
    ht: LispObject,
) -> bool {
    match (o1.as_cons(), o2.as_cons()) {
        (Some(cons1), Some(cons2)) => cons1.equal(cons2, kind, depth, ht),
        _ => false,
    }
}

#[no_mangle]
pub extern "C" fn internal_equal_string(
    o1: LispObject,
    o2: LispObject,
    kind: equal_kind::Type,
    depth: i32,
    ht: LispObject,
) -> bool {
    let s1 = o1.as_string_or_error();
    let s2 = o2.as_string_or_error();

    s1.equal(s2, kind, depth, ht)
}

#[no_mangle]
pub extern "C" fn internal_equal_misc(
    o1: LispObject,
    o2: LispObject,
    kind: equal_kind::Type,
    depth: i32,
    ht: LispObject,
) -> bool {
    match (o1.as_misc(), o2.as_misc()) {
        (Some(m1), Some(m2)) => m1.equal(m2, kind, depth, ht),
        _ => false,
    }
}

#[cfg(windows)]
unsafe fn getloadaverage(loadavg: *mut libc::c_double, nelem: libc::c_int) -> libc::c_int {
    crate::remacs_sys::getloadavg(loadavg, nelem)
}

#[cfg(not(windows))]
unsafe fn getloadaverage(loadavg: *mut libc::c_double, nelem: libc::c_int) -> libc::c_int {
    libc::getloadavg(loadavg, nelem)
}

/// Return list of 1 minute, 5 minute and 15 minute load averages.
///
/// Each of the three load averages is multiplied by 100, then converted
/// to integer.
///
/// When USE-FLOATS is non-nil, floats will be used instead of integers.
/// These floats are not multiplied by 100.
///
/// If the 5-minute or 15-minute load averages are not available, return a
/// shortened list, containing only those averages which are available.
///
/// An error is thrown if the load average can't be obtained.  In some
/// cases making it work would require Emacs being installed setuid or
/// setgid so that it can read kernel information, and that usually isn't
/// advisable.
#[lisp_fn(min = "0")]
pub fn load_average(use_floats: bool) -> Vec<LispObject> {
    let mut load_avg: [libc::c_double; 3] = [0.0, 0.0, 0.0];
    let loads = unsafe { getloadaverage(load_avg.as_mut_ptr(), 3) };

    if loads < 0 {
        error!("load-average not implemented for this operating system");
    }

    (0..loads as usize)
        .map(|i| {
            if use_floats {
                LispObject::from(load_avg[i])
            } else {
                LispObject::from((100.0 * load_avg[i]) as i64)
            }
        })
        .collect()
}

include!(concat!(env!("OUT_DIR"), "/fns_exports.rs"));
