//! Coding system handler.

use remacs_macros::lisp_fn;

use crate::{
    data::aref,
    eval::unbind_to,
    hashtable::{
        gethash,
        HashLookupResult::{Found, Missing},
        LispHashTableRef,
    },
    lisp::LispObject,
    lists::{get, put},
    minibuf::completing_read,
    multibyte::LispStringRef,
    obarray::intern_lisp,
    remacs_sys::{
        code_convert_string as c_code_convert_string, code_convert_string_norecord,
        encode_file_name as c_encode_file_name, globals, specbind,
    },
    remacs_sys::{
        safe_eval, Qcoding_system_define_form, Qcoding_system_error, Qcoding_system_history,
        Qcoding_system_p, Qcompletion_ignore_case, Qnil, Qno_conversion, Qt, Qutf_8,
        Vcoding_system_hash_table,
    },
    threads::c_specpdl_index,
};

/// Return the spec vector of CODING_SYSTEM_SYMBOL.
/// Same as the CODING_SYSTEM_SPEC C macro.
fn coding_system_spec(coding_system: LispObject) -> LispObject {
    gethash(
        coding_system,
        unsafe { Vcoding_system_hash_table }.into(),
        Qnil,
    )
}

/// Return the ID of OBJECT.
/// Same as the CODING_SYSTEM_ID C macro.
pub fn coding_system_id(object: LispObject) -> isize {
    let h_ref: LispHashTableRef = unsafe { Vcoding_system_hash_table }.into();
    match h_ref.lookup(object) {
        Found(idx) => idx as isize,
        Missing(_) => -1,
    }
}

/// Check if X is a coding system or not.  If it is, return the spec vector of
/// the coding system.
/// Alternative to the CHECK_CODING_SYSTEM_GET_SPEC C macro.
fn check_coding_system_get_spec(x: LispObject) -> LispObject {
    match coding_system_spec(x) {
        Qnil => {
            check_coding_system_lisp(x);
            match coding_system_spec(x) {
                Qnil => wrong_type!(Qcoding_system_p, x),
                spec => spec,
            }
        }
        spec => spec,
    }
}

/// Return t if OBJECT is nil or a coding-system.
/// See the documentation of `define-coding-system' for information
/// about coding-system objects.
#[lisp_fn]
pub fn coding_system_p(object: LispObject) -> bool {
    object.is_nil()
        || coding_system_id(object) >= 0
        || object.is_symbol() && get(object.into(), Qcoding_system_define_form).into()
}

/// Check validity of CODING-SYSTEM.
/// If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
/// It is valid if it is nil or a symbol defined as a coding system by the
/// function `define-coding-system'.
#[lisp_fn(name = "check-coding-system", c_name = "check_coding_system")]
pub fn check_coding_system_lisp(coding_system: LispObject) -> LispObject {
    let define_form = get(coding_system.into(), Qcoding_system_define_form);
    if define_form.is_not_nil() {
        put(coding_system.into(), Qcoding_system_define_form, Qnil);
        unsafe { safe_eval(define_form) };
    }
    if !coding_system_p(coding_system) {
        xsignal!(Qcoding_system_error, coding_system);
    }
    coding_system
}

/// Return the list of aliases of CODING-SYSTEM.
#[lisp_fn]
pub fn coding_system_aliases(coding_system: LispObject) -> LispObject {
    let coding_system = match coding_system {
        Qnil => Qno_conversion,
        coding_system => coding_system,
    };
    let spec = check_coding_system_get_spec(coding_system);
    aref(spec, 1)
}

/// Wrapper for encode_file_name (NOT PORTED)
pub fn encode_file_name(fname: LispStringRef) -> LispStringRef {
    unsafe { c_encode_file_name(fname.into()) }.into()
}

/// Implements DECODE_SYSTEM macro
/// Decode the string `input_string` using the specified coding system
/// for system functions, if any.
pub fn decode_system(input_string: LispStringRef) -> LispStringRef {
    let local_coding_system: LispObject = unsafe { globals.Vlocale_coding_system };
    if local_coding_system.is_nil() {
        input_string
    } else {
        unsafe { code_convert_string_norecord(input_string.into(), Qutf_8, true).into() }
    }
}

/// Decode STRING which is encoded in CODING-SYSTEM, and return the result.
/// Optional third arg NOCOPY non-nil means it is OK to return STRING
/// itself if the decoding operation is trivial.
/// Optional fourth arg BUFFER non-nil means that the decoded text is inserted in that buffer after point (point does not move). In this case, the return value is the length of the decoded text.
/// This function sets `last-coding-system-used` to the precise coding system
/// used (which may be different from CODING-SYSTEM if CODING-SYSTEM is not fully specified.)
#[lisp_fn(min = "1")]
pub fn decode_coding_string(
    string: LispObject,
    coding_system: LispObject,
    nocopy: LispObject,
    buffer: LispObject,
) -> LispObject {
    code_convert_string(
        string,
        coding_system,
        buffer,
        false,
        nocopy.is_not_nil(),
        false,
    )
}

/// Encode STRING to CODING-SYSTEM, and return the result.
/// Optional third arg NOCOPY non-nil means it is OK to return STRING
/// itself if the encoding operation is trivial.
/// Optional fourth arg BUFFER non-nil means that the encoded text is inserted in that buffer after point (point does not move). In this case, the return value is the length of the encoded text.
/// This function sets `last-coding-system-used` to the precise coding system
/// used (which may be different from CODING-SYSTEM if CODING-SYSTEM is not fully specified.)
#[lisp_fn(min = "1")]
pub fn encode_coding_string(
    string: LispObject,
    coding_system: LispObject,
    nocopy: LispObject,
    buffer: LispObject,
) -> LispObject {
    code_convert_string(
        string,
        coding_system,
        buffer,
        true,
        nocopy.is_not_nil(),
        false,
    )
}

// Wrapper for code_convert_string (NOT PORTED)
pub fn code_convert_string(
    string: LispObject,
    coding_system: LispObject,
    dst_object: LispObject,
    encodep: bool,
    nocopy: bool,
    norecord: bool,
) -> LispObject {
    unsafe { c_code_convert_string(string, coding_system, dst_object, encodep, nocopy, norecord) }
}

/// Read a coding system from the minibuffer, prompting with string PROMPT.
/// If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
/// Ignores case when completing coding systems (all Emacs coding systems
/// are lower-case).
#[lisp_fn(min = "1")]
pub fn read_coding_system(prompt: LispObject, mut default_coding_system: LispObject) -> LispObject {
    let count = c_specpdl_index();

    if let Some(s) = default_coding_system.as_symbol() {
        default_coding_system = s.symbol_name();
    }

    unsafe {
        specbind(Qcompletion_ignore_case, Qt);
    }
    let val = completing_read(
        prompt,
        unsafe { globals.Vcoding_system_alist },
        Qnil,
        Qt,
        Qnil,
        Qcoding_system_history,
        default_coding_system,
        Qnil,
    );
    unbind_to(count, Qnil);

    let tem: LispStringRef = val.into();
    if tem.is_empty() {
        Qnil
    } else {
        intern_lisp(tem, None)
    }
}

include!(concat!(env!("OUT_DIR"), "/coding_exports.rs"));
