//! Generic Lisp eval macros.

/*
 * N.B. Wherever unsafe occurs in this file the line should be preceded
 * by `#[allow(unused_unsafe)]`. This allows the macro to be called
 * from within an `unsafe` block without the compiler complaining that
 * the unsafe is not used.
 */

/// Macro to generate an error with a list from any number of arguments.
/// Replaces xsignal0, etc. in the C layer.
///
/// Like `Fsignal`, but never returns. Can be used for any error
/// except `Qquit`, which can return from `Fsignal`. See the elisp docstring
/// for `signal` for an explanation of the arguments.
macro_rules! xsignal {
    ($symbol:expr) => {
        #[allow(unused_unsafe)]
        unsafe {
            crate::eval::signal($symbol, crate::remacs_sys::Qnil);
        }
    };
    ($symbol:expr, $($tt:tt)+) => {
        #[allow(unused_unsafe)]
        unsafe {
            crate::eval::signal($symbol, list!($($tt)+));
        }
    };
}

/// Macro to call Lisp functions with any number of arguments.
/// Replaces call0, call1, etc. in the C layer.
macro_rules! call {
    ($func:expr, $($arg:expr),*) => {
        crate::eval::funcall(&mut [$func, $($arg),*])
    };
    ($func:expr) => {
        crate::eval::funcall(&mut [$func])
    }
}

macro_rules! callN_raw {
    ($func:expr, $($arg:expr),*) => {{
        let mut argsarray = [$($arg),*];
        #[allow(unused_unsafe)]
        unsafe {
            $func(argsarray.len() as ::libc::ptrdiff_t, argsarray.as_mut_ptr())
        }
    }}
}

macro_rules! message_with_string {
    ($str:expr, $obj:expr, $should_log:expr) => {
        #[allow(unused_unsafe)]
        unsafe {
            crate::remacs_sys::message_with_string(
                $str.as_ptr() as *const ::libc::c_char,
                $obj,
                $should_log,
            );
        }
    };
}

/// Macro to format an error message.
/// Replaces error() in the C layer.
macro_rules! error {
    ($str:expr) => {{
        #[allow(unused_unsafe)]
        let strobj = unsafe {
            crate::remacs_sys::make_string($str.as_ptr() as *const ::libc::c_char,
                                      $str.len() as ::libc::ptrdiff_t)
        };
        xsignal!(crate::remacs_sys::Qerror, strobj);
    }};
    ($fmtstr:expr, $($arg:expr),*) => {{
        let formatted = format!($fmtstr, $($arg),*);
        #[allow(unused_unsafe)]
        let strobj = unsafe {
            crate::remacs_sys::make_string(formatted.as_ptr() as *const ::libc::c_char,
                                      formatted.len() as ::libc::ptrdiff_t)
        };
        xsignal!(crate::remacs_sys::Qerror, strobj);
    }};
}

/// Macro to format a "wrong argument type" error message.
macro_rules! wrong_type {
    ($pred:expr, $arg:expr) => {
        xsignal!(crate::remacs_sys::Qwrong_type_argument, $pred, $arg);
    };
}

macro_rules! args_out_of_range {
    ($($tt:tt)+) => { xsignal!(crate::remacs_sys::Qargs_out_of_range, $($tt)+); };
}

macro_rules! arith_error {
    () => {
        xsignal!(crate::remacs_sys::Qarith_error);
    };
}

macro_rules! void_variable {
    ($var:expr) => {
        xsignal!(crate::remacs_sys::Qvoid_variable, $var);
    };
}

macro_rules! wrong_number_of_arguments {
    ($sym:expr, $num:expr) => {
        xsignal!(crate::remacs_sys::Qwrong_number_of_arguments, $sym, $num);
    };
}

macro_rules! setting_constant {
    ($sym:expr) => {
        xsignal!(crate::remacs_sys::Qsetting_constant, $sym);
    };
}

macro_rules! user_error {
    ($msg:expr) => {
        xsignal!(crate::remacs_sys::Quser_error, $msg);
    };
}

macro_rules! list {
    ($arg:expr, $($tt:tt)+) => { $crate::lisp::LispObject::cons($arg, list!($($tt)+)) };
    ($arg:expr) => { $crate::lisp::LispObject::cons($arg, list!()) };
    () => { crate::remacs_sys::Qnil };
}

/// Macro that expands to nothing, but is used at build time to
/// generate the starting symbol table. Equivalent to the DEFSYM
/// macro. See also lib-src/make-docfile.c
macro_rules! def_lisp_sym {
    ($name:expr, $value:expr) => {};
}

/// Macros we use to define forwarded Lisp variables.
/// These are used in the syms_of_FILENAME functions.
///
/// An ordinary (not in buffer_defaults, per-buffer, or per-keyboard)
/// lisp variable is actually a field in `struct emacs_globals'.
///
/// In the C code, the field's name begins with "f_", which is a
/// convention enforced by these macros.  Each such global has a
/// corresponding #define in globals.h; the plain name should be used
/// in the C code.
///
/// E.g., the global "cons_cells_consed" is declared as "int
/// f_cons_cells_consed" in globals.h, but there is a define:
///
///    #define cons_cells_consed globals.f_cons_cells_consed
///
/// All C code uses the `cons_cells_consed' name.
///
/// As the Rust macro system has identifier hygine, the Rust code's
/// version of the struct emacs_globals does not include the f_ prefix
/// on the field names, and Rust code accesses the fields directly,
/// rather than through a macro.
///
/// This is all done this way to support indirection for
/// multi-threaded Emacs.
#[macro_export]
macro_rules! defvar_lisp {
    ($field_name:ident, $lisp_name:expr, $value:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            #[allow(const_err)]
            static mut o_fwd: crate::hacks::Hack<crate::data::Lisp_Objfwd> =
                unsafe { crate::hacks::Hack::uninitialized() };
            crate::remacs_sys::defvar_lisp(
                o_fwd.get_mut(),
                concat!($lisp_name, "\0").as_ptr() as *const i8,
                &mut crate::remacs_sys::globals.$field_name,
            );
            crate::remacs_sys::globals.$field_name = $value;
        }
    }};
}
#[macro_export]
macro_rules! defvar_lisp_nopro {
    ($field_name:ident, $lisp_name:expr, $value:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            #[allow(const_err)]
            static mut o_fwd: ::hacks::Hack<::data::Lisp_Objfwd> =
                unsafe { ::hacks::Hack::uninitialized() };
            ::remacs_sys::defvar_lisp_nopro(
                o_fwd.get_mut(),
                concat!($lisp_name, "\0").as_ptr() as *const i8,
                &mut ::remacs_sys::globals.$field_name,
            );
            ::remacs_sys::globals.$field_name = $value;
        }
    }};
}
#[macro_export]
macro_rules! defvar_bool {
    ($field_name:ident, $lisp_name:expr, $value:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            #[allow(const_err)]
            static mut o_fwd: ::hacks::Hack<::data::Lisp_Boolfwd> =
                unsafe { ::hacks::Hack::uninitialized() };
            ::remacs_sys::defvar_bool(
                o_fwd.get_mut(),
                concat!($lisp_name, "\0").as_ptr() as *const i8,
                &mut ::remacs_sys::globals.$field_name,
            );
            ::remacs_sys::globals.$field_name = $value;
        }
    }};
}
#[macro_export]
macro_rules! defvar_int {
    ($field_name:ident, $lisp_name:expr, $value:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            #[allow(const_err)]
            static mut o_fwd: ::hacks::Hack<::data::Lisp_Intfwd> =
                unsafe { ::hacks::Hack::uninitialized() };
            ::remacs_sys::defvar_int(
                o_fwd.get_mut(),
                concat!($lisp_name, "\0").as_ptr() as *const i8,
                &mut ::remacs_sys::globals.$field_name,
            );
            ::remacs_sys::globals.$field_name = $value;
        }
    }};
}

#[macro_export]
macro_rules! defvar_kboard {
    ($vname:ident, $lisp_name:expr) => {{
        #[allow(unused_unsafe)]
        unsafe {
            #[allow(const_err)]
            static mut o_fwd: crate::hacks::Hack<crate::data::Lisp_Kboard_Objfwd> =
                unsafe { crate::hacks::Hack::uninitialized() };
            crate::lread::defvar_kboard_offset(
                o_fwd.get_mut(),
                concat!($lisp_name, "\0").as_ptr() as *const i8,
                ::field_offset::offset_of!(crate::remacs_sys::kboard => $vname),
            );
        }
    }};
}

/// Similar to defvar_lisp but define a variable whose value is the
/// Lisp_Object stored in the current buffer.  LNAME is the Lisp-level
/// variable name.  VNAME is the name of the buffer slot.  PREDICATE
/// is nil for a general Lisp variable.  If PREDICATE is non-nil, then
/// only Lisp values that satisfies the PREDICATE are allowed (except
/// that nil is allowed too).  DOC is a dummy where you write the doc
/// string as a comment.
#[macro_export]
macro_rules! defvar_per_buffer {
    ($vname:ident, $lname:expr, $pred:ident) => {{
        #[allow(unused_unsafe)]
        unsafe {
            #[allow(const_err)]
            static mut o_fwd: crate::hacks::Hack<crate::data::Lisp_Buffer_Objfwd> =
                unsafe { crate::hacks::Hack::uninitialized() };
            crate::lread::defvar_per_buffer_offset(
                o_fwd.get_mut(),
                concat!($lname, "\0").as_ptr() as *const i8,
                ::field_offset::offset_of!(crate::remacs_sys::Lisp_Buffer => $vname),
                $pred,
            );
        }
    }};
}

#[allow(unused_macros)]
macro_rules! declare_GC_protected_static {
    ($var: ident, $value: expr) => {
        static mut $var: LispObject = $value;
    };
}

macro_rules! verify_lisp_type {
    ($obj:expr, Qarrayp) => {
        if !$obj.is_array() {
            wrong_type!(crate::remacs_sys::Qarrayp, $obj);
        }
    };
    ($n:expr, Qcharacterp) => {
        if $n < 0 || $n > (crate::remacs_sys::EmacsInt::from($crate::multibyte::MAX_CHAR)) {
            wrong_type!(
                crate::remacs_sys::Qcharacterp,
                $crate::lisp::LispObject::from($n)
            );
        }
    };
    ($obj:expr, Qstringp) => {
        if !$obj.is_string() {
            wrong_type!(crate::remacs_sys::Qstringp, $obj);
        }
    };
}

/// Get the index of `ident` into buffer's `local_flags` array. This
/// value will be stored in the variable `buffer_local_flags` of type
/// buffer

// This is equivalent to C's PER_BUFFER_VAR_IDX
macro_rules! per_buffer_var_idx {
    ($field: ident) => {
        #[allow(unused_unsafe)]
        (unsafe { crate::remacs_sys::buffer_local_flags.$field }).as_natnum_or_error() as usize
    };
}

// Creates a Lisp_String from $string on the stack and a LispObject
// pointing the Lisp_String. Assigns the LispObject to $name. See
// `generate-new-buffer-name` for sample usage.
#[macro_export]
#[allow(unused_macros)]
macro_rules! local_unibyte_string {
    ($name: ident, $string: expr) => {
        let bytes = unsafe { ($string).as_mut_str().as_bytes_mut() };
        let len = bytes.len() as ::libc::ptrdiff_t;
        let mut obj = crate::remacs_sys::Lisp_String {
            u: crate::remacs_sys::Lisp_String__bindgen_ty_1 {
                s: crate::remacs_sys::Lisp_String__bindgen_ty_1__bindgen_ty_1 {
                    size: len,
                    size_byte: -1,
                    intervals: ::std::ptr::null_mut(),
                    data: bytes.as_mut_ptr() as *mut u8,
                },
            },
        };
        let $name = crate::lisp::LispObject::from(crate::lisp::ExternalPtr::new(
            &mut obj as *mut crate::remacs_sys::Lisp_String,
        ));
    };
}

#[test]
pub fn test_local_unibyte_string() {
    let mut s = String::from("abc");
    local_unibyte_string!(a, s);
    assert_eq!(crate::multibyte::LispStringRef::from(a).byte_at(0), b'a');
    assert_eq!(crate::multibyte::LispStringRef::from(a).len_chars(), 3);
    s = String::from("defg");
    local_unibyte_string!(a, s);
    assert_eq!(crate::multibyte::LispStringRef::from(a).byte_at(0), b'd');
    assert_eq!(crate::multibyte::LispStringRef::from(a).len_chars(), 4);
}
