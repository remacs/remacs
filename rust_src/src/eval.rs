//! Generic Lisp eval functions and macros.

/// Macro to generate an error with a list from any number of arguments.
/// Replaces xsignal0, etc. in the C layer.
///
/// Like `Fsignal`, but never returns. Can be used for any error
/// except `Qquit`, which can return from `Fsignal`. See the elisp docstring
/// for `signal` for an explanation of the arguments.
macro_rules! xsignal {
    ($symbol:expr) => {
        unsafe {
            ::remacs_sys::Fsignal($symbol, ::remacs_sys::Qnil);
        }
    };
    ($symbol:expr, $($tt:tt)+) => {
        unsafe {
            ::remacs_sys::Fsignal($symbol, list!($($tt)+).to_raw());
        }
    };
}

/// Macro to call Lisp functions with any number of arguments.
/// Replaces CALLN, call1, etc. in the C layer.
macro_rules! call {
    ($func:expr, $($arg:expr),*) => {{
        let mut argsarray = [$func.to_raw(), $($arg.to_raw()),*];
        unsafe {
            LispObject::from(
                ::remacs_sys::Ffuncall(argsarray.len() as ::libc::ptrdiff_t, argsarray.as_mut_ptr())
            )
        }
    }}
}

/// Macro to format an error message.
/// Replaces error() in the C layer.
macro_rules! error {
    ($str:expr) => {{
        let strobj = unsafe {
            ::remacs_sys::make_string($str.as_ptr() as *const ::libc::c_char,
                                      $str.len() as ::libc::ptrdiff_t)
        };
        xsignal!(::remacs_sys::Qerror, $crate::lisp::LispObject::from(strobj));
    }};
    ($fmtstr:expr, $($arg:expr),*) => {{
        let formatted = format!($fmtstr, $($arg),*);
        let strobj = unsafe {
            ::remacs_sys::make_string(formatted.as_ptr() as *const ::libc::c_char,
                                      formatted.len() as ::libc::ptrdiff_t)
        };
        xsignal!(::remacs_sys::Qerror, $crate::lisp::LispObject::from(strobj));
    }};
}

/// Macro to format a "wrong argument type" error message.
macro_rules! wrong_type {
    ($pred:expr, $arg:expr) => {
        xsignal!(::remacs_sys::Qwrong_type_argument, LispObject::from($pred), $arg);
    };
}

macro_rules! args_out_of_range {
    ($($tt:tt)+) => { xsignal!(::remacs_sys::Qargs_out_of_range, $($tt)+); };
}

macro_rules! list {
    ($arg:expr, $($tt:tt)+) => { $crate::lisp::LispObject::cons($arg, list!($($tt)+)) };
    ($arg:expr) => { $crate::lisp::LispObject::cons($arg, list!()) };
    () => { $crate::lisp::LispObject::constant_nil() };
}
