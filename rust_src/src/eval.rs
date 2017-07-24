//! Generic Lisp eval functions and macros.

/// Macro to generate an error with a list from any number of arguments.
/// Replaces xsignal0, etc. in the C layer.
///
/// Like `Fsignal`, but never returns. Can be used for any error
/// except `Qquit`, which can return from `Fsignal`. See the elisp docstring
/// for `signal` for an explanation of the arguments.
macro_rules! xsignal {
    ($symbol:expr) => {{
        unsafe {
            ::remacs_sys::Fsignal($symbol, ::remacs_sys::Qnil);
        }
    }};
    ($symbol:expr, $arg:expr) => {{
        let list = $crate::lisp::LispObject::cons($arg, $crate::lisp::LispObject::constant_nil());
        unsafe {
            ::remacs_sys::Fsignal($symbol, list.to_raw());
        }
    }};
    ($symbol:expr, $arg1:expr, $arg2:expr) => {{
        let list = $crate::lisp::LispObject::cons(
            $arg1,
            $crate::lisp::LispObject::cons($arg2, $crate::lisp::LispObject::constant_nil())
        );
        unsafe {
            ::remacs_sys::Fsignal($symbol, list.to_raw());
        }
    }};
    ($symbol:expr, $($arg:expr),*) => {{
        let mut argsarray = [$($arg),*];
        unsafe {
            ::remacs_sys::Fsignal($symbol,
                                  $crate::lists::list(&mut argsarray[..]).to_raw());
        }
    }}
}

/// Macro to call Lisp functions with any number of arguments.
/// Replaces CALLN, call1, etc. in the C layer.
macro_rules! call {
    ($func:expr, $($arg:expr),*) => {{
        let mut argsarray = [$func.to_raw(), $($arg.to_raw()),*];
        unsafe {
            LispObject::from_raw(
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
            ::remacs_sys::make_string($str.as_ptr() as *const i8,
                                      $str.len() as ::libc::ptrdiff_t)
        };
        xsignal!(::remacs_sys::Qerror, $crate::lisp::LispObject::from_raw(strobj));
    }};
    ($fmtstr:expr, $($arg:expr),*) => {{
        let formatted = format!($fmtstr, $($arg),*);
        let strobj = unsafe {
            ::remacs_sys::make_string(formatted.as_ptr() as *const i8,
                                      formatted.len() as ::libc::ptrdiff_t)
        };
        xsignal!(::remacs_sys::Qerror, $crate::lisp::LispObject::from_raw(strobj));
    }}
}

/// Macro to format a "wrong argument type" error message.
macro_rules! wrong_type {
    ($pred:expr, $arg:expr) => {{
        xsignal!(::remacs_sys::Qwrong_type_argument, LispObject::from_raw(unsafe { $pred }), $arg);
    }}
}
