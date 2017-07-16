use lisp::{LispObject, Qnil};
use remacs_sys::Fsignal;

/// Signal an error in Emacs.
///
/// Like `Fsignal`, but never returns. Can be used for any error
/// except `Qquit`, which can return from `Fsignal`. See the elisp docstring
/// for `signal` for an explanation of the arguments.
fn xsignal(error_symbol: LispObject, data: LispObject) -> ! {
    unsafe {
        Fsignal(error_symbol.to_raw(), data.to_raw());
    }
}

/// Convenience function for calling `xsignal` with an empty list.
pub fn xsignal0(error_symbol: LispObject) -> ! {
    xsignal(error_symbol, Qnil);
}

/// Convenience function for calling `xsignal` with a two-element list.
pub fn xsignal2(error_symbol: LispObject, arg1: LispObject, arg2: LispObject) -> ! {
    xsignal(
        error_symbol,
        LispObject::cons(arg1, LispObject::cons(arg2, LispObject::constant_nil())),
    )
}
