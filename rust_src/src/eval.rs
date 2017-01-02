use lisp::{LispObject, Qnil};

extern "C" {
    fn Fsignal(error_symbol: LispObject, data: LispObject);
}

/// Signal an error in Emacs.
///
/// Like `Fsignal`, but never returns. Can be used for any error
/// except `Qquit`, which can return from `Fsignal`. See the elisp docstring
/// for `signal` for an explanation of the arguments.
fn xsignal(error_symbol: LispObject, data: LispObject) {
    unsafe {
        Fsignal(error_symbol, data);
    }
    panic!("Fsignal should not return in xsignal");
}

/// Convenience function for calling `xsignal` with an empty list.
pub fn xsignal0(error_symbol: LispObject) {
    xsignal(error_symbol, Qnil);
}
