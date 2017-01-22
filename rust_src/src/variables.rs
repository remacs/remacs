//! This module contains Lisp variables related functions.

/// Functions from `lread.c`.
pub mod ffi {
    use libc;

    use lisp::LispObjfwd;
    use lisp::LispObject;

    extern "C" {
        pub fn defvar_lisp(o_fwd: *mut LispObjfwd,
                           namestring: *const libc::c_char,
                           address: *mut LispObject);
    }
}

/// # Safety
/// This macro is `unsafe` and should be used within an `unsafe` block.
///
/// Also `lname` should be `&'static [u8]` otherwise **undefined behavior** can
/// happen.
///
/// # Port notes
/// This is the equivalent of `DEFVAR_LISP` in C code.
///
/// When porting a `DEFVAR_LISP` expression from C, `vname` **SHOULD** change
/// to `f_vname`, because `EmacsGlobals` fields are prefixed with `f_` and
/// there isn't a stable method for concatenating identifiers in rust
/// (see [this][concat_ident]).
///
/// ### Conversion example
/// Example C code:
///
/// ```c
/// DEFVAR_LISP ("command-line-args", Vcommand_line_args,
///               doc: /* Args passed by shell to Emacs, as a list of strings.
/// Many arguments are deleted from the list as they are processed.  */);
/// ```
///
/// Rust conversion:
///
/// ```rust
/// defvar!(b"command-line-args", f_Vcommand_line_args);
/// ```
///
/// [concat_ident]: https://doc.rust-lang.org/std/macro.concat_idents.html
#[macro_export]
macro_rules! defvar {
    ($lname:expr, $vname:ident) => {
        {
            static mut o_fwd: $crate::lisp::LispObjfwd = $crate::lisp::LispObjfwd {
                ty: $crate::lisp::LispFwdType::Obj,
                objvar: 0 as *mut $crate::lisp::LispObject,
            };

            $crate::variables::ffi::defvar_lisp(&mut o_fwd, $lname.as_ptr() as *const $crate::libc::c_char, &mut $crate::globals::globals.$vname)
        }
    }
}
