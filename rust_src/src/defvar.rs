//! defvar related functions

// TODO: Find a better name for module.

/// Contains imports declarations from C code, this means that you can't put exporting code here.
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

/// # Port notes
/// This is the equivalent of `DEFVAR_LISP` in C code.
///
/// When porting a `DEFVAR_LISP` expression from C, `vname` **SHOULD** change
/// to `f_vname`, because `EmacsGlobals` fields are prefixed with `f_` and
/// there isn't a stable method for concatenating identifiers in rust
/// (see [this][concat_ident]).
///
/// # TODO:
/// - Add missing doc field.
/// - Use `&'stattic str` instead of `&'static [libc::c_char]`?
///
/// [concat_ident]: https://doc.rust-lang.org/std/macro.concat_idents.html
#[macro_export]
macro_rules! defvar_lisp {
    ($lname:expr, $vname:ident) => {
        {
            static mut o_fwd: $crate::lisp::LispObjfwd = $crate::lisp::LispObjfwd {
                type_: $crate::lisp::LispFwdType::Obj,
                objvar: 0 as *mut $crate::lisp::LispObject,
            };

            $crate::defvar::ffi::defvar_lisp(&mut o_fwd, $lname.as_ptr() as *const $crate::libc::c_char, &mut $crate::globals::globals.$vname)
        }
    }
}
