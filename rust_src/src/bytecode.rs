//! Execution of byte code produced by bytecomp.el.

use remacs_macros::lisp_fn;

use crate::{lisp::LispObject, remacs_sys::exec_byte_code as c_exec_byte_code, remacs_sys::Qnil};

// Temporary Rust wrapper for C's exec_byte_code
fn rust_exec_byte_code(
    bytestr: LispObject,
    vector: LispObject,
    maxdepth: LispObject,
    args_template: LispObject,
    arg: &mut [LispObject],
) -> LispObject {
    unsafe {
        c_exec_byte_code(
            bytestr,
            vector,
            maxdepth,
            args_template,
            arg.len() as isize,
            arg.as_mut_ptr() as *mut LispObject,
        )
    }
}

/// Function used internally in byte-compiled code.
/// The first argument, BYTESTR, is a string of byte code;
/// the second, VECTOR, a vector of constants;
/// the third, MAXDEPTH, the maximum stack depth used in this function.
/// If the third argument is incorrect, Emacs may crash :(
#[lisp_fn]
pub fn byte_code(bytestr: LispObject, vector: LispObject, maxdepth: LispObject) -> LispObject {
    rust_exec_byte_code(bytestr, vector, maxdepth, Qnil, &mut [])
}

include!(concat!(env!("OUT_DIR"), "/bytecode_exports.rs"));
