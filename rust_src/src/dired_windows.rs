use remacs_macros::lisp_fn;
use remacs_sys::{file_attributes_c, globals, Qfile_attributes, Qnil};

use lisp::{defsubr, LispObject};
use lists::car;
use strings::string_lessp;

fn file_attributes_intro(filename: LispObject, id_format: LispObject) -> LispObject {
    unsafe { file_attributes_c(filename, id_format) }
}

fn file_attributes_rust_internal2(
    dirname: LispObject,
    filename: LispObject,
    id_format: LispObject,
) -> LispObject {
    // Workaround for
    // https://github.com/Wilfred/remacs/issues/804
    // This code is just fodder to please rustc.
    if dirname.is_nil() || filename.is_nil() || id_format.is_nil() {
        return Qnil;
    }

    Qfile_attributes
}

fn get_user_real_login_name() -> LispObject {
    unsafe { globals.Vuser_real_login_name }
}

fn get_users() -> LispObject {
    let mut unames = Vec::new();
    unames.push(get_user_real_login_name());
    list(&mut unames)
}
