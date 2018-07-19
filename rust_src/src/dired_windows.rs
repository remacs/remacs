use remacs_sys::{file_attributes_c, globals, Qfile_attributes, Qnil};

use lisp::LispObject;
use lists::list;

pub fn file_attributes_intro(filename: LispObject, id_format: LispObject) -> LispObject {
    unsafe { file_attributes_c(filename, id_format) }
}

// Workaround for
// https://github.com/Wilfred/remacs/issues/804
// That is; file_attributes_rust_internal is exported
// even though the code is in the C module. So we need
// to have something here to pls rustc.
pub fn file_attributes_rust_internal2(
    dirname: LispObject,
    filename: LispObject,
    id_format: LispObject,
) -> LispObject {
    if dirname.is_nil() || filename.is_nil() || id_format.is_nil() {
        return Qnil;
    }

    Qfile_attributes
}

fn get_user_real_login_name() -> LispObject {
    unsafe { globals.Vuser_real_login_name }
}

pub fn get_users() -> LispObject {
    let mut unames = Vec::new();
    unames.push(get_user_real_login_name());
    list(&mut unames)
}
