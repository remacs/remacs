use remacs_sys::{file_attributes_c, globals, Qfile_attributes, Qnil};

use lisp::LispObject;
use lists::list;

pub fn file_attributes_intro(filename: LispObject, id_format: LispObject) -> LispObject {
    unsafe { file_attributes_c(filename, id_format) }
}

fn get_user_real_login_name() -> LispObject {
    unsafe { globals.Vuser_real_login_name }
}

pub fn get_users() -> LispObject {
    let mut unames = Vec::new();
    unames.push(get_user_real_login_name());
    list(&mut unames)
}
