use crate::{
    lisp::LispObject,
    lists::list,
    remacs_sys::{
        directory_files_and_attributes_c, directory_files_c, file_attributes_c, globals,
        Qfile_attributes, Qnil,
    },
};

pub fn directory_files(
    directory: LispObject,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
) -> LispObject {
    unsafe { directory_files_c(directory, full, match_re, nosort) }
}

pub fn directory_files_and_attributes(
    directory: LispObject,
    full: LispObject,
    match_re: LispObject,
    nosort: LispObject,
    id_format: LispObject,
) -> LispObject {
    unsafe { directory_files_and_attributes_c(directory, full, match_re, nosort, id_format) }
}

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

pub fn get_groups() -> LispObject {
    Qnil
}
