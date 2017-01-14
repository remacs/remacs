extern crate libc;

use std::env;
use std::io::Write;
use std::fs::File;
use std::path::PathBuf;
use std::mem::size_of;

#[cfg(feature = "wide-emacs-int")]
const WIDE_EMACS_INT: bool = true;

#[cfg(not(feature = "wide-emacs-int"))]
const WIDE_EMACS_INT: bool = false;

fn emacs_int_max<T>() -> String {
    match size_of::<T>() {
        1 => "0x7F_i8".to_owned(),
        2 => "0x7FFF_i16".to_owned(),
        4 => "0x7FFFFFFF_i32".to_owned(),
        8 => "0x7FFFFFFFFFFFFFFF_i64".to_owned(),
        16 => "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF_i128".to_owned(),
        _ => panic!("nonstandard int size {}", size_of::<T>()),
    }
}

fn emacs_int_types() -> (String, String, String) {
    if size_of::<libc::intptr_t>() <= size_of::<libc::c_int>() && !WIDE_EMACS_INT {
        return ("libc::c_int".to_owned(), "libc::c_uint".to_owned(), emacs_int_max::<libc::c_int>());
    }

    if size_of::<libc::intptr_t>() <= size_of::<libc::c_long>() && !WIDE_EMACS_INT {
        return ("libc::c_long".to_owned(), "libc::c_ulong".to_owned(), emacs_int_max::<libc::c_long>());
    }

    if size_of::<libc::intptr_t>() <= size_of::<libc::c_longlong>() {
        return ("libc::c_longlong".to_owned(), "libc::c_ulonglong".to_owned(), emacs_int_max::<libc::c_longlong>());
    }

    panic!("build.rs: intptr_t is too large!");
}

fn main() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("definitions.rs");
    let mut file = File::create(out_path).expect("Failed to create definition file");
    let (type1, type2, max1) = emacs_int_types();
    write!(&mut file, "pub type EmacsInt = {};\n", type1).expect("Write error!");
    write!(&mut file, "pub type EmacsUint = {};\n", type2).expect("Write error!");
    write!(&mut file, "pub const EMACS_INT_MAX: EmacsInt = {};\n", max1).expect("Write error!");
}
