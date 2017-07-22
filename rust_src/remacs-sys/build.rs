extern crate libc;

use std::env;
use std::io::Write;
use std::fs::File;
use std::path::PathBuf;
use std::mem::size_of;
use std::cmp::max;

#[cfg(feature = "wide-emacs-int")]
const WIDE_EMACS_INT: bool = true;

#[cfg(not(feature = "wide-emacs-int"))]
const WIDE_EMACS_INT: bool = false;

fn integer_max_constant(len: usize) -> &'static str {
    match len {
        1 => "0x7F_i8",
        2 => "0x7FFF_i16",
        4 => "0x7FFFFFFF_i32",
        8 => "0x7FFFFFFFFFFFFFFF_i64",
        16 => "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF_i128",
        _ => panic!("nonstandard int size {}", len),
    }
}

fn main() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("definitions.rs");
    let mut file = File::create(out_path).expect("Failed to create definition file");

    // signed and unsigned size shall be the same.
    let integer_types = [
        ("libc::c_int", "libc::c_uint", size_of::<libc::c_int>()),
        ("libc::c_long", "libc::c_ulong", size_of::<libc::c_long>()),
        (
            "libc::c_longlong",
            "libc::c_ulonglong",
            size_of::<libc::c_longlong>(),
        ),
    ];
    let actual_ptr_size = size_of::<libc::intptr_t>();
    let usable_integers_narrow = ["libc::c_int", "libc::c_long", "libc::c_longlong"];
    let usable_integers_wide = ["libc::c_longlong"];
    let usable_integers = if !WIDE_EMACS_INT {
        usable_integers_narrow.as_ref()
    } else {
        usable_integers_wide.as_ref()
    };
    let integer_type_item = integer_types
        .iter()
        .find(|&&(n, _, l)| {
            actual_ptr_size <= l && usable_integers.iter().find(|&x| x == &n).is_some()
        })
        .expect("build.rs: intptr_t is too large!");

    let float_types = [("f64", size_of::<f64>())];

    let float_type_item = &float_types[0];

    write!(&mut file, "pub type EmacsInt = {};\n", integer_type_item.0).expect("Write error!");
    write!(&mut file, "pub type EmacsUint = {};\n", integer_type_item.1).expect("Write error!");
    write!(
        &mut file,
        "pub const EMACS_INT_MAX: EmacsInt = {};\n",
        integer_max_constant(integer_type_item.2)
    ).expect("Write error!");

    write!(
        &mut file,
        "pub const EMACS_INT_SIZE: EmacsInt = {};\n",
        integer_type_item.2
    ).expect("Write error!");

    write!(&mut file, "pub type EmacsDouble = {};\n", float_type_item.0).expect("Write error!");
    write!(
        &mut file,
        "pub const EMACS_FLOAT_SIZE: EmacsInt = {};\n",
        max(float_type_item.1, actual_ptr_size)
    ).expect("Write error!");

    let bits = 8; // bits in a byte.
    let gc_type_bits = 3;
    let uint_max_len = integer_type_item.2 * bits;
    let int_max_len = uint_max_len - 1;
    let val_max_len = int_max_len - (gc_type_bits - 1);
    let use_lsb_tag = val_max_len - 1 < int_max_len;
    write!(
        &mut file,
        "pub const USE_LSB_TAG: bool = {};\n",
        if use_lsb_tag { "true" } else { "false" }
    ).expect("Write error!");

    run_bindgen();
}

extern crate bindgen;
extern crate regex;

fn run_bindgen() {
    let bindings = bindgen::Builder::default()
        .generate_comments(true)
        .clang_arg("-I../../src")
        .clang_arg("-I../../lib")
        .header("wrapper.h")
        .hide_type("USE_LSB_TAG")
        // this is wallpaper for a bug in bindgen, we don't lose anything by it
        // https://github.com/servo/rust-bindgen/issues/687
        .hide_type("BOOL_VECTOR_BITS_PER_CHAR")
        // this is wallpaper for a function argument that shadows a static of the same name
        // https://github.com/servo/rust-bindgen/issues/840
        .hide_type("face_change")
        // these never return, and bindgen doesn't yet detect that, so we will do them manually
        .hide_type("error")
        .hide_type("circular_list")
        .hide_type("wrong_type_argument")
        .hide_type("nsberror")
        .hide_type("emacs_abort")
        .hide_type("Fsignal")
        .generate()
        .expect("Unable to generate bindings");

    // https://github.com/servo/rust-bindgen/issues/839
    let source = bindings.to_string();
    let re = regex::Regex::new(
        r"pub use self::gnutls_cipher_algorithm_t as gnutls_cipher_algorithm;",
    );
    let munged = re.unwrap().replace_all(
        &source,
        "// pub use self::gnutls_cipher_algorithm_t as gnutls_cipher_algorithm;",
    );
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs");
    let file = File::create(out_path);
    file.unwrap()
        .write_all(munged.into_owned().as_bytes())
        .unwrap();
}
