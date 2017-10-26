extern crate libc;

use std::env;
use std::io::{BufRead, BufReader, Write};
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

fn generate_definitions() {
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

    write!(file, "pub type EmacsInt = {};\n", integer_type_item.0).expect("Write error!");
    write!(file, "pub type EmacsUint = {};\n", integer_type_item.1).expect("Write error!");
    write!(
        file,
        "pub const EMACS_INT_MAX: EmacsInt = {};\n",
        integer_max_constant(integer_type_item.2)
    ).expect("Write error!");

    write!(
        file,
        "pub const EMACS_INT_SIZE: EmacsInt = {};\n",
        integer_type_item.2
    ).expect("Write error!");

    write!(file, "pub type EmacsDouble = {};\n", float_type_item.0).expect("Write error!");
    write!(
        file,
        "pub const EMACS_FLOAT_SIZE: EmacsInt = {};\n",
        max(float_type_item.1, actual_ptr_size)
    ).expect("Write error!");

    let bits = 8; // bits in a byte.
    let gc_type_bits = 3;
    write!(file, "pub const GCTYPEBITS: EmacsInt = {};\n", gc_type_bits).expect("Write error!");

    let uint_max_len = integer_type_item.2 * bits;
    let int_max_len = uint_max_len - 1;
    let val_max_len = int_max_len - (gc_type_bits - 1);
    let use_lsb_tag = val_max_len - 1 < int_max_len;
    write!(
        file,
        "pub const USE_LSB_TAG: bool = {};\n",
        if use_lsb_tag { "true" } else { "false" }
    ).expect("Write error!");
}

fn generate_globals() {
    let in_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("..")
        .join("..")
        .join("src")
        .join("globals.h");
    let in_file = BufReader::new(File::open(in_path).expect("Failed to open globals file"));
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("globals.rs");
    let mut out_file = File::create(out_path).expect("Failed to create definition file");

    write!(out_file, "#[allow(unused)]\n").expect("Write error!");
    write!(out_file, "#[repr(C)]\n").expect("Write error!");
    write!(out_file, "pub struct emacs_globals {{\n").expect("Write error!");

    for line in in_file.lines() {
        let line = line.expect("Read error!");
        if line.starts_with("  ") {
            let mut parts = line.trim().trim_matches(';').split(' ');
            let vtype = parts.next().unwrap();
            let vname = parts.next().unwrap();
            write!(
                out_file,
                "    pub {}: {},\n",
                vname,
                match vtype {
                    "EMACS_INT" => "EmacsInt",
                    t => t,
                }
            ).expect("Write error!");
        }
        if line.starts_with('}') {
            break;
        }
    }

    write!(out_file, "}}\n").expect("Write error!");
}

fn main() {
    generate_definitions();
    generate_globals();
}
