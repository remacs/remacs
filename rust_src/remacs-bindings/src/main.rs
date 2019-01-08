extern crate bindgen;

use std::cmp::max;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::mem::size_of;
use std::path::{Path, PathBuf};
use std::process;

#[cfg(feature = "wide-emacs-int")]
const WIDE_EMACS_INT: bool = true;

#[cfg(not(feature = "wide-emacs-int"))]
const WIDE_EMACS_INT: bool = false;

#[cfg(feature = "ns-impl-gnustep")]
const NS_IMPL_GNUSTEP: bool = true;

#[cfg(not(feature = "ns-impl-gnustep"))]
const NS_IMPL_GNUSTEP: bool = false;

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

#[derive(Eq, PartialEq)]
enum ParseState {
    ReadingGlobals,
    ReadingSymbols,
    Complete,
}

fn generate_definitions(path: &str) {
    let out_path = PathBuf::from(path);
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
    )
    .expect("Write error!");

    write!(
        file,
        "pub const EMACS_INT_SIZE: EmacsInt = {};\n",
        integer_type_item.2
    )
    .expect("Write error!");

    write!(file, "pub type EmacsDouble = {};\n", float_type_item.0).expect("Write error!");
    write!(
        file,
        "pub const EMACS_FLOAT_SIZE: EmacsInt = {};\n",
        max(float_type_item.1, actual_ptr_size)
    )
    .expect("Write error!");

    if NS_IMPL_GNUSTEP {
        write!(file, "pub type BoolBF = libc::c_uint;\n").expect("Write error!");
    } else {
        // There is no such thing as a libc::cbool
        // See https://users.rust-lang.org/t/is-rusts-bool-compatible-with-c99--bool-or-c-bool/3981
        write!(file, "pub type BoolBF = bool;\n").expect("Write error!");
    }

    let bits = 8; // bits in a byte.
    let gc_type_bits = 3;
    let uint_max_len = integer_type_item.2 * bits;
    let int_max_len = uint_max_len - 1;
    let val_max_len = int_max_len - (gc_type_bits - 1);
    let use_lsb_tag = val_max_len - 1 < int_max_len;
    write!(
        file,
        "pub const USE_LSB_TAG: bool = {};\n",
        if use_lsb_tag { "true" } else { "false" }
    )
    .expect("Write error!");
}

fn generate_globals(path: &str) {
    let in_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("..")
        .join("..")
        .join("src")
        .join("globals.h");
    let in_file = BufReader::new(File::open(in_path).expect("Failed to open globals file"));
    let out_path = PathBuf::from(path);
    let mut out_file = File::create(out_path).expect("Failed to create definition file");
    let mut parse_state = ParseState::ReadingGlobals;

    write!(out_file, "#[allow(unused)]\n").expect("Write error!");
    write!(out_file, "#[repr(C)]\n").expect("Write error!");
    write!(out_file, "pub struct emacs_globals {{\n").expect("Write error!");

    for line in in_file.lines() {
        let line = line.expect("Read error!");
        match parse_state {
            ParseState::ReadingGlobals => {
                if line.starts_with("  ") {
                    let mut parts = line.trim().trim_matches(';').split(' ');
                    let vtype = parts.next().unwrap();
                    let vname = parts.next().unwrap().splitn(2, "_").nth(1).unwrap();

                    write!(
                        out_file,
                        "    pub {}: {},\n",
                        vname,
                        match vtype {
                            "EMACS_INT" => "EmacsInt",
                            "bool_bf" => "BoolBF",
                            "Lisp_Object" => "LispObject",
                            t => t,
                        }
                    )
                    .expect("Write error!");
                }
                if line.starts_with('}') {
                    write!(out_file, "}}\n").expect("Write error!");
                    parse_state = ParseState::ReadingSymbols;
                    continue;
                }
            }

            ParseState::ReadingSymbols => {
                if line.trim().starts_with("#define") {
                    let mut parts = line.split(' ');
                    let _ = parts.next().unwrap(); // The #define
                                                   // Remove the i in iQnil
                    let (_, symbol_name) = parts.next().unwrap().split_at(1);
                    let value = parts.next().unwrap();
                    write!(
                        out_file,
                        "pub const {}: LispObject = crate::lisp::LispObject( \
                         {} * (std::mem::size_of::<Lisp_Symbol>() as EmacsInt));\n",
                        symbol_name, value
                    )
                    .expect("Write error in reading symbols stage");
                } else if line.trim().starts_with("_Noreturn") {
                    parse_state = ParseState::Complete
                }
            }

            ParseState::Complete => {
                break;
            }
        };
    }
}

fn run_bindgen(path: &str) {
    let out_path = PathBuf::from(path);
    let skip = std::env::var_os("SKIP_BINDINGS");
    if skip.is_some() {
        // create bindings.rs if it doesn't already exist, leaving it empty.
        OpenOptions::new()
            .write(true)
            .create(true)
            .open(out_path)
            .expect("Could not create bindings.rs");
        return;
    }
    let cflags = std::env::var_os("EMACS_CFLAGS");
    match cflags {
        None => {
            if out_path.exists() {
                println!("No EMACS_CFLAGS specified, but {:?} already exists so we'll just skip the bindgen step this time.", out_path);
            } else {
                panic!("No EMACS_CFLAGS were specified, and we need them in order to run bindgen.");
            }
        }
        Some(cflags) => {
            let mut builder = bindgen::Builder::default()
                .rust_target(bindgen::RustTarget::Nightly)
                .generate_comments(true);

            let cflags_str = cflags.to_string_lossy();
            let mut processed_args: Vec<String> = Vec::new();
            for arg in cflags_str.split(' ') {
                if arg.starts_with("-I") {
                    // we're running clang from a different directory, so we have to adjust any relative include paths
                    let path = Path::new("../src").join(arg.get(2..).unwrap());
                    let buf = std::fs::canonicalize(path).unwrap();
                    processed_args.push(String::from("-I") + &buf.to_string_lossy());
                } else {
                    if !arg.is_empty() && !arg.starts_with("-M") && !arg.ends_with(".d") {
                        processed_args.push(arg.into());
                    }
                };
            }
            builder = builder.clang_args(processed_args);
            if cfg!(target_os = "windows") {
                builder = builder.clang_arg("-I../nt/inc");
                builder =
                    builder.clang_arg("-Ic:\\Program Files\\LLVM\\lib\\clang\\6.0.0\\include");
            }

            builder = builder
                .clang_arg("-Demacs")
                .header("../rust_src/wrapper.h")
                .generate_inline_functions(true)
                .derive_default(true)
                .ctypes_prefix("::libc")
                // we define these ourselves, for various reasons
                .blacklist_item("Lisp_Object")
                .blacklist_item("emacs_globals")
                .blacklist_item("Q.*") // symbols like Qnil and so on
                .blacklist_item("USE_LSB_TAG")
                .blacklist_item("VALMASK")
                .blacklist_item("PSEUDOVECTOR_FLAG")
                .blacklist_item("Fmapc")
                // these two are found by bindgen on mac, but not linux
                .blacklist_item("EMACS_INT_MAX")
                .blacklist_item("VAL_MAX")
                // this is wallpaper for a bug in bindgen, we don't lose much by it
                // https://github.com/servo/rust-bindgen/issues/687
                .blacklist_item("BOOL_VECTOR_BITS_PER_CHAR")
                // this is wallpaper for a function argument that shadows a static of the same name
                // https://github.com/servo/rust-bindgen/issues/804
                .blacklist_item("face_change")
                // these never return, and bindgen doesn't yet detect that, so we will do them manually
                .blacklist_item("error")
                .blacklist_item("circular_list")
                .blacklist_item("wrong_type_argument")
                .blacklist_item("nsberror")
                .blacklist_item("emacs_abort")
                .blacklist_item("Fsignal")
                .blacklist_item("memory_full")
                .blacklist_item("wrong_choice")
                .blacklist_item("wrong_range")
                // these are defined in data.rs
                .blacklist_item("Lisp_Fwd")
                .blacklist_item("Lisp_.*fwd")
                // these are defined in remacs_lib
                .blacklist_item("timespec")
                .blacklist_item("current_timespec")
                .blacklist_item("timex")
                .blacklist_item("clock_adjtime")
                // bindgen fails to generate this one correctly; it's hard
                // https://github.com/rust-lang-nursery/rust-bindgen/issues/1318
                .blacklist_item("max_align_t")
                // by default we want C enums to be converted into a Rust module with constants in it
                .default_enum_style(bindgen::EnumVariation::ModuleConsts)
                // enums with only one variant are better as simple constants
                .constified_enum("EMACS_INT_WIDTH")
                .constified_enum("BOOL_VECTOR_BITS_PER_CHAR")
                .constified_enum("BITS_PER_BITS_WORD")
                // TODO(db48x): verify that these enums meet Rust's requirements (primarily that they have no duplicate variants)
                .rustified_enum("Lisp_Misc_Type")
                .rustified_enum("Lisp_Type")
                .rustified_enum("case_action")
                .rustified_enum("face_id")
                .rustified_enum("output_method")
                .rustified_enum("pvec_type")
                .rustified_enum("symbol_redirect")
                .rustified_enum("syntaxcode");

            if cfg!(target_os = "windows") {
                builder = builder
                    .rustified_enum("_HEAP_INFORMATION_CLASS")
                    .rustified_enum("SECURITY_IMPERSONATION_LEVEL")
                    .rustified_enum("TOKEN_INFORMATION_CLASS");
            }

            let bindings = builder
                .rustfmt_bindings(true)
                .rustfmt_configuration_file(std::fs::canonicalize("rustfmt.toml").ok())
                .generate()
                .expect("Unable to generate bindings");

            // https://github.com/rust-lang-nursery/rust-bindgen/issues/839
            let source = bindings.to_string();
            let re = regex::Regex::new(
                r"pub use self\s*::\s*gnutls_cipher_algorithm_t as gnutls_cipher_algorithm\s*;",
            );
            let munged = re.unwrap().replace_all(&source, "");
            let file = File::create(out_path);
            file.unwrap()
                .write_all(munged.into_owned().as_bytes())
                .unwrap();
        }
    }
}

fn usage() {
    println!("usage: remacs-bindings <definitions|bindings|globals> <path>");
    process::exit(1);
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    match args[1].as_str() {
        "definitions" => generate_definitions(&args[2]),
        "bindings" => run_bindgen(&args[2]),
        "globals" => generate_globals(&args[2]),
        _ => usage(),
    };
}
