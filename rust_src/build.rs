extern crate bindgen;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate regex;

use std::cmp::max;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::mem::size_of;
use std::path::{Path, PathBuf};
use std::process;

use regex::Regex;

#[cfg(feature = "wide-emacs-int")]
const WIDE_EMACS_INT: bool = true;

#[cfg(not(feature = "wide-emacs-int"))]
const WIDE_EMACS_INT: bool = false;

#[cfg(feature = "ns-impl-gnustep")]
const NS_IMPL_GNUSTEP: bool = true;

#[cfg(not(feature = "ns-impl-gnustep"))]
const NS_IMPL_GNUSTEP: bool = false;

static C_NAME: &str = "c_name = \"";

/// Exit with error $code after printing the $fmtstr to stderr
macro_rules! fail_with_msg {
    ($code:expr, $modname:expr, $lineno:expr, $($arg:expr),*) => {{
        eprintln!("In {} on line {}", $modname, $lineno);
        eprintln!($($arg),*);
        process::exit($code);
    }};
}

struct LintMsg {
    modname: String,
    lineno: u32,
    msg: String,
}

impl LintMsg {
    fn new(modname: &str, lineno: u32, msg: String) -> Self {
        Self {
            modname: modname.to_string(),
            lineno: lineno,
            msg: msg,
        }
    }

    fn fail(self, code: i32) -> ! {
        fail_with_msg!(code, self.modname, self.lineno, "{}", self.msg);
    }
}

enum BuildError {
    IOError(io::Error),
    Lint(LintMsg),
}

impl From<io::Error> for BuildError {
    fn from(e: io::Error) -> Self {
        BuildError::IOError(e)
    }
}

impl From<LintMsg> for BuildError {
    fn from(e: LintMsg) -> Self {
        BuildError::Lint(e)
    }
}

#[derive(Clone)]
struct ModuleInfo {
    pub name: String,
    pub path: PathBuf,
}

impl ModuleInfo {
    pub fn from_path(mod_path: &PathBuf) -> Option<ModuleInfo> {
        // in order to parse correctly, determine where the code lives.
        // For submodules that will be in a mod.rs file.
        if mod_path.is_dir() {
            let tmp = path_as_str(mod_path.file_name()).to_string();
            let path = mod_path.join("mod.rs");
            if path.is_file() {
                return Some(ModuleInfo {
                    path: path,
                    name: tmp,
                });
            }
        } else if let Some(ext) = mod_path.extension() {
            if ext == "rs" {
                return Some(ModuleInfo {
                    path: mod_path.clone(),
                    name: path_as_str(mod_path.file_stem()).to_string(),
                });
            }
        }

        None
    }
}

struct ModuleData {
    pub info: ModuleInfo,
    pub c_exports: Vec<String>,
    pub lisp_fns: Vec<String>,
    pub protected_statics: Vec<String>,
}

impl ModuleData {
    pub fn new(info: ModuleInfo) -> Self {
        Self {
            info: info,
            c_exports: Vec::new(),
            lisp_fns: Vec::new(),
            protected_statics: Vec::new(),
        }
    }
}

struct ModuleParser<'a> {
    info: &'a ModuleInfo,
    lineno: u32,
}

impl<'a> ModuleParser<'a> {
    pub fn new(mod_info: &'a ModuleInfo) -> Self {
        ModuleParser {
            info: mod_info,
            lineno: 0,
        }
    }

    pub fn run<R>(&mut self, in_file: R) -> Result<ModuleData, BuildError>
    where
        R: BufRead,
    {
        let mut mod_data = ModuleData::new(self.info.clone());
        let mut reader = in_file.lines();
        let mut has_include = false;

        while let Some(next) = reader.next() {
            let line = next?;
            self.lineno += 1;

            if line.starts_with(' ') {
                continue;
            }

            if line.starts_with("declare_GC_protected_static!") {
                let var = self.parse_gc_protected_static(&line)?;
                mod_data.protected_statics.push(var);
            } else if line.starts_with("#[no_mangle]") {
                if let Some(next) = reader.next() {
                    let line = next?;

                    if let Some(func) = self.parse_c_export(&line, None)? {
                        self.lint_nomangle(&line)?;
                        mod_data.c_exports.push(func);
                    }
                } else {
                    self.fail(1, "unexpected end of file");
                }
            } else if line.starts_with("#[lisp_fn") {
                let line = if line.ends_with("]") {
                    line.clone()
                } else {
                    let mut line = line.clone();
                    loop {
                        if let Some(next) = reader.next() {
                            let l = next?;
                            if !l.ends_with(")]") {
                                line += &l;
                            } else {
                                line += &l;
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    line
                };

                let name = if let Some(begin) = line.find(C_NAME) {
                    let start = begin + C_NAME.len();
                    let end = line[start..].find('"').unwrap() + start;
                    let name = line[start..end].to_string();
                    if name.starts_with('$') {
                        // Ignore macros, nothing we can do with them
                        continue;
                    }

                    Some(name)
                } else {
                    None
                };

                if let Some(next) = reader.next() {
                    let line = next?;

                    if let Some(func) = self.parse_c_export(&line, name)? {
                        mod_data.lisp_fns.push(func);
                    }
                } else {
                    self.fail(1, "unexpected end of file");
                }
            } else if line.starts_with("include!(concat!(env!(\"OUT_DIR\"),") {
                has_include = true;
            } else if line.starts_with("/*") && !line.ends_with("*/") {
                while let Some(next) = reader.next() {
                    let line = next?;
                    if line.ends_with("*/") {
                        break;
                    }
                }
            }
        }

        if !has_include && !(mod_data.lisp_fns.is_empty() && mod_data.protected_statics.is_empty())
        {
            let msg = format!(
                "{} is missing the required include for protected statics or lisp_fn exports.",
                path_as_str(self.info.path.file_name()).to_string()
            );

            self.fail(2, &msg);
        }

        Ok(mod_data)
    }

    fn fail(&mut self, code: i32, msg: &str) -> ! {
        fail_with_msg!(code, &self.info.name, self.lineno, "{}", msg);
    }

    /// Handle both no_mangle and lisp_fn functions
    fn parse_c_export(
        &mut self,
        line: &str,
        name: Option<String>,
    ) -> Result<Option<String>, LintMsg> {
        let name = self.validate_exported_function(name, line, "function must be public.")?;
        if let Some(func) = name {
            Ok(Some(func))
        } else {
            Ok(None)
        }
    }

    fn parse_gc_protected_static(&mut self, line: &str) -> Result<String, LintMsg> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r#"GC_protected_static!\((.+), .+\);"#).unwrap();
        }

        match RE.captures(line) {
            Some(caps) => {
                let name = caps[1].to_string();
                Ok(name)
            }
            None => Err(LintMsg::new(
                &self.info.name,
                self.lineno,
                "could not parse protected static".to_string(),
            )),
        }
    }

    // Determine if a function is exported correctly and return that function's name or None.
    fn validate_exported_function(
        &mut self,
        name: Option<String>,
        line: &str,
        msg: &str,
    ) -> Result<Option<String>, LintMsg> {
        match name.or_else(|| get_function_name(line)) {
            Some(name) => {
                if line.starts_with("pub ") {
                    Ok(Some(name))
                } else if line.starts_with("fn ") {
                    Err(LintMsg::new(
                        &self.info.name,
                        self.lineno,
                        format!("\n`{}` is not public.\n{}", name, msg),
                    ))
                } else {
                    eprintln!(
                        "Unhandled code in the {} module at line {}",
                        self.info.name, self.lineno
                    );
                    unreachable!();
                }
            }
            None => Ok(None),
        }
    }

    fn lint_nomangle(&mut self, line: &str) -> Result<(), LintMsg> {
        if !(line.starts_with("pub extern \"C\" ") || line.starts_with("pub unsafe extern \"C\" "))
        {
            Err(LintMsg::new(
                &self.info.name,
                self.lineno,
                "'no_mangle' functions exported for C need 'extern \"C\"' too.".to_string(),
            ))
        } else {
            Ok(())
        }
    }
}

// Parse the function name out of a line of source
fn get_function_name(line: &str) -> Option<String> {
    if let Some(pos) = line.find('(') {
        if let Some(fnpos) = line.find("fn ") {
            let name = line[(fnpos + 3)..pos].trim();
            return Some(name.to_string());
        }
    }

    None
}

fn handle_file(mod_path: &PathBuf) -> Result<Option<ModuleData>, BuildError> {
    if let Some(mod_info) = ModuleInfo::from_path(mod_path) {
        let fp = match File::open(mod_info.path.clone()) {
            Ok(f) => f,
            Err(e) => {
                return Err(io::Error::new(
                    e.kind(),
                    format!("Failed to open {}: {}", mod_info.path.to_string_lossy(), e),
                )
                .into())
            }
        };

        let mut parser = ModuleParser::new(&mod_info);
        let mod_data = parser.run(BufReader::new(fp))?;
        Ok(Some(mod_data))
    } else {
        Ok(None)
    }
}

// Transmute &OsStr to &str
fn path_as_str(path: Option<&OsStr>) -> &str {
    path.and_then(|p| p.to_str())
        .unwrap_or_else(|| panic!("Cannot understand string: {:?}", path))
}

fn env_var(name: &str) -> String {
    env::var(name).unwrap_or_else(|e| panic!("Could not find {} in environment: {}", name, e))
}

// What to ignore when walking the list of files
fn ignore(path: &str) -> bool {
    path == "" || path.starts_with('.') || path == "lib.rs" || path == "functions.rs"
}

fn generate_include_files() -> Result<(), BuildError> {
    let mut modules: Vec<ModuleData> = Vec::new();

    let in_path: PathBuf = [&env_var("CARGO_MANIFEST_DIR"), "src"].iter().collect();
    for entry in fs::read_dir(in_path)? {
        let mod_path = entry?.path();

        if !ignore(path_as_str(mod_path.file_name())) {
            if let Some(mod_data) = handle_file(&mod_path)? {
                modules.push(mod_data);
            }
        }
    }

    if modules.is_empty() {
        return Ok(());
    }

    let out_path: PathBuf = [&env_var("OUT_DIR"), "c_exports.rs"].iter().collect();
    let mut out_file = File::create(out_path)?;

    for mod_data in &modules {
        for func in &mod_data.c_exports {
            write!(
                out_file,
                "pub use crate::{}::{};\n",
                mod_data.info.name, func
            )?;
        }
        for func in &mod_data.lisp_fns {
            write!(
                out_file,
                "pub use crate::{}::F{};\n",
                mod_data.info.name, func
            )?;
        }
    }
    write!(out_file, "\n")?;

    write!(
        out_file,
        "#[no_mangle]\npub extern \"C\" fn rust_init_syms() {{\n"
    )?;
    for mod_data in &modules {
        let exports_path: PathBuf = [
            env_var("OUT_DIR"),
            [&mod_data.info.name, "_exports.rs"].concat(),
        ]
        .iter()
        .collect();
        if exports_path.exists() {
            // Start with a clean slate
            fs::remove_file(&exports_path)?;
        }

        if !mod_data.lisp_fns.is_empty() {
            let mut file = File::create(&exports_path)?;
            write!(
                file,
                "export_lisp_fns! {{ {} }}\n",
                mod_data.lisp_fns.join(", ")
            )?;

            write!(out_file, "    {}::rust_init_syms();\n", mod_data.info.name)?;
        }

        if !mod_data.protected_statics.is_empty() {
            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open(exports_path)?;
            write!(
                file,
                "protect_statics_from_GC! {{ {} }}\n",
                mod_data.protected_statics.join(", ")
            )?;

            write!(
                out_file,
                "    {}::rust_static_syms();\n",
                mod_data.info.name
            )?;
        }
    }

    // Add this one by hand.
    write!(out_file, "    floatfns::rust_init_extra_syms();\n")?;
    write!(out_file, "}}\n")?;

    Ok(())
}

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

fn generate_globals() {
    let in_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("..")
        .join("src")
        .join("globals.h");
    let in_file = BufReader::new(File::open(in_path).expect("Failed to open globals file"));
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("globals.rs");
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

fn run_bindgen() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs");
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
                .header("wrapper.h")
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

fn main() {
    for varname in ["EMACS_CFLAGS", "SRC_HASH"].iter() {
        println!("cargo:rerun-if-env-changed={}", varname);
    }

    if let Err(e) = generate_include_files() {
        match e {
            BuildError::IOError(msg) => {
                eprintln!("{}", msg);
                process::exit(3);
            }
            BuildError::Lint(msg) => {
                msg.fail(1);
            }
        }
    }
    generate_definitions();
    run_bindgen();
    generate_globals();
}
