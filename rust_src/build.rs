#![recursion_limit = "128"]

#[macro_use]
extern crate quote;
extern crate syn;

extern crate remacs_util;

use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process;

use remacs_util::{parse_lisp_fn, LispFnArgs};

mod function;

// I tried to use the num crate's FromPrimitive here but it fails to compile/parse
enum FnArgsMax {
    /// denote functions that have no limit on the maximum number of arguments.
    MANY = -2,
    /// denote functions that received unevaluated forms
    UNEVALLED = -1,
    ZERO = 0,
    ONE = 1,
    TWO = 2,
    THREE = 3,
    FOUR = 4,
    FIVE = 5,
    SIX = 6,
    SEVEN = 7,
    EIGHT = 8,
}

impl From<isize> for FnArgsMax {
    fn from(n: isize) -> FnArgsMax {
        match n {
            -2 => FnArgsMax::MANY,
            -1 => FnArgsMax::UNEVALLED,
            0 => FnArgsMax::ZERO,
            1 => FnArgsMax::ONE,
            2 => FnArgsMax::TWO,
            3 => FnArgsMax::THREE,
            4 => FnArgsMax::FOUR,
            5 => FnArgsMax::FIVE,
            6 => FnArgsMax::SIX,
            7 => FnArgsMax::SEVEN,
            8 => FnArgsMax::EIGHT,
            _ => panic!("{} is not valid for lisp_fn max args", n),
        }
    }
}

#[derive(Eq, PartialEq)]
enum ParseState {
    Looking,
    IgnoreComments,
    NoMangleFn,
}

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
    lineno: u64,
    msg: String,
}

impl LintMsg {
    fn new(modname: &str, lineno: u64, msg: String) -> Self {
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

fn lint_nomangle(line: &str, modname: &str, lineno: u64) -> Result<(), LintMsg> {
    if !(line.starts_with("pub extern \"C\" ") || line.starts_with("pub unsafe extern \"C\" ")) {
        Err(LintMsg::new(
            modname,
            lineno,
            "'no_mangle' functions exported for C need 'extern \"C\"' too.".to_string(),
        ))
    } else if line.contains(": LispObject") || line.contains("-> LispObject") {
        Err(LintMsg::new(
            modname,
            lineno,
            "functions exported to C must use 'Lisp_Object' not 'LispObject'".to_string(),
        ))
    } else {
        Ok(())
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

// Transmute &OsStr to &str
fn path_as_str(path: Option<&OsStr>) -> &str {
    path.and_then(|p| p.to_str())
        .unwrap_or_else(|| panic!("Cannot understand string: {:?}", path))
}

fn env_var(name: &str) -> String {
    env::var(name).unwrap_or_else(|e| panic!("Could not find {} in environment: {}", name, e))
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

// Determine if a function is exported correctly and return that function's name or None.
fn export_function(
    name: &str,
    line: &str,
    modname: &str,
    lineno: u64,
    msg: &str,
) -> Result<String, LintMsg> {
    if line.starts_with("pub ") {
        Ok(name.to_string())
    } else if line.starts_with("fn ") {
        Err(LintMsg::new(
            modname,
            lineno,
            format!(
                "
`{}` is not public.
{}",
                name, msg
            ),
        ))
    } else {
        eprintln!(
            "Unhandled code in the {} module at line {}",
            modname, lineno
        );
        unreachable!();
    }
}

struct Source<B> {
    lines: io::Lines<B>,
    lineno: u64,
}

impl<B: BufRead> Source<B> {
    fn new(lines: io::Lines<B>) -> Self {
        Self { lines, lineno: 0 }
    }

    pub fn line(&self) -> u64 {
        self.lineno
    }
}

impl<B: BufRead> Iterator for Source<B> {
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<io::Result<String>> {
        self.lineno += 1;
        self.lines.next()
    }
}

struct ModuleExporter {
    mod_dir: String,
    mod_path: PathBuf,
    mod_name: String,
    c_exports: Vec<String>,
    lisp_functions: Vec<(LispFnArgs, function::Function)>,
}

impl ModuleExporter {
    pub fn new(mod_dir: String, mod_path: PathBuf) -> Self {
        Self {
            mod_dir,
            mod_path,
            mod_name: "".to_string(),
            c_exports: Vec::new(),
            lisp_functions: Vec::new(),
        }
    }

    fn run(&mut self, out_file: &File) -> Result<Option<String>, BuildError> {
        let mut name: Option<String> = None;

        // in order to parse correctly, determine where the code lives
        // for submodules that will be in a mod.rs file.
        if self.mod_path.is_dir() {
            let tmp = path_as_str(self.mod_path.file_name()).to_string();
            self.mod_path = self.mod_path.join("mod.rs");
            if self.mod_path.is_file() {
                name = Some(tmp);
            }
        } else if let Some(ext) = self.mod_path.extension() {
            if ext == "rs" {
                name = Some(path_as_str(self.mod_path.file_stem()).to_string());
            }
        }

        // only handle Rust files
        if let Some(modname) = name {
            let fp = match File::open(&self.mod_path) {
                Ok(f) => f,
                Err(e) => {
                    return Err(io::Error::new(
                        e.kind(),
                        format!("Failed to open {:?}: {}", self.mod_path, e),
                    ).into())
                }
            };

            self.mod_name = modname;

            if self.parse(BufReader::new(fp))? {
                if self.export(out_file)? {
                    return Ok(Some(self.mod_name.clone()));
                }
            }
        }

        Ok(None)
    }

    fn parse<R>(&mut self, in_file: R) -> Result<bool, BuildError>
    where
        R: BufRead,
    {
        let mut parse_state = ParseState::Looking;
        let mut has_include = false;

        let mut source = Source::new(in_file.lines());

        let mut keep_going = true;
        'outer: while keep_going {
            let mut line = match source.next() {
                None => {
                    keep_going = false;
                    continue 'outer;
                }
                Some(line) => line?,
            };

            line = line.trim_right().to_string();

            match parse_state {
                ParseState::Looking => {
                    if line.starts_with("#[lisp_fn") {
                        let lisp_fn_macro = &line;

                        let mut func: Vec<String> = Vec::new();
                        let mut found = false;

                        // The syn parser needs an entire function to
                        // parse but we do not want to spend time
                        // parsing code that is not relevant. Instead just read
                        // up to the opening brace. Then tack on a closing
                        // brace to make a dummy function.

                        while !found {
                            let mut line = match source.next() {
                                None => {
                                    keep_going = false;
                                    continue 'outer;
                                }
                                Some(next) => next?,
                            };

                            line = line.trim_right().to_string();
                            found = line.contains("{");
                            func.push(line.clone());
                        }

                        func.push("}".to_string());

                        let item = func.join("\n");

                        // parse the function to learn about the arguments
                        let fn_item = syn::parse_item(&item)
                            .map_err(|e| LintMsg::new(&self.mod_name, source.line(), e))?;
                        let function = function::parse(&fn_item).map_err(|e| {
                            LintMsg::new(&self.mod_name, source.line(), e.to_string())
                        })?;

                        // now parse the macro that we know the function name
                        let lisp_fn_args =
                            parse_lisp_fn(
                                &lisp_fn_macro,
                                &function.name,
                                function.fntype.def_min_args(),
                            ).map_err(|e| LintMsg::new(&self.mod_name, source.line(), e))?;

                        let name = export_function(
                            &lisp_fn_args.c_name,
                            &item,
                            &self.mod_name,
                            source.line(),
                            "lisp_fn functions are meant to be used from Rust as well.",
                        )?;

                        self.c_exports.push(format!("F{}", name));
                        self.lisp_functions.push((lisp_fn_args.clone(), function));
                    } else if line.starts_with("#[no_mangle]") {
                        parse_state = ParseState::NoMangleFn;
                    } else if line.starts_with("/*") {
                        if !line.ends_with("*/") {
                            parse_state = ParseState::IgnoreComments;
                        }
                    } else if line == format!("include!(\"{}_exports.rs\");", self.mod_name) {
                        has_include = true;
                    }
                }

                ParseState::IgnoreComments => if line.starts_with("*/") || line.ends_with("*/") {
                    parse_state = ParseState::Looking;
                },

                // export public #[no_mangle] functions
                ParseState::NoMangleFn => {
                    if let Some(fname) = get_function_name(&line) {
                        let name = export_function(
                            &fname,
                            &line,
                            &self.mod_name,
                            source.line(),
                            "'no_mangle' function must be public.",
                        )?;

                        lint_nomangle(&line, &self.mod_name, source.line())?;
                        self.c_exports.push(name);
                    }

                    parse_state = ParseState::Looking;
                }
            }
        }

        if !(self.lisp_functions.is_empty() || has_include) {
            eprintln!("Missing: include!(\"{}_exports.rs\");", self.mod_name);

            fail_with_msg!(
                2,
                self.mod_name,
                source.line(),
                "{} is missing the required include for lisp_fn exports.",
                self.source_file(&self.mod_name)
            );
        }

        Ok(true)
    }

    // Determine source file name for a give module.
    fn source_file(&self, modname: &str) -> String {
        let base_path: PathBuf = [&self.mod_dir, modname].iter().collect();

        let mut file = base_path.clone();
        if base_path.is_dir() {
            file = base_path.join("mod.rs");
        } else {
            file.set_extension("rs");
        }

        if file.is_file() {
            path_as_str(file.file_name()).to_string()
        } else {
            modname.to_string()
        }
    }

    fn export(&mut self, out_file: &File) -> Result<bool, BuildError> {
        self.export_c(out_file)?;
        self.export_lisp_fns()
    }

    fn export_c(&mut self, mut out_file: &File) -> Result<bool, BuildError> {
        for elt in self.c_exports.iter() {
            write!(out_file, "pub use {}::{};\n", self.mod_name, elt)?;
        }

        Ok(false)
    }

    fn export_lisp_fns(&mut self) -> Result<bool, BuildError> {
        let base_path: PathBuf = [&self.mod_dir, &self.mod_name].iter().collect();

        let mod_path: String = if base_path.is_dir() {
            base_path.to_str().unwrap().to_string()
        } else {
            self.mod_dir.clone()
        };

        let path: PathBuf = [mod_path, [&self.mod_name, "_exports.rs"].concat()]
            .iter()
            .collect();

        if self.lisp_functions.is_empty() {
            if path.exists() {
                // There are no exported functions. Remove the export file.
                fs::remove_file(path)?;
            }
            return Ok(false);
        }

        let mut exports_file = File::create(path)?;

        let mut windows_header = "".to_string();
        if cfg!(windows) {
            windows_header =
                "| (::std::mem::size_of::<::remacs_sys::Lisp_Subr>()\n\
                 / ::std::mem::size_of::<::remacs_sys::EmacsInt>()) as ::libc::ptrdiff_t\
                 "
                    .to_string();
        }

        for &(ref lisp_fn_args, ref function) in self.lisp_functions.iter() {
            // The reason for the odd syntax of the Some arm is to
            // transform the Rust string into a raw C string
            let intspec = match lisp_fn_args.intspec.as_ref().map(String::as_str) {
                None => "::std::ptr::null()".to_string(),
                Some(intspec_value) => format!(
                    "(b\"{}\x00\").as_ptr() as *const ::libc::c_char",
                    intspec_value
                ),
            };

            let max_args = if lisp_fn_args.unevalled {
                FnArgsMax::UNEVALLED
            } else {
                match function.fntype {
                    function::LispFnType::Normal(_) => {
                        FnArgsMax::from(function.args.len() as isize)
                    }
                    function::LispFnType::Many => FnArgsMax::MANY,
                }
            };

            let mut cargs: Vec<String> = Vec::new();
            let mut rargs: Vec<String> = Vec::new();
            let mut body = "".to_string();
            match function.fntype {
                function::LispFnType::Normal(_) => for arg in function.args.iter() {
                    cargs.push(format!("{}: ::remacs_sys::Lisp_Object", arg));
                    rargs.push(format!("::lisp::LispObject::from_raw({}).into()", arg));
                },
                function::LispFnType::Many => {
                    cargs.push(
                        "nargs: ::libc::ptrdiff_t, args: *mut ::remacs_sys::Lisp_Object"
                            .to_string(),
                    );
                    body = "\
                            let args = unsafe {\n\
                            ::std::slice::from_raw_parts_mut::<::remacs_sys::Lisp_Object>(args,\n\
                            nargs as usize)\n\
                            };\n\
                            "
                        .to_string();
                    rargs.push("unsafe { ::std::mem::transmute(args) }".to_string());
                }
            }

            let code = format!(
                "\n\
#[no_mangle]\n\
pub extern \"C\" fn F{}({}) -> ::remacs_sys::Lisp_Object {{\n\
{}\n\
\tlet ret = {}({});\n\
\t::lisp::LispObject::from(ret).to_raw()\n\
}}\n\
\n\
lazy_static! {{\n\
\tpub static ref S{}: ::lisp::LispSubrRef = {{\n\
\t    let subr = ::remacs_sys::Lisp_Subr {{\n\
\t        header: ::remacs_sys::Lisp_Vectorlike_Header {{\n\
\t            size: ((::remacs_sys::PseudovecType::PVEC_SUBR as ::libc::ptrdiff_t)\n\
\t                << ::remacs_sys::PSEUDOVECTOR_AREA_BITS) {},\n\
\t        }},\n\
\t        function: self::F{} as *const ::libc::c_void,\n\
\t        min_args: {},\n\
\t        max_args: {},\n\
\t        symbol_name: (b\"{}\x00\").as_ptr() as *const ::libc::c_char,\n\
\t        intspec: {},\n\
\t        doc: ::std::ptr::null(),\n\
\t        lang: ::remacs_sys::Lisp_Subr_Lang_Rust,\n\
\t    }};\n\
\n\
\t    unsafe {{\n\
\t        let ptr =\n\
\t            ::remacs_sys::xmalloc(::std::mem::size_of::<::remacs_sys::Lisp_Subr>())\n\
\t            as *mut ::remacs_sys::Lisp_Subr;\n\
\t        ::std::ptr::copy_nonoverlapping(&subr, ptr, 1);\n\
\t        ::std::mem::forget(subr);\n\
\t        ::lisp::ExternalPtr::new(ptr)\n\
\t    }}\n\
\t}};\n\
}}\n",
                lisp_fn_args.c_name,
                cargs.join(", "),
                body,
                function.name,
                rargs.join(", "),
                lisp_fn_args.c_name,
                windows_header,
                lisp_fn_args.c_name,
                lisp_fn_args.min,
                max_args as isize,
                lisp_fn_args.name,
                intspec,
            );
            write!(exports_file, "{}", code.as_str())?;
        }

        write!(exports_file, "\n// exports\n")?;

        write!(
            exports_file,
            "export_lisp_fns! {{ {} }}",
            self.lisp_functions
                .iter()
                .map(|&(ref fnargs, _)| fnargs.c_name.clone())
                .collect::<Vec<String>>()
                .join(", "),
        )?;

        Ok(true)
    }
}

// What to ignore when walking the list of files
fn ignore(path: &str) -> bool {
    path == "" || path.starts_with('.') || path.ends_with("_exports.rs") || path == "lib.rs"
}

struct Exporter {
    src_dir: String,
}

impl Exporter {
    pub fn new(src_dir: String) -> Self {
        Self { src_dir }
    }

    pub fn run(&self) -> Result<(), BuildError> {
        let out_path: PathBuf = [&self.src_dir, "lib_exports.rs"].iter().collect();
        let mut out_file = File::create(out_path)?;

        let mut modules_with_lisp_fns: Vec<String> = Vec::new();

        for entry in fs::read_dir(&self.src_dir)? {
            let mod_path = entry?.path();

            if !ignore(path_as_str(mod_path.file_name())) {
                let mut mod_handler = ModuleExporter::new(self.src_dir.clone(), mod_path);
                if let Some(modname) = mod_handler.run(&out_file)? {
                    modules_with_lisp_fns.push(modname);
                }
            }
        }

        if !modules_with_lisp_fns.is_empty() {
            write!(
                out_file,
                "\n\
                 #[no_mangle]\n\
                 pub extern \"C\" fn rust_init_syms() {{\n\
                 {}\n\
                 }}\n\
                 ",
                modules_with_lisp_fns
                    .iter()
                    .map(|m| format!("{}::rust_init_syms();", m))
                    .collect::<Vec<String>>()
                    .join("\n    ")
            )?;
        }

        Ok(())
    }
}

fn main() {
    let exporter = Exporter::new([&env_var("CARGO_MANIFEST_DIR"), "/", "src"].concat());

    if let Err(e) = exporter.run() {
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
}
