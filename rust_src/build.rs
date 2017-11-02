use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;

#[derive(Eq, PartialEq)]
enum ParseState {
    Looking,
    IgnoreComments,
    NoMangleFn,
    LispFn(Option<String>),
}

fn get_function_name(line: &str) -> Option<String> {
    if let Some(pos) = line.find('(') {
        if let Some(fnpos) = line.find("fn ") {
            let name = line[(fnpos + 3)..pos].trim();
            return Some(name.to_string());
        }
    }

    None
}

static C_NAME: &str = "c_name = \"";

fn c_exports_for_module<R>(
    modname: &str,
    mut out_file: &File,
    in_file: R,
) -> Result<bool, io::Error>
where
    R: BufRead,
{
    let mut parse_state = ParseState::Looking;
    let mut exported: Vec<String> = Vec::new();
    let mut has_include = false;

    for line in in_file.lines() {
        let line = line?;

        match parse_state {
            ParseState::Looking => {
                if line.starts_with("#[lisp_fn") {
                    if let Some(begin) = line.find(C_NAME) {
                        let start = begin + C_NAME.len();
                        let end = line[start..].find('"').unwrap() + start;
                        let name = line[start..end].to_string();
                        // Ignore macros, nothing we can do with them
                        if !name.starts_with('$') {
                            // even though we do not need to parse the
                            // next line of the file this keeps all of the
                            // lisp_fn code in one place.
                            parse_state = ParseState::LispFn(Some(name));
                        }
                    } else {
                        parse_state = ParseState::LispFn(None);
                    }
                } else if line.starts_with("#[no_mangle]") {
                    parse_state = ParseState::NoMangleFn;
                } else if line.starts_with("/*") {
                    if !line.ends_with("*/") {
                        parse_state = ParseState::IgnoreComments;
                    }
                } else if line.starts_with("include!(concat!(env!(\"OUT_DIR\"),") {
                    has_include = true;
                }
            }

            ParseState::IgnoreComments => if line.starts_with("*/") || line.ends_with("*/") {
                parse_state = ParseState::Looking;
            },

            ParseState::LispFn(name) => {
                if line.starts_with("pub") || line.starts_with("fn") {
                    if let Some(func) = name.or_else(|| get_function_name(&line)) {
                        write!(out_file, "pub use {}::F{};\n", modname, func)?;
                        exported.push(func);
                    }
                }

                parse_state = ParseState::Looking;
            }

            // export public #[no_mangle] functions
            ParseState::NoMangleFn => {
                if line.starts_with("pub") {
                    if let Some(func) = get_function_name(&line) {
                        write!(out_file, "pub use {}::{};\n", modname, func)?;
                    }
                }

                parse_state = ParseState::Looking;
            }
        };
    }

    if exported.is_empty() {
        Ok(false)
    } else {
        let path =
            PathBuf::from(env::var("OUT_DIR").unwrap()).join([modname, "_exports.rs"].concat());
        let mut exports_file = File::create(path)?;

        write!(
            exports_file,
            "export_lisp_fns! {{ {} }}",
            exported.join(", ")
        )?;

        if !has_include {
            panic!(format!(
                "{}.rs is missing the required include for lisp_fn exports",
                modname
            ));
        }

        Ok(true)
    }
}

fn ignore(path: &str) -> bool {
    path == "" || path.starts_with('.') || path == "lib.rs"
}

fn path_as_str(path: Option<&OsStr>) -> &str {
    path.and_then(|p| p.to_str())
        .unwrap_or_else(|| panic!(format!("Cannot understand string: {:?}", path)))
}

fn generate_c_exports() -> Result<(), io::Error> {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("c_exports.rs");
    let mut out_file = File::create(out_path)?;

    let mut modules: Vec<String> = Vec::new();

    let in_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("src");
    for entry in fs::read_dir(in_path)? {
        let entry = entry?;
        let mut mod_path = entry.path();

        if ignore(path_as_str(mod_path.file_name())) {
            continue;
        }

        let mut name: Option<String> = None;

        if mod_path.is_dir() {
            name = Some(path_as_str(mod_path.file_name()).to_string());
            mod_path = mod_path.join("mod.rs");
        } else if let Some(ext) = mod_path.extension() {
            if ext == "rs" {
                name = Some(path_as_str(mod_path.file_stem()).to_string());
            }
        }

        if let Some(modname) = name {
            let fp = match File::open(&mod_path) {
                Ok(f) => f,

                Err(e) => {
                    eprintln!("Failed to open {:?}", mod_path);
                    return Err(e);
                }
            };

            if c_exports_for_module(&modname, &out_file, BufReader::new(fp))? {
                modules.push(modname);
            }
        }
    }

    write!(out_file, "\n")?;

    write!(
        out_file,
        "#[no_mangle]\npub extern \"C\" fn rust_init_syms() {{\n"
    )?;
    for module in modules {
        write!(out_file, "    {}::rust_init_syms();\n", module)?;
    }
    // Add this one by hand.
    write!(out_file, "    floatfns::rust_init_extra_syms();\n")?;
    write!(out_file, "}}\n")?;

    Ok(())
}

fn main() {
    if let Err(e) = generate_c_exports() {
        eprintln!("Errors occurred: {}", e);
    }
}
