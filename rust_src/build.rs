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

fn ignore(path: &str) -> bool {
    path == "" || path.starts_with('.') || path == "lib.rs"
}

fn path_as_str(path: Option<&OsStr>) -> &str {
    path.and_then(|p| p.to_str())
        .unwrap_or_else(|| panic!(format!("Cannot understand string: {:?}", path)))
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

fn env_var(name: &str) -> String {
    match env::var(name) {
        Ok(value) => value,
        Err(e) => panic!(format!("Could not find {} in environment: {}", name, e)),
    }
}

fn source_file(modname: &str) -> String {
    let base_path: PathBuf = [&env_var("CARGO_MANIFEST_DIR"), "src", modname]
        .iter()
        .collect();

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
        let path: PathBuf = [env_var("OUT_DIR"), [modname, "_exports.rs"].concat()]
            .iter()
            .collect();
        let mut exports_file = File::create(path)?;

        write!(
            exports_file,
            "export_lisp_fns! {{ {} }}",
            exported.join(", ")
        )?;

        if !has_include {
            panic!(format!(
                "{} is missing the required include for lisp_fn exports",
                source_file(modname)
            ));
        }

        Ok(true)
    }
}

fn handle_file(mut mod_path: PathBuf, out_file: &File) -> Result<Option<String>, io::Error> {
    let mut name: Option<String> = None;

    // in order to parse correctly, determine where the code lives
    // for submodules that will be in a mod.rs file.
    if mod_path.is_dir() {
        let tmp = path_as_str(mod_path.file_name()).to_string();
        mod_path = mod_path.join("mod.rs");
        if mod_path.is_file() {
            name = Some(tmp);
        }
    } else if let Some(ext) = mod_path.extension() {
        if ext == "rs" {
            name = Some(path_as_str(mod_path.file_stem()).to_string());
        }
    }

    // only handle Rust files
    if let Some(modname) = name {
        let fp = match File::open(&mod_path) {
            Ok(f) => f,
            Err(e) => {
                return Err(io::Error::new(
                    e.kind(),
                    format!("Failed to open {:?}: {}", mod_path, e),
                ))
            }
        };

        if c_exports_for_module(&modname, out_file, BufReader::new(fp))? {
            return Ok(Some(modname));
        }
    }

    Ok(None)
}

fn generate_c_exports() -> Result<(), io::Error> {
    let out_path: PathBuf = [&env_var("OUT_DIR"), "c_exports.rs"].iter().collect();
    let mut out_file = File::create(out_path)?;

    let mut modules: Vec<String> = Vec::new();

    let in_path: PathBuf = [&env_var("CARGO_MANIFEST_DIR"), "src"].iter().collect();
    for entry in fs::read_dir(in_path)? {
        let mod_path = entry?.path();

        if !ignore(path_as_str(mod_path.file_name())) {
            if let Some(modname) = handle_file(mod_path, &out_file)? {
                modules.push(modname);
            }
        }
    }

    if !modules.is_empty() {
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
    }

    Ok(())
}

fn main() {
    if let Err(e) = generate_c_exports() {
        panic!(format!("Errors occurred:\n{}", e));
    }
}
