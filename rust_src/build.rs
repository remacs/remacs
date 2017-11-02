use std::env;
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
    ReadingFnNames,
    Complete,
}

fn c_exports_for_module<R>(
    modname: &str,
    mut out_file: &File,
    in_file: R,
) -> Result<bool, io::Error>
where
    R: BufRead,
{
    let mut parse_state = ParseState::Looking;
    let mut has_lisp_fns = false;

    for line in in_file.lines() {
        let line = line?;
        let line = line.trim();

        match parse_state {
            ParseState::Looking => if line.starts_with("export_lisp_fns!") {
                parse_state = ParseState::ReadingFnNames;
                has_lisp_fns = true;
            } else if line.starts_with("#[no_mangle]") {
                parse_state = ParseState::NoMangleFn;
            } else if line.starts_with("/*") {
                if !line.ends_with("*/") {
                    parse_state = ParseState::IgnoreComments;
                }
            },

            ParseState::IgnoreComments => if line.starts_with("*/") || line.ends_with("*/") {
                parse_state = ParseState::Looking;
            },

            // export public #[no_mangle] functions
            ParseState::NoMangleFn => {
                if line.starts_with("pub") {
                    if let Some(pos) = line.find('(') {
                        // have to look for fn to support:
                        // pub fn foo
                        // pub extern "C" fn foo
                        //
                        if let Some(fnpos) = line.find("fn ") {
                            let func = &line[(fnpos + 3)..pos].to_string();
                            write!(out_file, "pub use {}::{};\n", modname, func.trim())?;
                        }
                    }
                }
                parse_state = ParseState::Looking;
            }

            ParseState::ReadingFnNames => if line.starts_with("}") {
                parse_state = ParseState::Complete;
            } else {
                let mut parts = line.trim().split(',');
                let func = parts.next().unwrap();
                write!(out_file, "pub use {}::F{};\n", modname, func)?;
            },

            ParseState::Complete => {
                break;
            }
        };
    }

    Ok(has_lisp_fns)
}

fn ignore(path: &str) -> bool {
    path == "" || path.starts_with('.') || path == "lib.rs"
}

fn generate_c_exports() -> Result<(), io::Error> {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("c_exports.rs");
    let mut out_file = File::create(out_path)?;

    let mut modules: Vec<String> = Vec::new();

    let in_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("src");
    for entry in fs::read_dir(in_path)? {
        let entry = entry?;
        let mut mod_path = entry.path();

        if ignore(mod_path.file_name().unwrap().to_str().unwrap()) {
            continue;
        }

        let mut name: Option<String> = None;

        if mod_path.is_dir() {
            name = Some(
                mod_path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .map_or_else(|| panic!("Cannot understand string"), |s| s.to_string()),
            );
            mod_path = mod_path.join("mod.rs");
        } else if let Some(ext) = mod_path.extension() {
            if ext == "rs" {
                name = Some(
                    mod_path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .map_or_else(|| panic!("Cannot understand string"), |s| s.to_string()),
                );
            }
        }

        if let Some(modname) = name {
            let path = mod_path
                .to_str()
                .map_or_else(|| panic!("Cannot understand string"), |s| s.to_string());

            let fp = match File::open(PathBuf::from(&path)) {
                Ok(f) => f,

                Err(e) => {
                    eprintln!("Failed to open {}", path);
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
    write!(out_file, "}}\n")?;

    Ok(())
}

fn main() {
    if let Err(e) = generate_c_exports() {
        eprintln!("Errors occurred: {}", e);
    }
}
