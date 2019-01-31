//! Extract Rust docstrings from source files for Emacs' DOC file.

#![allow(clippy::cyclomatic_complexity)]

use libc::{c_char, c_int};
use regex::Regex;

use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{stdout, BufRead, BufReader, Write};
use std::mem;
use std::ptr;

use remacs_util::parse_lisp_fn;

#[allow(dead_code)]
const INVALID: c_int = 0;
#[allow(dead_code)]
const LISP_OBJECT: c_int = 1;
#[allow(dead_code)]
const EMACS_INTEGER: c_int = 2;
#[allow(dead_code)]
const BOOLEAN: c_int = 3;
#[allow(dead_code)]
const SYMBOL: c_int = 4;
#[allow(dead_code)]
const FUNCTION: c_int = 5;

type AddGlobalFn = fn(c_int, *const c_char, c_int, *const c_char) -> *const ();

#[no_mangle]
pub unsafe extern "C" fn scan_rust_file(
    filename: *const c_char,
    generate_globals: c_int,
    add_global: AddGlobalFn,
) {
    let filename = CStr::from_ptr(filename).to_str().unwrap();
    let fp = BufReader::new(File::open(&*filename).unwrap());

    let mut in_docstring = false;
    let mut docstring = String::new();
    let mut docstring_usage = String::new();
    let mut attribute = String::new();

    let mut line_iter = fp.lines();

    let mut in_lisp_fn = false;

    while let Some(line) = line_iter.next() {
        let line = line.unwrap();
        let line = line.trim();

        // Collect a whole docstring
        if line.starts_with("///") {
            if !in_docstring {
                attribute.clear();
                docstring_usage.clear();
                docstring.clear();
            }
            in_docstring = true;
            if line.starts_with("/// usage: (") {
                docstring = format!("{}\n", docstring.trim_end());
                let begin = &line[11..];
                // Now find the first space after the function name. If there is a space
                // capture the rest of the usage text.
                // The function name is dropped either way.
                if let Some(mut pos) = begin.find(' ') {
                    pos += 11;
                    docstring_usage.push_str(&line[pos..]);
                }
            } else {
                docstring.push_str(line[3..].trim_start());
                docstring.push('\n');
            }
        } else {
            in_docstring = false;
        }

        if line == "#[lisp_fn(" {
            attribute = line.to_owned();
            in_lisp_fn = true;
            continue;
        } else if line.starts_with("#[lisp_fn") {
            attribute = line.to_owned();
            continue;
        }

        if in_lisp_fn {
            if line == ")]" {
                attribute += line;
                in_lisp_fn = false;
                continue;
            } else {
                attribute += line;
                continue;
            }
        }

        if line.starts_with("pub fn ") || line.starts_with("fn ") {
            if attribute.is_empty() {
                // Not a #[lisp_fn]
                continue;
            }
            let attribute = mem::replace(&mut attribute, String::new());
            let mut split = line.split('(');
            let name = split.next().unwrap().split_whitespace().last().unwrap();

            if name.starts_with('$') {
                // Macro; do not use it
                continue;
            }

            // Read lines until the closing paren
            let mut sig = split.next().unwrap().to_string();
            while !sig.contains(')') {
                sig.extend(line_iter.next().unwrap());
            }
            let sig = sig.split(')').next().unwrap();
            let has_many_args = sig.contains("&mut") || sig.contains("&[");

            // Split arg names and types
            let splitters = [':', ','];
            let args = sig.split_terminator(&splitters[..]).collect::<Vec<_>>();

            let nargs = args.len() / 2;
            let def_min_args = if has_many_args { 0 } else { nargs as i16 };
            let attr_props = parse_lisp_fn(&attribute, name, def_min_args)
                .unwrap_or_else(|e| panic!("Invalid #[lisp_fn] macro ({}): {}", attribute, e));

            if generate_globals != 0 {
                let c_name_str = CString::new(format!("F{}", attr_props.c_name)).unwrap();
                // -1 is MANY
                // -2 is UNEVALLED
                let maxargs = if has_many_args { -1 } else { nargs as c_int };
                add_global(FUNCTION, c_name_str.as_ptr(), maxargs, ptr::null());
            } else {
                // Create usage line (fn ARG1 ...) from signature if necessary
                if docstring_usage.is_empty() {
                    for (i, chunk) in args.chunks(2).enumerate() {
                        if chunk[1].contains("&mut") || chunk[1].contains("&[") {
                            docstring_usage.push(' ');
                            docstring_usage.push_str("&rest");
                        } else if i == attr_props.min as usize {
                            docstring_usage.push(' ');
                            docstring_usage.push_str("&optional");
                        }
                        let argname = chunk[0]
                            .trim()
                            .trim_start_matches("mut ")
                            .trim()
                            .to_uppercase()
                            .replace("_", "-");
                        docstring_usage.push(' ');
                        docstring_usage.push_str(&argname);
                    }
                    docstring_usage.push(')');
                }
                // Print contents for docfile to stdout
                print!(
                    "\x1fF{}\n{}\n(fn{}",
                    attr_props.name, docstring, docstring_usage
                );
            }
        } else if line.starts_with("def_lisp_sym!(") {
            lazy_static! {
                static ref RE: Regex = Regex::new(r#"def_lisp_sym!\((.+?),\s+"(.+?)"\);"#).unwrap();
            }
            let caps = RE.captures(line).unwrap();
            let name = CString::new(&caps[1]).unwrap();
            let value = CString::new(&caps[2]).unwrap();
            add_global(SYMBOL, name.as_ptr(), 0, value.as_ptr());
        } else if line.starts_with("defvar_") {
            // defvar_lisp!(f_Vpost_self_insert_hook, "post-self-insert-hook", Qnil);
            // defvar_kboard!(Vlast_command_, "last-command");
            lazy_static! {
                static ref RE: Regex =
                    Regex::new(r#"defvar_(.+?)!\((.+?),\s+"(.+?)"(?:,\s+(.+?))?\);"#).unwrap();
            }
            for caps in RE.captures_iter(line) {
                if generate_globals != 0 {
                    let kindstr = &caps[1];
                    let kind = match kindstr {
                        "lisp" => LISP_OBJECT,
                        "lisp_nopro" => LISP_OBJECT,
                        "bool" => BOOLEAN,
                        "int" => EMACS_INTEGER,
                        "per_buffer" => INVALID, // not really invalid, we just skip them here
                        "kboard" => INVALID,
                        _ => panic!("unknown macro 'defvar_{}' found; either you have a typo in '{}' or you need to update docfile.rs`",
                                    kindstr, filename),
                    };
                    if kind != INVALID {
                        let field_name = &caps[2];
                        assert!(!field_name.starts_with("f_"));
                        let field_name = CString::new(&caps[2]).unwrap();
                        add_global(kind, field_name.as_ptr(), 0, ptr::null());
                    }
                } else {
                    let lisp_name = &caps[3];
                    print!("\x1fV{}\n{}", lisp_name, docstring)
                }
            }
        }
    }
    stdout().flush().unwrap();
}
