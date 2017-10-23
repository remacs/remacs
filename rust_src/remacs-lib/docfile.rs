//! Extract Rust docstrings from source files for Emacs' DOC file.
#![allow(dead_code)]

use libc::{c_char, c_int};

use std::collections::HashMap;
use std::fs::File;
use std::io::{stdout, BufRead, BufReader, Write};
use std::ffi::{CStr, CString};
use std::mem;
use std::ptr;

const LISP_OBJECT: c_int = 1;
const EMACS_INTEGER: c_int = 2;
const BOOLEAN: c_int = 3;
const SYMBOL: c_int = 4;
const FUNCTION: c_int = 5;

type AddGlobalFn = fn(c_int, *const c_char, c_int, *const c_char) -> *const ();


#[no_mangle]
pub extern "C" fn scan_rust_file(
    filename: *const c_char,
    generate_globals: c_int,
    add_global: AddGlobalFn,
) {
    let filename = unsafe { CStr::from_ptr(filename) }.to_str().unwrap();
    let fp = BufReader::new(File::open(&*filename).unwrap());

    let mut in_docstring = false;
    let mut docstring = String::new();
    let mut docstring_usage = String::new();
    let mut attribute = String::new();

    let mut line_iter = fp.lines();

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
                docstring_usage.push_str(&line[11..]);
            } else {
                docstring.push_str(line[3..].trim_left());
                docstring.push('\n');
            }
        } else {
            in_docstring = false;
        }

        if line.starts_with("#[lisp_fn") {
            attribute = line.to_owned();
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

            // Parse attribute properties to get minimum # of arguments,
            // and Lisp function name.
            let mut attr_props = HashMap::new();
            if attribute.contains('(') {
                let splitters = ['(', ')'];
                // Extract part between parens
                let attr = attribute.split(&splitters[..]).nth(1).unwrap();
                // Parse key-value pairs
                for arg in attr.split_terminator(',') {
                    let mut name_val = arg.split('=');
                    attr_props.insert(
                        name_val.next().unwrap().trim(),
                        name_val.next().unwrap().trim().trim_matches('"'),
                    );
                }
            }

            // Read lines until the closing paren
            let mut sig = split.next().unwrap().to_string();
            while !sig.contains(')') {
                sig.extend(line_iter.next().unwrap());
            }
            let sig = sig.split(')').next().unwrap();
            let has_many_args = sig.contains("&mut");
            // Split arg names and types
            let splitters = [':', ','];
            let args = sig.split_terminator(&splitters[..]).collect::<Vec<_>>();
            let lisp_name = attr_props
                .get("name")
                .map_or_else(|| name.replace("_", "-"), |&name| name.into());
            let c_name = format!("F{}", attr_props.get("c_name").unwrap_or(&name));

            let nargs = args.len() / 2;
            let minargs = attr_props.get("min").map_or(nargs, |m| m.parse().unwrap());

            if generate_globals != 0 {
                let c_name_str = CString::new(c_name).unwrap();
                // -1 is MANY
                // -2 is UNEVALLED
                let maxargs = if has_many_args { -1 } else { nargs as c_int };
                add_global(FUNCTION, c_name_str.as_ptr(), maxargs, ptr::null());
            } else {
                // Create usage line (fn ARG1 ...) from signature if necessary
                if docstring_usage.is_empty() {
                    docstring_usage.push_str("(fn ");
                    for (i, chunk) in args.chunks(2).enumerate() {
                        if i == minargs {
                            docstring_usage.push_str("&optional ");
                        }
                        if chunk[1].contains("&mut") {
                            docstring_usage.push_str("&rest ");
                        }
                        let argname = chunk[0]
                            .trim()
                            .trim_left_matches("mut ")
                            .trim()
                            .to_uppercase()
                            .replace("_", "-");
                        docstring_usage.push_str(&argname);
                        docstring_usage.push(' ');
                    }
                    docstring_usage.pop();
                    docstring_usage.push(')');
                }
                // Print contents for docfile to stdout
                print!(
                    "\x1f{}{}\n{}\n{}",
                    "F",
                    lisp_name,
                    docstring,
                    docstring_usage
                );
            }
        }
    }
    stdout().flush().unwrap();
}
