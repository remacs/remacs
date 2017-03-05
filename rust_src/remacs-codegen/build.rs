//! Small build script to ensure that we are building with the correct Rust
//! version.

extern crate version_check;

use std::io;
use std::io::Stderr;
use std::io::Write;
use std::process;

const MIN_VERSION: &'static str = "1.17.0";
const MIN_DATE: &'static str = "2017-02-18";

fn is_rustc_on_path(stderr: &mut Stderr) -> ! {
    write!(stderr,
           "cannot check rustc version, are you sure that `rustc` is on path?")
        .unwrap();
    process::exit(1)
}

fn main() {
    let mut stderr = io::stderr();

    match version_check::is_nightly() {
        Some(true) => (),
        Some(false) => {
            write!(&mut stderr,
                   "the compiler is not nightly, please check the project's `README.md`")
                .unwrap();
            process::exit(1);
        }
        None => is_rustc_on_path(&mut stderr),
    };

    match version_check::is_min_version(MIN_VERSION) {
        Some((true, _)) => (),
        Some((false, version)) => {
            write!(&mut stderr,
                   "Remacs needs at least the Rust version {}, but instead this was found \
                    {}.\nPlease upgrade.",
                   MIN_VERSION,
                   version)
                .unwrap();
            process::exit(1);
        }
        None => is_rustc_on_path(&mut stderr),
    }

    match version_check::is_min_date(MIN_DATE) {
        Some((true, _)) => (),
        Some((false, date)) => {
            write!(&mut stderr,
                   "Remacs needs at least the Rust compiler made on date {}, but instead this \
                    was found {}.\nPlease upgrade.",
                   MIN_DATE,
                   date)
                .unwrap();
        }
        None => is_rustc_on_path(&mut stderr),
    }
}
