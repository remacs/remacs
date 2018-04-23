//! Functions to deal with files

use lisp::defsubr;
use multibyte::LispStringRef;
use remacs_macros::lisp_fn;
use std::path;

/// Return non-nil if NAME ends with a directory separator character.
#[lisp_fn]
pub fn directory_name_p(name: LispStringRef) -> bool {
    if name.len_bytes() > 0 {
        let b = name.byte_at(name.len_bytes() - 1);
        return b as char == path::MAIN_SEPARATOR;
    }
    false
}

include!(concat!(env!("OUT_DIR"), "/fileio_exports.rs"));
