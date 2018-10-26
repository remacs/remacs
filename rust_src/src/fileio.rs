//! Functions to deal with files
use std::path;

use remacs_macros::lisp_fn;

use lisp::defsubr;
use lists::LispCons;
use math::{arithcompare, ArithComparison};
use multibyte::LispStringRef;
use threads::ThreadState;

/// Return t if (car A) is numerically less than (car B).
#[lisp_fn]
pub fn car_less_than_car(a: LispCons, b: LispCons) -> bool {
    arithcompare(a.car(), b.car(), ArithComparison::Less)
}

def_lisp_sym!(Qcar_less_than_car, "car-less-than-car");

/// Return non-nil if NAME ends with a directory separator character.
#[lisp_fn]
pub fn directory_name_p(name: LispStringRef) -> bool {
    if name.len_bytes() == 0 {
        return false;
    }

    let b = name.byte_at(name.len_bytes() - 1);
    b as char == path::MAIN_SEPARATOR
}

/// Clear any record of a recent auto-save failure in the current buffer.
#[lisp_fn]
pub fn clear_buffer_auto_save_failure() {
    ThreadState::current_buffer().auto_save_failure_time = 0;
}

include!(concat!(env!("OUT_DIR"), "/fileio_exports.rs"));
