//! Functions to deal with files
use std::path;

use remacs_macros::lisp_fn;

use crate::{
    lisp::defsubr,
    lists::LispCons,
    math::{arithcompare, ArithComparison},
    multibyte::LispStringRef,
    threads::ThreadState,
};

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

/// Return t if current buffer has been auto-saved recently.
/// More precisely, if it has been auto-saved since last read from or saved
/// in the visited file.  If the buffer has no visited file,
/// then any auto-save counts as "recent".
#[lisp_fn]
pub fn recent_auto_save_p() -> bool {
    let cur_buf = ThreadState::current_buffer();

    // FIXME: maybe we should return nil for indirect buffers since
    //  they're never autosaved.
    cur_buf.modifications_since_save() < cur_buf.auto_save_modified
}

include!(concat!(env!("OUT_DIR"), "/fileio_exports.rs"));
