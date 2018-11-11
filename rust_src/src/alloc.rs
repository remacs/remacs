//! Storage allocation and gc

use remacs_macros::lisp_fn;

use crate::{
    lisp::{defsubr, LispObject},
    remacs_sys::bounded_number,
    remacs_sys::globals,
};

/// Return a list of counters that measure how much consing there has been.
/// Each of these counters increments for a certain kind of object.
/// The counters wrap around from the largest positive integer to zero.
/// Garbage collection does not decrease them.
/// The elements of the value are as follows:
///   (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS MISCS INTERVALS STRINGS)
/// All are in units of 1 = one object consed except for VECTOR-CELLS
/// and STRING-CHARS, which count the total length of objects consed.
/// MISCS include overlays, markers, and some internal types.
/// Frames, windows, buffers, and subprocesses count as vectors
///   (but the contents of a buffer's text do not count here).
#[lisp_fn]
pub fn memory_use_counts() -> Vec<LispObject> {
    unsafe {
        vec![
            bounded_number(globals.cons_cells_consed),
            bounded_number(globals.floats_consed),
            bounded_number(globals.vector_cells_consed),
            bounded_number(globals.symbols_consed),
            bounded_number(globals.string_chars_consed),
            bounded_number(globals.misc_objects_consed),
            bounded_number(globals.intervals_consed),
            bounded_number(globals.strings_consed),
        ]
    }
}

include!(concat!(env!("OUT_DIR"), "/alloc_exports.rs"));
