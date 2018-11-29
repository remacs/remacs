//! Storage allocation and gc

use remacs_macros::lisp_fn;

use crate::{
    lisp::{defsubr, LispObject},
    remacs_sys::globals,
    remacs_sys::EmacsInt,
    remacs_sys::{bool_vector_fill, bool_vector_set, bounded_number, make_uninit_bool_vector},
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

/// Return a new bool-vector of length LENGTH, using INIT for each element.
/// LENGTH must be a number.  INIT matters only in whether it is t or nil.
#[lisp_fn]
pub fn make_bool_vector(length: EmacsInt, init: bool) -> LispObject {
    unsafe { bool_vector_fill(make_uninit_bool_vector(length), init.into()) }
}

/// Return a new bool-vector with specified arguments as elements.
/// Any number of arguments, even zero arguments, are allowed.
/// usage: (bool-vector &rest OBJECTS)
#[lisp_fn]
pub fn bool_vector(args: &mut [LispObject]) -> LispObject {
    let vector = unsafe { make_uninit_bool_vector(args.len() as EmacsInt) };

    for (i, arg) in args.iter().enumerate() {
        unsafe { bool_vector_set(vector, i as EmacsInt, arg.is_not_nil()) }
    }

    vector
}

include!(concat!(env!("OUT_DIR"), "/alloc_exports.rs"));
