//! Storage allocation and gc

use remacs_macros::lisp_fn;

use crate::{
    chartable::LispCharTableRef,
    lisp::{ExternalPtr, LispObject},
    lists::get,
    remacs_sys::globals,
    remacs_sys::{
        allocate_record, allocate_vector, bool_vector_fill, bool_vector_set, bounded_number,
        make_uninit_bool_vector,
    },
    remacs_sys::{char_table_specials::CHAR_TABLE_STANDARD_SLOTS, EmacsInt, EmacsUint},
    remacs_sys::{pvec_type, Lisp_Type::Lisp_Vectorlike},
    remacs_sys::{Qchar_table_extra_slots, Qnil},
    symbols::LispSymbolRef,
    vectors::LispVectorRef,
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

/// Return a newly created vector of length LENGTH, with each element being INIT.
/// See also the function `vector'.
#[lisp_fn]
pub fn make_vector(length: EmacsUint, init: LispObject) -> LispVectorRef {
    let mut p = LispVectorRef::new(unsafe { allocate_vector(length as EmacsInt) });

    for i in 0..length {
        unsafe { p.set_unchecked(i as usize, init) };
    }

    p
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

/// Return a newly created char-table, with purpose PURPOSE.
/// Each element is initialized to INIT, which defaults to nil.
///
/// PURPOSE should be a symbol.  If it has a `char-table-extra-slots'
/// property, the property's value should be an integer between 0 and 10
/// that specifies how many extra slots the char-table has.  Otherwise,
/// the char-table has no extra slot.
#[lisp_fn(min = "1")]
pub fn make_char_table(purpose: LispSymbolRef, init: LispObject) -> LispCharTableRef {
    let n = get(purpose, Qchar_table_extra_slots);
    let n_extras: EmacsUint = match n.into() {
        None => 0,
        Some(x) => {
            if x > 10 {
                args_out_of_range!(n, Qnil);
            }
            x
        }
    };

    let size = EmacsUint::from(CHAR_TABLE_STANDARD_SLOTS) + n_extras;
    let vector = make_vector(size, init);
    let mut char_table = LispCharTableRef::from_vector(vector);
    char_table.parent = Qnil;
    char_table.purpose = purpose.into();
    set_vector_type!(char_table, pvec_type::PVEC_CHAR_TABLE);

    char_table
}

/// Create a new record.
/// TYPE is its type as returned by `type-of'; it should be either a
/// symbol or a type descriptor.  SLOTS is the number of non-type slots,
/// each initialized to INIT.
#[lisp_fn]
pub fn make_record(r#type: LispObject, slots: EmacsUint, init: LispObject) -> LispObject {
    let size = slots + 1;
    unsafe {
        let ptr = allocate_record(size as i64);
        let contents = (*ptr).contents.as_mut_slice(size as usize);
        contents[0] = r#type;
        for rec in contents.iter_mut().skip(1) {
            *rec = init;
        }
        LispObject::tag_ptr(ExternalPtr::new(ptr), Lisp_Vectorlike)
    }
}

/// Create a new record.
/// TYPE is its type as returned by `type-of'; it should be either a
/// symbol or a type descriptor.  SLOTS is used to initialize the record
/// slots with shallow copies of the arguments.
/// usage: (record TYPE &rest SLOTS)
#[lisp_fn(min = "1")]
pub fn record(args: &mut [LispObject]) -> LispObject {
    unsafe {
        let ptr = allocate_record(args.len() as i64);
        (*ptr)
            .contents
            .as_mut_slice(args.len())
            .copy_from_slice(args);
        LispObject::tag_ptr(ExternalPtr::new(ptr), Lisp_Vectorlike)
    }
}

include!(concat!(env!("OUT_DIR"), "/alloc_exports.rs"));
