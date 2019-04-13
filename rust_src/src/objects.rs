//! Various functions operating on any object.

use remacs_macros::lisp_fn;

use crate::{hashtable::LispHashTableRef, lisp::LispObject, remacs_sys::equal_kind};

/// Return t if OBJECT is nil, and return nil otherwise.
#[lisp_fn]
pub fn null(object: LispObject) -> bool {
    object.is_nil()
}

/// Return t if the two args are the same Lisp object.
#[lisp_fn]
pub fn eq(obj1: LispObject, obj2: LispObject) -> bool {
    obj1.eq(obj2)
}

/// Return t if the two args are the same Lisp object.
/// Floating-point numbers of equal value are `eql', but they may not be `eq'.
#[lisp_fn]
pub fn eql(obj1: LispObject, obj2: LispObject) -> bool {
    obj1.eql(obj2)
}

/// Return t if two Lisp objects have similar structure and contents.
/// They must have the same data type.
/// Conses are compared by comparing the cars and the cdrs.
/// Vectors and strings are compared element by element.
/// Numbers are compared by value, but integers cannot equal floats.
///  (Use `=' if you want integers and floats to be able to be equal.)
/// Symbols must match exactly.
#[lisp_fn]
pub fn equal(o1: LispObject, o2: LispObject) -> bool {
    o1.equal(o2)
}

/// Return t if two Lisp objects have similar structure and contents.
/// This is like `equal' except that it compares the text properties
/// of strings.  (`equal' ignores text properties.)
#[lisp_fn]
pub fn equal_including_properties(o1: LispObject, o2: LispObject) -> bool {
    let mut ht = LispHashTableRef::empty();
    o1.equal_internal(o2, equal_kind::EQUAL_INCLUDING_PROPERTIES, 0, &mut ht)
}

/// Return the argument unchanged.
#[lisp_fn]
pub fn identity(arg: LispObject) -> LispObject {
    arg
}

include!(concat!(env!("OUT_DIR"), "/objects_exports.rs"));
