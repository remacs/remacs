//! Various functions operating on any object.

use remacs_macros::lisp_fn;
use remacs_sys::{EqualKind, Qnil};
use remacs_sys::internal_equal;

use lisp::LispObject;
use lisp::defsubr;

/// Return t if OBJECT is nil, and return nil otherwise.
#[lisp_fn]
fn null(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_nil())
}

/// Return t if the two args are the same Lisp object.
#[lisp_fn]
fn eq(obj1: LispObject, obj2: LispObject) -> LispObject {
    LispObject::from_bool(obj1.eq(obj2))
}

/// Return t if the two args are the same Lisp object.
/// Floating-point numbers of equal value are `eql', but they may not be `eq'.
#[lisp_fn]
fn eql(obj1: LispObject, obj2: LispObject) -> LispObject {
    LispObject::from_bool(obj1.eql(obj2))
}

/// Return t if two Lisp objects have similar structure and contents.
/// They must have the same data type.
/// Conses are compared by comparing the cars and the cdrs.
/// Vectors and strings are compared element by element.
/// Numbers are compared by value, but integers cannot equal floats.
///  (Use `=' if you want integers and floats to be able to be equal.)
/// Symbols must match exactly.
#[lisp_fn]
fn equal(o1: LispObject, o2: LispObject) -> LispObject {
    LispObject::from_bool(o1.equal(o2))
}

/// Return t if two Lisp objects have similar structure and contents.
/// This is like `equal' except that it compares the text properties
/// of strings.  (`equal' ignores text properties.)
#[lisp_fn]
fn equal_including_properties(o1: LispObject, o2: LispObject) -> LispObject {
    let res = unsafe {
        internal_equal(
            o1.to_raw(),
            o2.to_raw(),
            EqualKind::IncludingProperties,
            0,
            Qnil,
        )
    };
    LispObject::from_bool(res)
}

/// Return the argument unchanged.
#[lisp_fn]
fn identity(arg: LispObject) -> LispObject {
    arg
}

pub fn rust_init_syms() {
    unsafe {
        defsubr(&*Seq);
        defsubr(&*Seql);
        defsubr(&*Sequal);
        defsubr(&*Sequal_including_properties);
        defsubr(&*Sidentity);
        defsubr(&*Snull);
    }
}
