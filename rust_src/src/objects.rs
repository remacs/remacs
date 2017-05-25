/// Various functions operating on any object.

use std::ptr;

use lisp::{LispObject, Qnil};
use remacs_sys::internal_equal;


fn null(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_nil())
}

defun!("null",
       Fnull(object),
       Snull,
       null,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is nil, and return nil otherwise.

(fn OBJECT)");

fn eq(obj1: LispObject, obj2: LispObject) -> LispObject {
    LispObject::from_bool(obj1.eq(obj2))
}

defun!("eq",
       Feq(obj1, obj2),
       Seq,
       eq,
       2,
       2,
       ptr::null(),
       "Return t if the two args are the same Lisp object.

(fn OBJ1 OBJ2)");

fn eql(obj1: LispObject, obj2: LispObject) -> LispObject {
    LispObject::from_bool(obj1.eql(obj2))
}

defun!("eql",
       Feql(obj1, obj2),
       Seql,
       eql,
       2,
       2,
       ptr::null(),
       "Return t if the two args are the same Lisp object.
Floating-point numbers of equal value are `eql', but they may not be `eq'.

(fn OBJ1 OBJ2)
");

fn equal(o1: LispObject, o2: LispObject) -> LispObject {
    LispObject::from_bool(o1.equal(o2))
}

defun!("equal",
       Fequal(o1, o2),
       Sequal,
       equal,
       2,
       2,
       ptr::null(),
       "Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
Conses are compared by comparing the cars and the cdrs.
Vectors and strings are compared element by element.
Numbers are compared by value, but integers cannot equal floats.
 (Use `=' if you want integers and floats to be able to be equal.)
Symbols must match exactly.

(fn O1 O2)
");

fn equal_including_properties(o1: LispObject, o2: LispObject) -> LispObject {
    let res = unsafe { internal_equal(o1.to_raw(), o2.to_raw(), 0, true, Qnil.to_raw()) };
    LispObject::from_bool(res)
}

defun!("equal-including-properties",
       Fequal_including_properties(o1, o2),
       Sequal_including_properties,
       equal_including_properties,
       2,
       2,
       ptr::null(),
       "Return t if two Lisp objects have similar structure and contents.
This is like `equal' except that it compares the text properties
of strings.  (`equal' ignores text properties.)

(fn O1 O2)
");
