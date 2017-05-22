use std::mem;

use libc;

use lisp::{CHECK_TYPE, LispObject, LispType, Qnil, XTYPE, XUNTAG};
use remacs_sys::{Lisp_Object, wrong_type_argument};
use remacs_macros::lisp_fn;

extern "C" {
    static Qconsp: Lisp_Object;
    fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const libc::c_void);
    static Qlistp: Lisp_Object;
}


pub fn CONSP(x: LispObject) -> bool {
    XTYPE(x) == LispType::Lisp_Cons
}

/// Return t if OBJECT is not a cons cell.  This includes nil.
#[lisp_fn(name = "atom", min = "1")]
fn atom(object: LispObject) -> LispObject {
    if CONSP(object) {
        Qnil
    } else {
        LispObject::constant_t()
    }
}

/// Return t if OBJECT is a cons cell.
/// (fn OBJECT)
#[lisp_fn(name = "consp", min = "1")]
fn consp(object: LispObject) -> LispObject {
    if CONSP(object) {
        LispObject::constant_t()
    } else {
        Qnil
    }
}

/// Represents a cons cell, or GC bookkeeping for cons cells.
///
/// A cons cell is pair of two pointers, used to build linked lists in
/// lisp.
///
/// # C Porting Notes
///
/// The equivalent C struct is `Lisp_Cons`. Note that the second field
/// may be used as the cdr or GC bookkeeping.
// TODO: this should be aligned to 8 bytes.
#[repr(C)]
#[allow(unused_variables)]
struct LispCons {
    /// Car of this cons cell.
    car: LispObject,
    /// Cdr of this cons cell, or the chain used for the free list.
    cdr: LispObject,
}

// alloc.c uses a union for `Lisp_Cons`, which we emulate with an
// opaque struct.
#[repr(C)]
#[allow(dead_code)]
pub struct LispConsChain {
    chain: *const LispCons,
}

/// Extract the LispCons data from an elisp value.
fn XCONS(a: LispObject) -> *mut LispCons {
    debug_assert!(CONSP(a));
    unsafe { mem::transmute(XUNTAG(a, LispType::Lisp_Cons)) }
}

/// Set the car of a cons cell.
fn XSETCAR(c: LispObject, n: LispObject) {
    let cons_cell = XCONS(c);
    unsafe {
        (*cons_cell).car = n;
    }
}

/// Set the cdr of a cons cell.
fn XSETCDR(c: LispObject, n: LispObject) {
    let cons_cell = XCONS(c);
    unsafe {
        (*cons_cell).cdr = n;
    }
}

/// Set the car of CELL to be NEWCAR. Returns NEWCAR.
/// (fn CELL NEWCAR)
#[lisp_fn(name = "setcar", min = "2")]
pub fn setcar(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), LispObject::from_raw(Qconsp), cell);
        CHECK_IMPURE(cell.to_raw(), XCONS(cell) as *const libc::c_void);
    }

    XSETCAR(cell, newcar);
    newcar
}

/// Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.
/// (fn CELL NEWCDR)
#[lisp_fn(name = "setcdr", min = "2")]
fn setcdr(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), LispObject::from_raw(Qconsp), cell);
        CHECK_IMPURE(cell.to_raw(), XCONS(cell) as *const libc::c_void);
    }

    XSETCDR(cell, newcar);
    newcar
}

/// Is `object` nil?
pub fn NILP(object: LispObject) -> bool {
    object == Qnil
}

unsafe fn XCAR(object: LispObject) -> LispObject {
    (*XCONS(object)).car
}

unsafe fn XCDR(object: LispObject) -> LispObject {
    (*XCONS(object)).cdr
}

/// Take the car/cdr of a cons cell, or signal an error if it's a
/// different type.
///
/// # Porting Notes
///
/// This is equivalent to `CAR`/`CDR` in C code.
///
/// # Usage
///  Return the car of LIST.  If arg is nil, return nil.
///  Error if arg is not nil and not a cons cell.  See also `car-safe'.
///  See Info node `(elisp)Cons Cells' for a discussion of related basic
///  Lisp concepts such as car, cdr, cons cell and list.
///  (fn LIST)
#[lisp_fn(name = "car", min = "1")]
fn car(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe { XCAR(object) }
    } else if NILP(object) {
        Qnil
    } else {
        LispObject::from_raw(unsafe { wrong_type_argument(Qlistp, object.to_raw()) })
    }
}

/// Return the cdr of LIST.  If arg is nil, return nil.
/// Error if arg is not nil and not a cons cell.  See also `cdr-safe'.
/// See Info node `(elisp)Cons Cells' for a discussion of related basic
/// Lisp concepts such as cdr, car, cons cell and list.
/// (fn LIST)
#[lisp_fn(name = "cdr", min = "1")]
fn cdr(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe { XCDR(object) }
    } else if NILP(object) {
        Qnil
    } else {
        LispObject::from_raw(unsafe { wrong_type_argument(Qlistp, object.to_raw()) })
    }
}

/// return t if OBJECT is a list, that is a cons cell or nil, Otherwise, return nil.
/// (fn OBJECT)
#[lisp_fn(name = "listp", min = "1")]
fn listp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        LispObject::constant_t()
    } else {
        Qnil
    }
}

/// Return t if OBJECT is not a list.  Lists include nil.
/// (fn OBJECT)
#[lisp_fn(name = "nlistp", min = "1")]
fn nlistp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        Qnil
    } else {
        LispObject::constant_t()
    }
}
