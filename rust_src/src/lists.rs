use std::ptr;
use std::mem;

use libc;

use lisp::{CHECK_TYPE, LispObject, LispType, Qnil, XTYPE, XUNTAG};
use remacs_sys::{Lisp_Object, wrong_type_argument};

extern "C" {
    static Qconsp: Lisp_Object;
    fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const libc::c_void);
    static Qlistp: Lisp_Object;
}


pub fn CONSP(x: LispObject) -> bool {
    XTYPE(x) == LispType::Lisp_Cons
}

fn atom(object: LispObject) -> LispObject {
    if CONSP(object) {
        Qnil
    } else {
        LispObject::constant_t()
    }
}

defun!("atom",
       Fatom(object),
       Satom,
       atom,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is not a cons cell.  This includes nil.");

fn consp(object: LispObject) -> LispObject {
    if CONSP(object) {
        LispObject::constant_t()
    } else {
        Qnil
    }
}

defun!("consp",
       Fconsp(object),
       Sconsp,
       consp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a cons cell.

(fn OBJECT)");

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

pub fn setcar(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), LispObject::from_raw(Qconsp), cell);
        CHECK_IMPURE(cell.to_raw(), XCONS(cell) as *const libc::c_void);
    }

    XSETCAR(cell, newcar);
    newcar
}

defun!("setcar",
       Fsetcar(cell, newcar),
       Ssetcar,
       setcar,
       2,
       2,
       ptr::null(),
       "Set the car of CELL to be NEWCAR. Returns NEWCAR.

(fn CELL NEWCAR)");

fn setcdr(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), LispObject::from_raw(Qconsp), cell);
        CHECK_IMPURE(cell.to_raw(), XCONS(cell) as *const libc::c_void);
    }

    XSETCDR(cell, newcar);
    newcar
}

defun!("setcdr",
       Fsetcdr(cell, newcar),
       Ssetcdr,
       setcdr,
       2,
       2,
       ptr::null(),
       "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.

(fn CELL NEWCDR)");

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
fn car(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe { XCAR(object) }
    } else if NILP(object) {
        Qnil
    } else {
        LispObject::from_raw(unsafe { wrong_type_argument(Qlistp, object.to_raw()) })
    }
}

defun!("car",
       Fcar(list),
       Scar,
       car,
       1,
       1,
       ptr::null(),
       "Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a \
        cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of \
        related basic
Lisp concepts such as car, cdr, cons cell and list.

(fn LIST)");

fn cdr(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe { XCDR(object) }
    } else if NILP(object) {
        Qnil
    } else {
        LispObject::from_raw(unsafe { wrong_type_argument(Qlistp, object.to_raw()) })
    }
}

defun!("cdr",
       Fcdr(list),
       Scdr,
       cdr,
       1,
       1,
       ptr::null(),
       "Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a \
        cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of \
        related basic
Lisp concepts such as cdr, car, cons cell and list.

(fn LIST)");

fn listp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        LispObject::constant_t()
    } else {
        Qnil
    }
}

defun!("listp",
       Flistp(object),
       Slistp,
       listp,
       1,
       1,
       ptr::null(),
       "return t if OBJECT is a list, that is a cons cell or nil, Otherwise, return nil.

(fn \
        OBJECT)");

fn nlistp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        Qnil
    } else {
        LispObject::constant_t()
    }
}

defun!("nlistp",
       Fnlistp(object),
       Snlistp,
       nlistp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is not a list.  Lists include nil.

(fn OBJECT)");
