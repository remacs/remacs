extern crate libc;

use std::os::raw::c_char;
use std::ptr;
use std::mem;

use lisp::{LispObject, LispType, XTYPE, XUNTAG, Qt, Qnil, LispSubr, PvecType, VectorLikeHeader,
           PSEUDOVECTOR_AREA_BITS, CHECK_TYPE};

extern "C" {
    static Qconsp: LispObject;
    fn CHECK_IMPURE(obj: LispObject, ptr: *const libc::c_void);
}


fn CONSP(x: LispObject) -> bool {
    XTYPE(x) == LispType::Lisp_Cons
}

fn Fconsp(object: LispObject) -> LispObject {
    if CONSP(object) { unsafe { Qt } } else { Qnil }
}

lazy_static! {
    pub static ref Sconsp: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fconsp as *const libc::c_void),
        min_args: 1,
        max_args: 1,
        symbol_name: ("consp\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return t if OBJECT is a cons cell.

(fn OBJECT)\0".as_ptr()) as *const c_char,
    };
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

#[no_mangle]
pub extern "C" fn Fsetcar(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), Qconsp, cell);
        CHECK_IMPURE(cell, XCONS(cell) as *const libc::c_void);
    }

    XSETCAR(cell, newcar);
    newcar
}

lazy_static! {
    pub static ref Ssetcar: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fsetcar as *const libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("setcar\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Set the car of CELL to be NEWCAR. Returns NEWCAR.

(fn CELL NEWCAR)\0".as_ptr()) as *const c_char,
    };
}

#[no_mangle]
pub extern "C" fn Fsetcdr(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), Qconsp, cell);
        CHECK_IMPURE(cell, XCONS(cell) as *const libc::c_void);
    }

    XSETCDR(cell, newcar);
    newcar
}

lazy_static! {
    pub static ref Ssetcdr: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fsetcdr as *const libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("setcdr\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.

(fn CELL NEWCDR)\0".as_ptr()) as *const c_char,
    };
}
