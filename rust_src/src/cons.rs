extern crate libc;

use std::os::raw::c_char;
use std::ptr;

use lisp::{LispObject, LispType, XTYPE, Qt, Qnil, LispSubr,
           PvecType, VectorLikeHeader, PSEUDOVECTOR_AREA_BITS};

fn CONSP(x: LispObject) -> bool {
    XTYPE(x) == LispType::Lisp_Cons
}

fn Fconsp(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe {
            Qt
        }
    } else {
        Qnil
    }
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
