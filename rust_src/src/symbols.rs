use std::os::raw::c_char;
use std::ptr;

extern crate libc;

use lisp::{XTYPE, LispObject, LispType, LispSubr, Qnil,
           VectorLikeHeader, PvecType, PSEUDOVECTOR_AREA_BITS};

extern "C" {
    static Qt: LispObject;
}

/// Is this LispObject a symbol?
#[allow(non_snake_case)]
pub fn SYMBOLP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Symbol
}

#[test]
fn test_symbolp() {
    assert!(SYMBOLP(Qnil));
}

fn Fsymbolp(object: LispObject) -> LispObject {
    if SYMBOLP(object) {
        unsafe { Qt }
    } else {
        Qnil
    }
}

lazy_static! {
    pub static ref Ssymbolp: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fsymbolp as *const libc::c_void),
        min_args: 1,
        max_args: 1,
        symbol_name: ("symbolp\0".as_ptr()) as *const c_char,
        intspec: ptr::null(),
        doc: ("Return t if OBJECT is a symbol.

(fn OBJECT)\0".as_ptr()) as *const c_char,
    };
}
