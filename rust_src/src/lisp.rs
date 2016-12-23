/// This module contains Rust definitions whose C equivalents live in
/// lisp.h.

extern crate libc;

use std::os::raw::c_char;
use std::mem;

// TODO: tweak Makefile to rebuild C files if this changes.

// EMACS_INT is defined as 'long int' in lisp.h.
pub type EmacsInt = libc::c_longlong;

// This is dependent on CHECK_LISP_OBJECT_TYPE, a compile time flag,
// but it's usually false.
pub type LispObject = EmacsInt;

extern "C" {
    pub fn defsubr(sname: *mut LispSubr);
    pub static Qt: LispObject;
    pub fn make_number(n: EmacsInt) -> LispObject;
}

#[allow(non_upper_case_globals)]
const Qnil: LispObject = 0;

const PSEUDOVECTOR_SIZE_BITS: libc::c_int = 12;
#[allow(dead_code)]
const PSEUDOVECTOR_SIZE_MASK: libc::c_int = (1 << PSEUDOVECTOR_SIZE_BITS) - 1;
const PSEUDOVECTOR_REST_BITS: libc::c_int = 12;
#[allow(dead_code)]
const PSEUDOVECTOR_REST_MASK: libc::c_int = (((1 << PSEUDOVECTOR_REST_BITS) - 1) <<
                                             PSEUDOVECTOR_SIZE_BITS);
pub const PSEUDOVECTOR_AREA_BITS: libc::c_int = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS;
#[allow(dead_code)]
const PVEC_TYPE_MASK: libc::c_int = 0x3f << PSEUDOVECTOR_AREA_BITS;

#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub enum PvecType {
    // TODO: confirm these are the right numbers.
    PVEC_NORMAL_VECTOR = 0,
    PVEC_FREE = 1,
    PVEC_PROCESS = 2,
    PVEC_FRAME = 3,
    PVEC_WINDOW = 4,
    PVEC_BOOL_VECTOR = 5,
    PVEC_BUFFER = 6,
    PVEC_HASH_TABLE = 7,
    PVEC_TERMINAL = 8,
    PVEC_WINDOW_CONFIGURATION = 9,
    PVEC_SUBR = 10,
    PVEC_OTHER = 11,
    PVEC_XWIDGET = 12,
    PVEC_XWIDGET_VIEW = 13,

    PVEC_COMPILED = 14,
    PVEC_CHAR_TABLE = 15,
    PVEC_SUB_CHAR_TABLE = 16,
    PVEC_FONT = 17,
}

#[repr(C)]
#[derive(PartialEq, Eq)]
#[allow(dead_code)]
enum LispType {
    /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.  */
    Lisp_Symbol = 0,

    /* Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
       whose first member indicates the subtype.  */
    Lisp_Misc = 1,

    /* Integer.  XINT (obj) is the integer value.  */
    Lisp_Int0 = 2,
    // This depend on USE_LSB_TAG in the C, but in my build that value
    // is 1.
    Lisp_Int1 = 6,

    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein.  */
    Lisp_String = 4,

    /* Vector of Lisp objects, or something resembling it.
       XVECTOR (object) points to a struct Lisp_Vector, which contains
       the size and contents.  The size field also contains the type
       information, if it's not a real vector object.  */
    Lisp_Vectorlike = 5,

    /* Cons.  XCONS (object) points to a struct Lisp_Cons.  */
    Lisp_Cons = 3,

    Lisp_Float = 7,
}

/* This is the set of data types that share a common structure.
   The first member of the structure is a type code from this set.
   The enum values are arbitrary, but we'll use large numbers to make it
   more likely that we'll spot the error if a random word in memory is
   mistakenly interpreted as a Lisp_Misc.  */
#[repr(C)]
#[derive(PartialEq, Eq)]
#[allow(dead_code)]
enum LispMiscType {
    Lisp_Misc_Free = 0x5eab,
    Lisp_Misc_Marker,
    Lisp_Misc_Overlay,
    Lisp_Misc_Save_Value,
    Lisp_Misc_Finalizer,
}

/* Number of bits in a Lisp_Object tag.  */
const GCTYPEBITS: libc::c_int = 3;

// This is also dependent on USE_LSB_TAG, which we're assuming to be 1.
const VALMASK: EmacsInt = - (1 << GCTYPEBITS);

#[repr(C)]
pub struct VectorLikeHeader {
    pub size: libc::ptrdiff_t,
}

#[repr(C)]
pub struct LispSubr {
    pub header: VectorLikeHeader,
    // TODO: lisp.h has an elaborate union here.
    pub function: *mut libc::c_void,
    pub min_args: libc::c_short,
    pub max_args: libc::c_short,
    pub symbol_name: *const c_char,
    pub intspec: *const c_char,
    pub doc: *const c_char,
}

/// Convert LispObject to EmacsInt.
///
/// It's so simple that we should avoid using this, but it's handy
/// when transliterating from C.
#[allow(non_snake_case)]
fn XLI(o: LispObject) -> EmacsInt {
    o as EmacsInt
}

#[allow(non_snake_case)]
fn XTYPE(a: LispObject) -> LispType {
    let obj = XLI(a);
    println!("obj: {:b}", obj);
    println!("mask: {:b}\nnegated: {:b}", VALMASK, !VALMASK);
    let res = XLI(a) & !VALMASK;
    println!("res: {:b} ({})", res, res);
    unsafe {
        mem::transmute(res as u32)
    }
}

#[test]
fn test_xtype() {
    assert!(XTYPE(Qnil) == LispType::Lisp_Symbol);
}

#[allow(non_snake_case)]
fn FLOATP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Float
}

#[test]
fn test_floatp() {
    assert!(!FLOATP(Qnil));
}

#[allow(non_snake_case)]
fn MISCP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Misc
}

#[test]
fn test_miscp() {
    assert!(!MISCP(Qnil));
}

#[allow(dead_code)]
#[allow(non_snake_case)]
fn XUNTAG(a: LispObject, ty: libc::c_int) -> *const libc::c_void {
    (XLI(a) - ty as EmacsInt) as *const libc::c_void
}

#[allow(dead_code)]
#[allow(non_snake_case)]
#[allow(unused_variables)]
fn XMISCTYPE(a: LispObject) -> LispMiscType {
    unimplemented!()
}

#[allow(non_snake_case)]
fn MARKERP(a: LispObject) -> bool {
    MISCP(a) && XMISCTYPE(a) == LispMiscType::Lisp_Misc_Marker
}

#[test]
fn test_markerp() {
    assert!(!MARKERP(Qnil));
}
