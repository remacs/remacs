/// This module contains Rust definitions whose C equivalents live in
/// lisp.h.

extern crate libc;

use std::os::raw::c_char;
use std::mem;
use std::ptr;

use marker::LispMarker;

// TODO: tweak Makefile to rebuild C files if this changes.

/// Emacs values are represented as tagged pointers. A few bits are
/// used to represent the type, and the remaining bits are either used
/// to store the value directly (e.g. integers) or the address of a
/// more complex data type (e.g. a cons cell).
///
/// TODO: example representations
///
/// `EmacsInt` represents an integer big enough to hold our tagged
/// pointer representation.
///
/// In Emacs C, this is `EMACS_INT`, which is defined as `long int`.
pub type EmacsInt = libc::c_long;

/// Unsigned equivalent of `EmacsInt`. In Emacs C, this is
/// `EMACS_UINT`, which is defined as `unsigned long`.
pub type EmacsUint = libc::c_ulong;

// 2**63 - 1, which is the value of LONG_MAX in limits.h in the C
// stdlib.
pub const EMACS_INT_MAX: EmacsInt = 9223372036854775807;

// This is dependent on CHECK_LISP_OBJECT_TYPE, a compile time flag,
// but it's usually false.
pub type LispObject = EmacsInt;
// TODO: set CHECK_LISP_OBJECT_TYPE and use a struct here, as it would
// give us stronger guarantees from the type checker.

extern "C" {
    pub fn defsubr(sname: *mut LispSubr);
    fn wrong_type_argument(predicate: LispObject, value: LispObject) -> LispObject;
    pub static Qt: LispObject;
    pub static Qnumber_or_marker_p: LispObject;
}

#[allow(non_upper_case_globals)]
#[allow(dead_code)]
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
    // Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.
    Lisp_Symbol = 0,

    // Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
    // whose first member indicates the subtype.
    Lisp_Misc = 1,

    // Integer.  XINT (obj) is the integer value.
    Lisp_Int0 = 2,
    // This depends on USE_LSB_TAG in Emacs C, but in our build that
    // value is 1.
    Lisp_Int1 = 6,

    // String.  XSTRING (object) points to a struct Lisp_String.
    // The length of the string, and its contents, are stored therein.
    Lisp_String = 4,

    // Vector of Lisp objects, or something resembling it.
    // XVECTOR (object) points to a struct Lisp_Vector, which contains
    // the size and contents.  The size field also contains the type
    // information, if it's not a real vector object.
    Lisp_Vectorlike = 5,

    // Cons.  XCONS (object) points to a struct Lisp_Cons.
    Lisp_Cons = 3,

    Lisp_Float = 7,
}

// This is the set of data types that share a common structure.
// The first member of the structure is a type code from this set.
// The enum values are arbitrary, but we'll use large numbers to make it
// more likely that we'll spot the error if a random word in memory is
// mistakenly interpreted as a Lisp_Misc.
#[repr(u16)]
#[derive(PartialEq, Eq, Debug)]
#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub enum LispMiscType {
    Lisp_Misc_Free = 0x5eab,
    Lisp_Misc_Marker,
    Lisp_Misc_Overlay,
    Lisp_Misc_Save_Value,
    Lisp_Misc_Finalizer,
}

// Number of bits in a Lisp_Object tag.
const GCTYPEBITS: libc::c_int = 3;

const INTTYPEBITS: libc::c_int = GCTYPEBITS - 1;

// Largest and smallest numbers that can be represented as integers in
// Emacs lisp.
const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> INTTYPEBITS;
#[allow(dead_code)]
const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

// This is also dependent on USE_LSB_TAG, which we're assuming to be 1.
const VALMASK: EmacsInt = -(1 << GCTYPEBITS);

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

/// Note that CHECK_LISP_OBJECT_TYPE is 0 (false) in our build.
#[allow(non_snake_case)]
fn XIL(i: EmacsInt) -> LispObject {
    i as LispObject
}

#[test]
fn test_xil_xli_inverse() {
    assert!(XLI(XIL(0)) == 0);
}

pub fn make_number(n: EmacsInt) -> LispObject {
    // TODO: this is a rubbish variable name.
    let as_uint = (n << INTTYPEBITS) as EmacsUint + LispType::Lisp_Int0 as EmacsUint;
    XIL(as_uint as EmacsInt)
}

/// Convert a positive integer into its LispObject representation.
///
/// This is also the function to use when translating `XSETFASTINT`
/// from Emacs C.
// TODO: the C claims that make_natnum is faster, but it does the same
// thing as make_number when USE_LSB_TAG is 1, which it is for us. We
// should remove this in favour of make_number.
//
// TODO: it would be clearer if this function took a u64 or libc::c_int.
pub fn make_natnum(n: EmacsInt) -> LispObject {
    debug_assert!(0 <= n && n <= MOST_POSITIVE_FIXNUM);
    make_number(n)
}

#[allow(non_snake_case)]
fn XTYPE(a: LispObject) -> LispType {
    let res = XLI(a) & !VALMASK;
    // TODO: it would be better to check the type and fail,
    // https://www.reddit.com/r/rust/comments/36pgn9/integer_to_enum_after_removal_of_fromprimitive/crfy6al/
    unsafe { mem::transmute(res as u32) }
}

#[test]
fn test_xtype() {
    assert!(XTYPE(Qnil) == LispType::Lisp_Symbol);
}

#[allow(non_snake_case)]
pub fn FLOATP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Float
}

#[test]
fn test_floatp() {
    assert!(!FLOATP(Qnil));
}

#[allow(non_snake_case)]
pub fn INTEGERP(a: LispObject) -> bool {
    (XTYPE(a) as u32 & ((LispType::Lisp_Int0 as u32) | !(LispType::Lisp_Int1 as u32))) ==
    LispType::Lisp_Int0 as u32
}

#[test]
fn test_integerp() {
    assert!(!INTEGERP(Qnil));
    assert!(INTEGERP(make_number(1)));
    assert!(INTEGERP(make_natnum(1)));
}

#[allow(non_snake_case)]
#[allow(dead_code)]
pub fn SYMBOLP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Symbol
}

#[test]
fn test_symbolp() {
    assert!(SYMBOLP(Qnil));
}

/// Raise an error if `x` is the wrong type. `ok` should be a Rust/C
/// expression that evaluates if the type is correct. `predicate` is
/// the elisp-level equivalent predicate that failed.
#[allow(non_snake_case)]
pub fn CHECK_TYPE(ok: bool, predicate: LispObject, x: LispObject) {
    if !ok {
        unsafe {
            wrong_type_argument(predicate, x);
        }
    }
}

#[allow(non_snake_case)]
fn MISCP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Misc
}

#[test]
fn test_miscp() {
    assert!(!MISCP(Qnil));
}

#[allow(non_snake_case)]
pub fn XUNTAG(a: LispObject, ty: libc::c_int) -> *const libc::c_void {
    (XLI(a) as libc::intptr_t - ty as libc::intptr_t) as *const libc::c_void
}

// lisp.h uses a union for Lisp_Misc, which we emulate with an opaque
// struct.
#[repr(C)]
pub struct LispMisc {
    _ignored: i64,
}

// Supertype of all Misc types.
#[repr(C)]
pub struct LispMiscAny {
    pub ty: LispMiscType,
    // This is actually a GC marker bit plus 15 bits of padding, but
    // we don't care right now.
    padding: u16,
}

#[test]
fn test_lisp_misc_any_size() {
    // Should be 32 bits, which is 4 bytes.
    assert!(mem::size_of::<LispMiscAny>() == 4);
}

#[allow(non_snake_case)]
pub fn XMISC(a: LispObject) -> LispMisc {
    // TODO: XUNTAG should just take a LispType as an argument.
    unsafe { mem::transmute(XUNTAG(a, LispType::Lisp_Misc as libc::c_int)) }
}

#[allow(non_snake_case)]
pub fn XMISCANY(a: LispObject) -> *const LispMiscAny {
    debug_assert!(MISCP(a));
    unsafe { mem::transmute(XMISC(a)) }
}

// TODO: we should do some sanity checking, because we're currently
// exposing a safe API that dereferences raw pointers.
#[allow(non_snake_case)]
pub fn XMISCTYPE(a: LispObject) -> LispMiscType {
    unsafe { ptr::read(XMISCANY(a)).ty }
}

#[allow(non_snake_case)]
pub fn MARKERP(a: LispObject) -> bool {
    MISCP(a) && XMISCTYPE(a) == LispMiscType::Lisp_Misc_Marker
}

#[allow(non_snake_case)]
pub fn XMARKER(a: LispObject) -> *const LispMarker {
    debug_assert!(MARKERP(a));
    unsafe { mem::transmute(XMISC(a)) }
}

#[test]
fn test_markerp() {
    assert!(!MARKERP(Qnil))
}
