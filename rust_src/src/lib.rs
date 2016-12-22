#![allow(dead_code)]

extern crate libc;

use std::os::raw::c_char;
use std::mem;

// TODO: tweak Makefile to rebuild C files if this changes.

// EMACS_INT is defined as 'long int' in lisp.h.
type EmacsInt = libc::c_longlong;

// This is dependent on CHECK_LISP_OBJECT_TYPE, a compile time flag,
// but it's usually false.
type LispObject = EmacsInt;

extern "C" {
    fn defsubr(sname: *mut LispSubr);
    static Qt: LispObject;
    fn make_number(n: EmacsInt) -> LispObject;
}

const Qnil: LispObject = 0;

const PSEUDOVECTOR_SIZE_BITS: libc::c_int = 12;
const PSEUDOVECTOR_SIZE_MASK: libc::c_int = (1 << PSEUDOVECTOR_SIZE_BITS) - 1;
const PSEUDOVECTOR_REST_BITS: libc::c_int = 12;
const PSEUDOVECTOR_REST_MASK: libc::c_int = (((1 << PSEUDOVECTOR_REST_BITS) - 1) <<
                                             PSEUDOVECTOR_SIZE_BITS);
const PSEUDOVECTOR_AREA_BITS: libc::c_int = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS;
const PVEC_TYPE_MASK: libc::c_int = 0x3f << PSEUDOVECTOR_AREA_BITS;

#[allow(non_camel_case_types)]
enum PvecType {
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

/* Number of bits in a Lisp_Object tag.  */
const GCTYPEBITS: libc::c_int = 3;

// This is also dependent on USE_LSB_TAG, which we're assuming to be 1.
const VALMASK: EmacsInt = - (1 << GCTYPEBITS);

#[repr(C)]
struct VectorLikeHeader {
    size: libc::ptrdiff_t,
}

#[repr(C)]
struct LispSubr {
    header: VectorLikeHeader,
    // TODO: lisp.h has an elaborate union here.
    function: *mut libc::c_void,
    min_args: libc::c_short,
    max_args: libc::c_short,
    symbol_name: *const c_char,
    intspec: *const c_char,
    doc: *const c_char,
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

#[allow(non_snake_case)]
fn FLOATP(a: LispObject) -> bool {
    XTYPE(a) == LispType::Lisp_Float
}

#[test]
fn test_floatp() {
    assert!(!FLOATP(Qnil));
}

#[no_mangle]
pub unsafe extern "C" fn rust_return_t() -> LispObject {
    println!("hello from rust!");
    Qt
}

#[no_mangle]
pub unsafe extern "C" fn rust_mod(x: LispObject, y: LispObject) -> LispObject {
    println!("mod from rust!");
    make_number(5)
}

#[no_mangle]
#[allow(non_snake_case)]
pub unsafe extern "C" fn rust_init_syms() {
    println!("init rust syms start");

    // TODO: to be consistent with Emacs, we should consider
    // statically allocating our LispSubr values. However:
    //
    // * we can't call .as_ptr() for a static value
    // * Rust would force us to define Sync on LispSubr
    //   see http://stackoverflow.com/a/28116557/509706
    // * the lazy_static crate might be a good fit, but
    //   we'd need to deref so make sure the data is
    //   initialised.
    //
    // TODO: this is blindly hoping we have the correct alignment.
    // We should ensure we have GCALIGNMENT (8 bytes).
    let mut Srust_return_t = Box::new(LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        // TODO: rust_return_t as standard Emacs naming.
        function: (rust_return_t as *mut libc::c_void),
        min_args: 0,
        max_args: 0,
        symbol_name: ("return-t\0".as_ptr()) as *const c_char,
        intspec: "\0".as_ptr() as *const c_char,
        doc: ("hello world\0".as_ptr()) as *const c_char,
    });

    defsubr(Srust_return_t.as_mut());

    // Shameful kludge to ensure Srust_return_t lives long enough.
    std::mem::forget(Srust_return_t);

    let mut Srust_mod = Box::new(LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (rust_mod as *mut libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("rust-mod\0".as_ptr()) as *const c_char,
        intspec: "\0".as_ptr() as *const c_char,
        doc: ("Calculate mod in rust\0".as_ptr()) as *const c_char,
    });

    defsubr(Srust_mod.as_mut());

    // Shameful kludge to ensure Srust_mod lives long enough.
    std::mem::forget(Srust_mod);

    println!("init rust syms end");
}
