#![allow(non_upper_case_globals)]
#![macro_use]

/// This module contains Rust definitions whose C equivalents live in
/// lisp.h.

extern crate libc;

#[cfg(test)]
use std::cmp::max;
use std::mem;
use std::ops::Deref;
use std::fmt::{Debug, Formatter, Error};

use marker::{LispMarker, marker_position};

use remacs_sys::{EmacsInt, EmacsUint, EmacsDouble, Lisp_Object, EMACS_INT_MAX, EMACS_INT_SIZE,
                 EMACS_FLOAT_SIZE, USE_LSB_TAG, GCTYPEBITS, wrong_type_argument, Qstringp,
                 Qnumber_or_marker_p, Qt, make_float, Lisp_String, STRING_BYTES, Qlistp,
                 Qintegerp, Qconsp, circular_list, internal_equal, Fcons, CHECK_IMPURE};
use remacs_sys::Lisp_Object as CLisp_Object;

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
/// In Emacs C, this is `EMACS_INT`.
///
/// `EmacsUint` represents the unsigned equivalent of `EmacsInt`.
/// In Emacs C, this is `EMACS_UINT`.
///
/// Their definition are determined in a way consistent with Emacs C.
/// Under casual systems, they're the type isize and usize respectively.
#[repr(C)]
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct LispObject(CLisp_Object);

pub const Qnil: LispObject = LispObject(0);

impl LispObject {
    #[inline]
    pub fn constant_t() -> LispObject {
        LispObject::from_raw(unsafe { Qt })
    }

    #[inline]
    pub fn constant_nil() -> LispObject {
        Qnil
    }

    #[inline]
    pub fn from_bool(v: bool) -> LispObject {
        if v { LispObject::constant_t() } else { Qnil }
    }

    #[inline]
    pub fn from_float(v: EmacsDouble) -> LispObject {
        LispObject::from_raw(unsafe { make_float(v) })
    }

    #[inline]
    pub fn from_raw(i: EmacsInt) -> LispObject {
        LispObject(i)
    }

    #[inline]
    pub fn to_raw(self) -> EmacsInt {
        self.0
    }
}

// Number of bits in a Lisp_Object tag.
#[allow(dead_code)]
const VALBITS: EmacsInt = EMACS_INT_SIZE * 8 - GCTYPEBITS;

const INTTYPEBITS: EmacsInt = GCTYPEBITS - 1;

#[allow(dead_code)]
const FIXNUM_BITS: EmacsInt = VALBITS + 1;

const VAL_MAX: EmacsInt = EMACS_INT_MAX >> (GCTYPEBITS - 1);

const VALMASK: EmacsInt = [VAL_MAX, -(1 << GCTYPEBITS)][USE_LSB_TAG as usize];

const INTMASK: EmacsInt = (EMACS_INT_MAX >> (INTTYPEBITS - 1));

/// Bit pattern used in the least significant bits of a lisp object,
/// to denote its type.
#[repr(u8)]
#[derive(PartialEq, Eq)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum LispType {
    // Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.
    Lisp_Symbol = 0,

    // Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
    // whose first member indicates the subtype.
    Lisp_Misc = 1,

    // Integer.  XINT (obj) is the integer value.
    Lisp_Int0 = 2,
    Lisp_Int1 = 3 + (USE_LSB_TAG as usize as u8) * 3, // 3 | 6

    // String.  XSTRING (object) points to a struct Lisp_String.
    // The length of the string, and its contents, are stored therein.
    Lisp_String = 4,

    // Vector of Lisp objects, or something resembling it.
    // XVECTOR (object) points to a struct Lisp_Vector, which contains
    // the size and contents.  The size field also contains the type
    // information, if it's not a real vector object.
    Lisp_Vectorlike = 5,

    // Cons.  XCONS (object) points to a struct Lisp_Cons.
    Lisp_Cons = 6 - (USE_LSB_TAG as usize as u8) * 3, // 6 | 3

    Lisp_Float = 7,
}

impl LispObject {
    #[allow(unused_unsafe)]
    pub fn get_type(self) -> LispType {
        let raw = self.to_raw() as EmacsUint;
        let res = (if USE_LSB_TAG {
                       raw & (!VALMASK as EmacsUint)
                   } else {
                       raw >> VALBITS
                   }) as u8;
        unsafe { mem::transmute(res) }
    }

    #[inline]
    pub fn get_untaggedptr(self) -> *mut libc::c_void {
        (self.to_raw() & VALMASK) as libc::intptr_t as *mut libc::c_void
    }
}

// Symbol support (LispType == Lisp_Symbol == 0)

impl LispObject {
    pub fn is_symbol(self) -> bool {
        self.get_type() == LispType::Lisp_Symbol
    }
}

// Misc support (LispType == Lisp_Misc == 1)

// This is the set of data types that share a common structure.
// The first member of the structure is a type code from this set.
// The enum values are arbitrary, but we'll use large numbers to make it
// more likely that we'll spot the error if a random word in memory is
// mistakenly interpreted as a Lisp_Misc.
#[repr(u16)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub enum LispMiscType {
    Free = 0x5eab,
    Marker,
    Overlay,
    SaveValue,
    Finalizer,
}

// Lisp_Misc is a union. Now we don't really care about its variants except the
// super type layout. LispMisc is an unsized type for this, and LispMiscAny is
// only the header and a padding, which is consistent with the c version.
// directly creating and moving or copying this struct is simply wrong!
// If needed, we can calculate all variants size and allocate properly.

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct ExternalPtr<T>(*mut T);

impl<T> ExternalPtr<T> {
    pub fn new(p: *mut T) -> ExternalPtr<T> {
        ExternalPtr(p)
    }

    pub fn as_ptr(&self) -> *const T {
        self.0
    }
}

impl<T> Deref for ExternalPtr<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

pub type LispMiscRef = ExternalPtr<LispMiscAny>;

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

impl LispObject {
    pub fn is_misc(self) -> bool {
        self.get_type() == LispType::Lisp_Misc
    }

    pub unsafe fn to_misc_unchecked(self) -> LispMiscRef {
        LispMiscRef::new(mem::transmute(self.get_untaggedptr()))
    }

    pub unsafe fn get_misc_type_unchecked(self) -> LispMiscType {
        self.to_misc_unchecked().ty
    }
}

// Fixnum(Integer) support (LispType == Lisp_Int0 | Lisp_Int1 == 2 | 6(LSB) )

/// Fixnums are inline integers that fit directly into Lisp's tagged word.
/// There's two LispType variants to provide an extra bit.

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> INTTYPEBITS;
#[allow(dead_code)]
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

/// Natnums(natural number) are the non-negative fixnums.
/// There were special branches in the original code for better performance.
/// However they are unified into the fixnum logic under LSB mode.
/// TODO: Recheck these logic in original C code.

impl LispObject {
    #[inline]
    pub unsafe fn from_fixnum_unchecked(n: EmacsInt) -> LispObject {
        let o = if USE_LSB_TAG {
            (n << INTTYPEBITS) as EmacsUint + LispType::Lisp_Int0 as EmacsUint
        } else {
            (n & INTMASK) as EmacsUint + ((LispType::Lisp_Int0 as EmacsUint) << VALBITS)
        };
        LispObject::from_raw(o as EmacsInt)
    }

    #[inline]
    pub fn to_fixnum(self) -> Option<EmacsInt> {
        if self.is_fixnum() {
            let raw = self.to_raw();
            if !USE_LSB_TAG {
                Some(raw & INTMASK)
            } else {
                Some(raw >> INTTYPEBITS)
            }
        } else {
            None
        }
    }

    #[inline]
    pub fn is_fixnum(self) -> bool {
        let ty = self.get_type();
        (ty as u8 & ((LispType::Lisp_Int0 as u8) | !(LispType::Lisp_Int1 as u8))) ==
        LispType::Lisp_Int0 as u8
    }

    /// TODO: Bignum support? (Current Emacs doesn't have it)
    #[inline]
    pub fn is_integer(self) -> bool {
        self.is_fixnum()
    }

    #[inline]
    pub fn is_natnum(self) -> bool {
        self.is_integer() && 0 <= XINT(self)
    }
}

// Cons support (LispType == 6 | 3)

/// From FOR_EACH_TAIL_INTERNAL in lisp.h
pub struct TailsIter {
    list: LispObject,
    safe: bool,
    tail: LispObject,
    tortoise: LispObject,
    max: isize,
    n: isize,
    q: u16,
}

impl TailsIter {
    fn new(list: LispObject, safe: bool) -> Self {
        Self {
            list,
            safe,
            tail: list,
            tortoise: list,
            max: 2,
            n: 0,
            q: 2,
        }
    }

    fn circular(&self) -> Option<LispCons> {
        if !self.safe {
            unsafe {
                circular_list(self.tail.to_raw());
            }
        } else {
            return None;
        }
    }
}

impl Iterator for TailsIter {
    type Item = LispCons;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tail.as_cons() {
            None => {
                if !self.safe {
                    CHECK_LIST_END(self.tail.to_raw(), self.list.to_raw());
                }
                return None;
            }
            Some(tail_cons) => {
                self.tail = tail_cons.cdr();
                self.q = self.q.wrapping_sub(1);
                if self.q != 0 {
                    if self.tail == self.tortoise {
                        return self.circular();
                    }
                } else {
                    self.n = self.n.wrapping_sub(1);
                    if self.n > 0 {
                        if self.tail == self.tortoise {
                            return self.circular();
                        }
                    } else {
                        self.max <<= 1;
                        self.q = self.max as u16;
                        self.n = self.max >> 16;
                        self.tortoise = self.tail;
                    }
                }
                Some(tail_cons)
            }
        }
    }
}

impl LispObject {
    #[inline]
    pub fn cons(car: LispObject, cdr: LispObject) -> Self {
        unsafe { LispObject::from_raw(Fcons(car.to_raw(), cdr.to_raw())) }
    }

    #[inline]
    pub fn is_cons(self) -> bool {
        self.get_type() == LispType::Lisp_Cons
    }

    #[inline]
    pub fn as_cons(self) -> Option<LispCons> {
        if self.is_cons() {
            Some(LispCons(self))
        } else {
            None
        }
    }

    #[inline]
    pub fn as_cons_or_error(self) -> LispCons {
        if self.is_cons() {
            LispCons(self)
        } else {
            unsafe { wrong_type_argument(Qconsp, self.to_raw()) }
        }
    }

    /// Iterate over all tails of self.  self should be a list, i.e. a chain
    /// of cons cells ending in nil.  Otherwise a wrong-type-argument error
    /// will be signaled.
    pub fn iter_tails(self) -> TailsIter {
        TailsIter::new(self, false)
    }

    /// Iterate over all tails of self.  If self is not a cons-chain,
    /// iteration will stop at the first non-cons without signaling.
    pub fn iter_tails_safe(self) -> TailsIter {
        TailsIter::new(self, true)
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
struct Lisp_Cons {
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

/// A newtype for objects we know are conses.
#[derive(Clone, Copy)]
pub struct LispCons(LispObject);

impl LispCons {
    pub fn as_obj(self) -> LispObject {
        self.0
    }

    fn _extract(self) -> *mut Lisp_Cons {
        unsafe { mem::transmute(XUNTAG(self.0, LispType::Lisp_Cons)) }
    }

    /// Return the car (first cell).
    pub fn car(self) -> LispObject {
        unsafe { (*self._extract()).car }
    }

    /// Return the cdr (second cell).
    pub fn cdr(self) -> LispObject {
        unsafe { (*self._extract()).cdr }
    }

    /// Set the car of the cons cell.
    pub fn set_car(self, n: LispObject) {
        unsafe {
            (*self._extract()).car = n;
        }
    }

    /// Set the car of the cons cell.
    pub fn set_cdr(self, n: LispObject) {
        unsafe {
            (*self._extract()).cdr = n;
        }
    }

    /// Check that "self" is an impure (i.e. not readonly) cons cell.
    pub fn check_impure(self) {
        unsafe {
            CHECK_IMPURE(self.0.to_raw(), self._extract() as *const libc::c_void);
        }
    }
}

// Float support (LispType == Lisp_Float == 7 )

/// Represents a floating point value in elisp, or GC bookkeeping for
/// floats.
///
/// # Porting from C
///
/// `Lisp_Float` in C uses a union between a `double` and a
/// pointer. We assume a double, as that's the common case, and
/// require callers to transmute to a `LispFloatChain` if they need
/// the pointer.
#[repr(C)]
pub struct LispFloat {
    data: [u8; EMACS_FLOAT_SIZE as usize],
}

#[repr(C)]
pub struct LispFloatChainRepr(*const LispFloat);

impl LispFloat {
    pub fn as_data(&self) -> &EmacsDouble {
        unsafe { &*(self.data.as_ptr() as *const EmacsDouble) }
    }
    pub fn as_chain(&self) -> &LispFloatChainRepr {
        unsafe { &*(self.data.as_ptr() as *const LispFloatChainRepr) }
    }
}

#[test]
fn test_lisp_float_size() {
    let double_size = mem::size_of::<EmacsDouble>();
    let ptr_size = mem::size_of::<*const LispFloat>();

    assert!(mem::size_of::<LispFloat>() == max(double_size, ptr_size));
}

pub type LispFloatRef = ExternalPtr<LispFloat>;

impl LispObject {
    #[inline]
    pub fn is_float(self) -> bool {
        self.get_type() == LispType::Lisp_Float
    }

    #[inline]
    pub unsafe fn to_float_unchecked(self) -> LispFloatRef {
        debug_assert!(self.is_float());
        LispFloatRef::new(mem::transmute(self.get_untaggedptr()))
    }

    pub unsafe fn get_float_data_unchecked(self) -> EmacsDouble {
        *self.to_float_unchecked().as_data()
    }

    pub fn to_float(self) -> Option<EmacsDouble> {
        if self.is_float() {
            Some(unsafe { self.get_float_data_unchecked() })
        } else {
            None
        }
    }

    /// If the LispObject is a number (of any kind), get a floating point value for it
    pub fn extract_float(self) -> Option<EmacsDouble> {
        let d = self.to_float();
        d.or(self.to_fixnum().map(|i| i as EmacsDouble))
    }
}

// String support (LispType == 4)

impl LispObject {
    #[inline]
    pub fn is_string(self) -> bool {
        self.get_type() == LispType::Lisp_String
    }
}

// Other functions

impl LispObject {
    #[inline]
    pub fn is_number(self) -> bool {
        self.is_integer() || self.is_float()
    }

    #[inline]
    pub fn is_nil(self) -> bool {
        self == Qnil
    }

    #[inline]
    pub fn is_marker(self) -> bool {
        self.is_misc() && XMISCTYPE(self) == LispMiscType::Marker
    }

    // The three Emacs Lisp comparison functions.

    #[inline]
    pub fn eq(self, other: LispObject) -> bool {
        self == other
    }

    #[inline]
    pub fn eql(self, other: LispObject) -> bool {
        if self.is_float() {
            self.equal(other)
        } else {
            self.eq(other)
        }
    }

    #[inline]
    pub fn equal(self, other: LispObject) -> bool {
        unsafe { internal_equal(self.to_raw(), other.to_raw(), 0, false, Qnil.to_raw()) }
    }
}

/// Used to denote functions that have no limit on the maximum number
/// of arguments.
pub const MANY: i16 = -2;

impl Debug for LispObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let ty = self.get_type();
        let self_ptr = &self as *const _ as usize;
        if ty as u8 >= 8 {
            write!(f,
                   "#<INVALID-OBJECT @ {:#X}: VAL({:#X})>",
                   self_ptr,
                   self.to_raw())?;
            return Ok(());
        }
        match ty {
            LispType::Lisp_Symbol => {
                write!(f, "#<SYMBOL @ {:#X}: VAL({:#X})>", self_ptr, self.to_raw())?;
            }
            LispType::Lisp_Cons => {
                write!(f, "#<CONS @ {:#X}: VAL({:#X})>", self_ptr, self.to_raw())?;
            }
            LispType::Lisp_Float => {
                write!(f, "#<FLOAT @ {:#X}: VAL({:#X})>", self_ptr, self.to_raw())?;
            }
            LispType::Lisp_Vectorlike => {
                write!(f,
                       "#<VECTOR-LIKE @ {:#X}: VAL({:#X})>",
                       self_ptr,
                       self.to_raw())?;
            }
            LispType::Lisp_Int0 |
            LispType::Lisp_Int1 => {
                write!(f, "#<INT @ {:#X}: VAL({:#X})>", self_ptr, self.to_raw())?;
            }
            LispType::Lisp_Misc => {
                write!(f, "#<MISC @ {:#X}: VAL({:#X})>", self_ptr, self.to_raw())?;
            }
            LispType::Lisp_String => {
                write!(f, "#<STRING @ {:#X}: VAL({:#X})>", self_ptr, self.to_raw())?;
            }
        }
        Ok(())
    }
}

/// # Porting Notes
///
/// This module contains some functions that is originally contained in Emacs C code
/// as macros and global functions, which does not conforms to Rust naming rules well
/// and lacks unsafe marks. However we'll keep them during the porting process to make
/// the porting easy, we should be able to remove once the relevant functionality is Rust-only.
mod deprecated {
    use super::*;
    use libc;
    use std;
    use remacs_sys::EmacsInt;

    /// Convert a LispObject to an EmacsInt.
    #[allow(non_snake_case)]
    #[allow(dead_code)]
    pub fn XLI(o: LispObject) -> EmacsInt {
        o.to_raw()
    }

    /// Convert an EmacsInt to an LispObject.
    #[allow(non_snake_case)]
    #[allow(dead_code)]
    pub fn XIL(i: EmacsInt) -> LispObject {
        // Note that CHECK_LISP_OBJECT_TYPE is 0 (false) in our build.
        LispObject::from_raw(i)
    }

    #[test]
    fn test_xil_xli_inverse() {
        assert!(XLI(XIL(0)) == 0);
    }

    /// Convert an integer to an elisp object representing that number.
    ///
    /// # Porting from C
    ///
    /// This function is a direct replacement for the C function
    /// `make_number`.
    ///
    /// The C macro `XSETINT` should also be replaced with this when
    /// porting. For example, `XSETINT(x, y)` should be written as `x =
    /// make_number(y)`.
    pub fn make_number(n: EmacsInt) -> LispObject {
        unsafe { LispObject::from_fixnum_unchecked(n) }
    }

    /// Extract the integer value from an elisp object representing an
    /// integer.
    #[allow(non_snake_case)]
    pub fn XINT(a: LispObject) -> EmacsInt {
        a.to_fixnum().unwrap()
    }

    #[test]
    fn test_xint() {
        let boxed_5 = make_number(5);
        assert!(XINT(boxed_5) == 5);
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
    pub fn XMISC(a: LispObject) -> LispMiscRef {
        unsafe { a.to_misc_unchecked() }
    }

    #[allow(non_snake_case)]
    #[allow(dead_code)]
    pub fn XMISCANY(a: LispObject) -> *const LispMiscAny {
        debug_assert!(a.is_misc());
        XMISC(a).0
    }

    // TODO: we should do some sanity checking, because we're currently
    // exposing a safe API that dereferences raw pointers.
    #[allow(non_snake_case)]
    pub fn XMISCTYPE(a: LispObject) -> LispMiscType {
        XMISC(a).ty
    }

    #[allow(non_snake_case)]
    #[allow(dead_code)]
    pub fn XFLOAT(a: LispObject) -> LispFloatRef {
        unsafe { a.to_float_unchecked() }
    }

    #[allow(non_snake_case)]
    #[allow(dead_code)]
    pub fn XFLOAT_DATA(f: LispObject) -> f64 {
        unsafe { f.get_float_data_unchecked() }
    }

    pub fn XSTRING(a: LispObject) -> *mut Lisp_String {
        debug_assert!(a.is_string());
        unsafe { std::mem::transmute(XUNTAG(a, LispType::Lisp_String)) }
    }

    pub fn SBYTES(string: LispObject) -> libc::ptrdiff_t {
        unsafe { STRING_BYTES(XSTRING(string)) }
    }

    pub fn SCHARS(string: LispObject) -> libc::ptrdiff_t {
        unsafe { (*XSTRING(string)).size }
    }


    /// Convert a tagged pointer to a normal C pointer.
    ///
    /// See the docstring for `LispType` for more information on tagging.
    #[allow(non_snake_case)]
    pub fn XUNTAG(a: LispObject, _: LispType) -> *const libc::c_void {
        a.get_untaggedptr()
    }

    // Implementation of the XFASTINT depends on the USE_LSB_TAG
    // in Emacs C. But we selected this implementation as in our
    // build that value is 1.
    // A must be nonnegative.
    #[allow(dead_code)]
    #[allow(non_snake_case)]
    pub fn XFASTINT(a: LispObject) -> EmacsInt {
        let n: EmacsInt = XINT(a);
        debug_assert!(0 <= n);
        n
    }
}

pub use self::deprecated::*;

/// Check that `x` is an integer or float, coercing markers to integers.
///
/// If `x` has a different type, raise an elisp error.
///
/// This function is equivalent to
/// `CHECK_NUMBER_OR_FLOAT_COERCE_MARKER` in Emacs C, but returns a
/// value rather than assigning to a variable.
pub fn check_number_coerce_marker(x: LispObject) -> LispObject {
    if x.is_marker() {
        make_natnum(marker_position(x) as EmacsInt)
    } else {
        unsafe {
            CHECK_TYPE(x.is_number(), LispObject::from_raw(Qnumber_or_marker_p), x);
        }
        x
    }
}

/// Raise an error if `x` is the wrong type. `ok` should be a Rust/C
/// expression that evaluates if the type is correct. `predicate` is
/// the elisp-level equivalent predicate that failed.
#[allow(non_snake_case)]
pub fn CHECK_TYPE(ok: bool, predicate: LispObject, x: LispObject) {
    if !ok {
        unsafe {
            wrong_type_argument(predicate.to_raw(), x.to_raw());
        }
    }
}

/// Raise an error if `x` is not a number.
#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn CHECK_NUMBER(x: Lisp_Object) {
    let x = LispObject::from_raw(x);
    CHECK_TYPE(x.is_integer(),
               LispObject::from_raw(unsafe { Qintegerp }),
               x);
}

/// Raise an error if `x` is not a lisp string.
#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn CHECK_STRING(x: Lisp_Object) {
    let x = LispObject::from_raw(x);
    CHECK_TYPE(x.is_string(), LispObject::from_raw(unsafe { Qstringp }), x);
}

/// Raise an error if `x` is not a cons.
#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn CHECK_CONS(x: Lisp_Object) {
    let x = LispObject::from_raw(x);
    CHECK_TYPE(x.is_cons(), LispObject::from_raw(unsafe { Qconsp }), x);
}


/// Raise an error if `x`, the final cons of the list `y`, is not nil.
#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn CHECK_LIST_END(x: Lisp_Object, y: Lisp_Object) {
    let x = LispObject::from_raw(x);
    let y = LispObject::from_raw(y);
    CHECK_TYPE(x == Qnil, LispObject::from_raw(unsafe { Qlistp }), y);
}

#[allow(non_snake_case)]
pub fn XMARKER(a: LispObject) -> *const LispMarker {
    debug_assert!(a.is_marker());
    unsafe { mem::transmute(XMISC(a)) }
}
