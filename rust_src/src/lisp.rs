//! This module contains Rust definitions whose C equivalents live in
//! lisp.h.

use std::convert::{From, Into};
use std::ffi::CString;
use std::fmt;
use std::fmt::{Debug, Display, Error, Formatter};
use std::mem;
use std::ops::{Deref, DerefMut};

use libc::{c_char, c_void, intptr_t, uintptr_t};

use crate::{
    buffers::LispBufferRef,
    eval::FUNCTIONP,
    hashtable::{
        HashLookupResult::{Found, Missing},
        LispHashTableRef,
    },
    lists::{list, memq, CarIter, LispCons, LispConsCircularChecks, LispConsEndChecks},
    multibyte::LispStringRef,
    process::LispProcessRef,
    remacs_sys::{build_string, make_float, Fmake_hash_table},
    remacs_sys::{
        equal_kind, pvec_type, EmacsDouble, EmacsInt, EmacsUint, Lisp_Bits, USE_LSB_TAG, VALMASK,
    },
    remacs_sys::{Lisp_Misc_Any, Lisp_Misc_Type, Lisp_Subr, Lisp_Type},
    remacs_sys::{QCtest, Qautoload, Qeq, Qnil, Qsubrp, Qt},
    remacs_sys::{Vbuffer_alist, Vprocess_alist},
};

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
#[repr(transparent)]
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct LispObject(pub EmacsInt);

impl LispObject {
    pub fn from_C(n: EmacsInt) -> Self {
        LispObject(n)
    }

    pub fn from_C_unsigned(n: EmacsUint) -> Self {
        Self::from_C(n as EmacsInt)
    }

    pub fn to_C(self) -> EmacsInt {
        self.0
    }

    pub fn to_C_unsigned(self) -> EmacsUint {
        self.0 as EmacsUint
    }

    pub fn from_float(v: EmacsDouble) -> Self {
        unsafe { make_float(v) }
    }
}

impl<T> From<Option<T>> for LispObject
where
    LispObject: From<T>,
{
    fn from(v: Option<T>) -> Self {
        match v {
            None => Qnil,
            Some(v) => v.into(),
        }
    }
}

// ExternalPtr

#[repr(transparent)]
pub struct ExternalPtr<T>(*mut T);

impl<T> Copy for ExternalPtr<T> {}

// Derive fails for this type so do it manually
impl<T> Clone for ExternalPtr<T> {
    fn clone(&self) -> Self {
        ExternalPtr::new(self.0)
    }
}

impl<T> ExternalPtr<T> {
    pub fn new(p: *mut T) -> Self {
        ExternalPtr(p)
    }

    pub fn is_null(self) -> bool {
        self.0.is_null()
    }

    pub fn as_ptr(self) -> *const T {
        self.0
    }

    pub fn as_mut(&mut self) -> *mut T {
        self.0
    }

    pub fn from_ptr(ptr: *mut c_void) -> Option<Self> {
        unsafe { ptr.as_ref().map(|p| mem::transmute(p)) }
    }

    pub fn replace_ptr(&mut self, ptr: *mut T) {
        self.0 = ptr;
    }

    pub unsafe fn ptr_offset(&mut self, size: isize) {
        let ptr = self.0.offset(size);
        self.replace_ptr(ptr);
    }

    pub unsafe fn ptr_add(&mut self, size: usize) {
        let ptr = self.0.add(size);
        self.replace_ptr(ptr);
    }

    pub unsafe fn ptr_sub(&mut self, size: usize) {
        let ptr = self.0.sub(size);
        self.replace_ptr(ptr);
    }
}

impl<T> Deref for ExternalPtr<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl<T> DerefMut for ExternalPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

impl<T> PartialEq for ExternalPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

// Misc support (LispType == Lisp_Misc == 1)

// Lisp_Misc is a union. Now we don't really care about its variants except the
// super type layout. LispMisc is an unsized type for this, and LispMiscAny is
// only the header and a padding, which is consistent with the c version.
// directly creating and moving or copying this struct is simply wrong!
// If needed, we can calculate all variants size and allocate properly.

pub type LispMiscRef = ExternalPtr<Lisp_Misc_Any>;

impl LispMiscRef {
    pub fn get_type(self) -> Lisp_Misc_Type {
        self.type_()
    }
}

impl LispStructuralEqual for LispMiscRef {
    fn equal(
        &self,
        other: Self,
        kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool {
        if self.get_type() != other.get_type() {
            false
        } else if let (Some(ov1), Some(ov2)) = (self.as_overlay(), other.as_overlay()) {
            ov1.equal(ov2, kind, depth, ht)
        } else if let (Some(marker1), Some(marker2)) = (self.as_marker(), other.as_marker()) {
            marker1.equal(marker2, kind, depth, ht)
        } else {
            false
        }
    }
}

impl LispObject {
    pub fn is_misc(self) -> bool {
        self.get_type() == Lisp_Type::Lisp_Misc
    }

    pub fn force_misc(self) -> LispMiscRef {
        unsafe { self.to_misc_unchecked() }
    }

    pub fn as_misc(self) -> Option<LispMiscRef> {
        if self.is_misc() {
            unsafe { Some(self.to_misc_unchecked()) }
        } else {
            None
        }
    }

    unsafe fn to_misc_unchecked(self) -> LispMiscRef {
        LispMiscRef::new(self.get_untaggedptr() as *mut Lisp_Misc_Any)
    }
}

impl Debug for LispMiscRef {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "#<MISC @ {:p}: VAL({:#X})>",
            self.as_ptr(),
            LispObject::tag_ptr(*self, Lisp_Type::Lisp_Misc).to_C()
        )
    }
}

// Lisp_Subr support

pub type LispSubrRef = ExternalPtr<Lisp_Subr>;
unsafe impl Sync for LispSubrRef {}

impl LispSubrRef {
    pub fn is_many(self) -> bool {
        !self.0.is_null() && self.max_args() == -2
    }

    pub fn is_unevalled(self) -> bool {
        !self.0.is_null() && self.max_args() == -1
    }

    pub fn max_args(self) -> i16 {
        unsafe { (*self.0).max_args }
    }

    pub fn min_args(self) -> i16 {
        unsafe { (*self.0).min_args }
    }

    pub fn symbol_name(self) -> *const c_char {
        unsafe { (*self.0).symbol_name }
    }
}

impl LispObject {
    pub fn is_subr(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_SUBR))
    }

    pub fn as_subr(self) -> Option<LispSubrRef> {
        self.into()
    }
}

impl From<LispObject> for LispSubrRef {
    fn from(o: LispObject) -> Self {
        o.as_subr().unwrap_or_else(|| wrong_type!(Qsubrp, o))
    }
}

impl From<LispObject> for Option<LispSubrRef> {
    fn from(o: LispObject) -> Self {
        o.as_vectorlike().and_then(ExternalPtr::as_subr)
    }
}

// Other functions

impl From<()> for LispObject {
    fn from(_v: ()) -> Self {
        Qnil
    }
}

impl From<Vec<LispObject>> for LispObject {
    fn from(v: Vec<LispObject>) -> Self {
        list(&v)
    }
}

impl<T> From<Vec<T>> for LispObject
where
    LispObject: From<T>,
{
    default fn from(v: Vec<T>) -> LispObject {
        list(
            &v.into_iter()
                .map(LispObject::from)
                .collect::<Vec<LispObject>>(),
        )
    }
}

impl From<LispObject> for bool {
    fn from(o: LispObject) -> Self {
        o.is_not_nil()
    }
}

impl From<bool> for LispObject {
    fn from(v: bool) -> Self {
        if v {
            Qt
        } else {
            Qnil
        }
    }
}

impl From<LispObject> for u32 {
    fn from(o: LispObject) -> Self {
        o.as_fixnum_or_error() as u32
    }
}

impl From<LispObject> for Option<u32> {
    fn from(o: LispObject) -> Self {
        match o.as_fixnum() {
            None => None,
            Some(n) => Some(n as u32),
        }
    }
}

impl From<!> for LispObject {
    fn from(_v: !) -> Self {
        // I'm surprized that this works
        Qnil
    }
}

/// Copies a Rust str into a new Lisp string
impl<'a> From<&'a str> for LispObject {
    fn from(s: &str) -> Self {
        let cs = CString::new(s).unwrap();
        unsafe { build_string(cs.as_ptr()) }
    }
}

impl LispObject {
    pub fn get_type(self) -> Lisp_Type {
        let raw = self.to_C_unsigned();
        let res = (if USE_LSB_TAG {
            raw & (!VALMASK as EmacsUint)
        } else {
            raw >> Lisp_Bits::VALBITS
        }) as u32;
        unsafe { mem::transmute(res) }
    }

    pub fn tag_ptr<T>(external: ExternalPtr<T>, ty: Lisp_Type) -> LispObject {
        let raw = external.as_ptr() as intptr_t;
        let res = if USE_LSB_TAG {
            let ptr = raw as intptr_t;
            let tag = ty as intptr_t;
            (ptr + tag) as EmacsInt
        } else {
            let ptr = raw as EmacsUint as uintptr_t;
            let tag = ty as EmacsUint as uintptr_t;
            ((tag << Lisp_Bits::VALBITS) + ptr) as EmacsInt
        };

        LispObject::from_C(res)
    }

    pub fn get_untaggedptr(self) -> *mut c_void {
        (self.to_C() & VALMASK) as intptr_t as *mut c_void
    }
}

impl From<LispObject> for EmacsInt {
    fn from(o: LispObject) -> Self {
        o.as_fixnum_or_error()
    }
}

impl From<LispObject> for Option<EmacsInt> {
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.as_fixnum_or_error())
        }
    }
}

impl From<LispObject> for EmacsUint {
    fn from(o: LispObject) -> Self {
        o.as_natnum_or_error()
    }
}

impl From<LispObject> for Option<EmacsUint> {
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.as_natnum_or_error())
        }
    }
}

impl From<EmacsInt> for LispObject {
    fn from(v: EmacsInt) -> Self {
        LispObject::from_fixnum(v)
    }
}

impl From<isize> for LispObject {
    fn from(v: isize) -> Self {
        LispObject::from_fixnum(v as EmacsInt)
    }
}

impl From<i32> for LispObject {
    fn from(v: i32) -> Self {
        LispObject::from_fixnum(EmacsInt::from(v))
    }
}

impl From<i16> for LispObject {
    fn from(v: i16) -> Self {
        LispObject::from_fixnum(EmacsInt::from(v))
    }
}

impl From<i8> for LispObject {
    fn from(v: i8) -> Self {
        LispObject::from_fixnum(EmacsInt::from(v))
    }
}

impl From<EmacsUint> for LispObject {
    fn from(v: EmacsUint) -> Self {
        LispObject::from_natnum(v)
    }
}

impl From<usize> for LispObject {
    fn from(v: usize) -> Self {
        LispObject::from_natnum(v as EmacsUint)
    }
}

impl From<u32> for LispObject {
    fn from(v: u32) -> Self {
        LispObject::from_natnum(EmacsUint::from(v))
    }
}

impl From<u16> for LispObject {
    fn from(v: u16) -> Self {
        LispObject::from_natnum(EmacsUint::from(v))
    }
}

impl From<u8> for LispObject {
    fn from(v: u8) -> Self {
        LispObject::from_natnum(EmacsUint::from(v))
    }
}

impl LispObject {
    pub fn is_mutex(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_MUTEX))
    }

    pub fn is_condition_variable(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_CONDVAR))
    }

    pub fn is_byte_code_function(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_COMPILED))
    }

    pub fn is_module_function(self) -> bool {
        self.as_vectorlike().map_or(false, |v| {
            v.is_pseudovector(pvec_type::PVEC_MODULE_FUNCTION)
        })
    }

    pub fn is_array(self) -> bool {
        self.is_vector() || self.is_string() || self.is_char_table() || self.is_bool_vector()
    }

    pub fn is_sequence(self) -> bool {
        self.is_cons() || self.is_nil() || self.is_array()
    }

    pub fn is_record(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_RECORD))
    }
}

/// From `FOR_EACH_ALIST_VALUE` in `lisp.h`
/// Implement `Iterator` over all values of `$data` yielding `$iter_item` type.
/// `$data` should be an `alist` and `$iter_item` type should implement `From<LispObject>`
macro_rules! impl_alistval_iter {
    ($iter_name:ident, $iter_item:ty, $data: expr) => {
        pub struct $iter_name(CarIter);

        impl $iter_name {
            pub fn new() -> Self {
                $iter_name($data.iter_cars(LispConsEndChecks::on, LispConsCircularChecks::on))
            }
        }

        impl Iterator for $iter_name {
            type Item = $iter_item;

            fn next(&mut self) -> Option<Self::Item> {
                self.0
                    .next()
                    .and_then(LispObject::as_cons)
                    .map(LispCons::cdr)
                    .and_then(Into::into)
            }
        }
    };
}

impl_alistval_iter! {LiveBufferIter, LispBufferRef, unsafe { Vbuffer_alist }}
impl_alistval_iter! {ProcessIter, LispProcessRef, unsafe { Vprocess_alist }}

pub fn is_autoload(function: LispObject) -> bool {
    match function.into() {
        None => false,
        Some((car, _)) => car.eq(Qautoload),
    }
}

pub trait LispStructuralEqual {
    fn equal(
        &self,
        other: Self,
        equal_kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool;
}

impl LispObject {
    pub fn is_nil(self) -> bool {
        self == Qnil
    }

    pub fn is_not_nil(self) -> bool {
        self != Qnil
    }

    pub fn is_t(self) -> bool {
        self == Qt
    }

    // The three Emacs Lisp comparison functions.

    pub fn eq(self, other: impl Into<LispObject>) -> bool {
        self == other.into()
    }

    pub fn eql(self, other: impl Into<LispObject>) -> bool {
        if self.is_float() {
            self.equal_no_quit(other)
        } else {
            self.eq(other)
        }
    }

    pub fn equal(self, other: impl Into<LispObject>) -> bool {
        let mut ht = LispHashTableRef::empty();
        self.equal_internal(other.into(), equal_kind::EQUAL_PLAIN, 0, &mut ht)
    }

    // Return true if O1 and O2 are equal.  EQUAL_KIND specifies what kind
    // of equality test to use: if it is EQUAL_NO_QUIT, do not check for
    // cycles or large arguments or quits; if EQUAL_PLAIN, do ordinary
    // Lisp equality; and if EQUAL_INCLUDING_PROPERTIES, do
    // equal-including-properties.
    //
    // If DEPTH is the current depth of recursion; signal an error if it
    // gets too deep.  HT is a hash table used to detect cycles; if nil,
    // it has not been allocated yet.  But ignore the last two arguments
    // if EQUAL_KIND == EQUAL_NO_QUIT.
    //
    pub fn equal_internal(
        self,
        other: Self,
        equal_kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool {
        if depth > 10 {
            assert!(equal_kind != equal_kind::EQUAL_NO_QUIT);
            if depth > 200 {
                error!("Stack overflow in equal");
            }
            if ht.is_null() {
                let mut new_ht: LispHashTableRef = callN_raw!(Fmake_hash_table, QCtest, Qeq).into();
                ht.replace_ptr(new_ht.as_mut());
            }
            match self.get_type() {
                Lisp_Type::Lisp_Cons | Lisp_Type::Lisp_Misc | Lisp_Type::Lisp_Vectorlike => {
                    match ht.lookup(self) {
                        Found(idx) => {
                            // `self' was seen already.
                            let o2s = ht.get_hash_value(idx);
                            if memq(other, o2s).is_nil() {
                                ht.set_hash_value(idx, LispObject::cons(other, o2s));
                            } else {
                                return true;
                            }
                        }
                        Missing(hash) => {
                            ht.put(self, LispObject::cons(other, Qnil), hash);
                        }
                    }
                }
                _ => {}
            }
        }

        if self.eq(other) {
            return true;
        }
        if self.get_type() != other.get_type() {
            return false;
        }

        match self.get_type() {
            Lisp_Type::Lisp_Int0 | Lisp_Type::Lisp_Int1 => {
                self.force_fixnum()
                    .equal(other.force_fixnum(), equal_kind, depth, ht)
            }
            Lisp_Type::Lisp_Symbol => {
                self.force_symbol()
                    .equal(other.force_symbol(), equal_kind, depth, ht)
            }
            Lisp_Type::Lisp_Cons => {
                self.force_cons()
                    .equal(other.force_cons(), equal_kind, depth, ht)
            }
            Lisp_Type::Lisp_Float => {
                self.force_floatref()
                    .equal(other.force_floatref(), equal_kind, depth, ht)
            }
            Lisp_Type::Lisp_Misc => {
                self.force_misc()
                    .equal(other.force_misc(), equal_kind, depth, ht)
            }
            Lisp_Type::Lisp_String => {
                self.force_string()
                    .equal(other.force_string(), equal_kind, depth, ht)
            }
            Lisp_Type::Lisp_Vectorlike => {
                self.force_vectorlike()
                    .equal(other.force_vectorlike(), equal_kind, depth, ht)
            }
        }
    }

    pub fn equal_no_quit(self, other: impl Into<LispObject>) -> bool {
        let mut ht = LispHashTableRef::empty();
        self.equal_internal(other.into(), equal_kind::EQUAL_NO_QUIT, 0, &mut ht)
    }

    pub fn is_function(self) -> bool {
        FUNCTIONP(self)
    }

    pub fn map_or<T>(self, default: T, action: impl FnOnce(LispObject) -> T) -> T {
        if self.is_nil() {
            default
        } else {
            action(self)
        }
    }

    pub fn map_or_else<T>(
        self,
        default: impl FnOnce() -> T,
        action: impl FnOnce(LispObject) -> T,
    ) -> T {
        if self.is_nil() {
            default()
        } else {
            action(self)
        }
    }
}

/// Used to denote functions that have no limit on the maximum number
/// of arguments.
pub const MANY: i16 = -2;

impl Display for LispObject {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", LispStringRef::from(*self))
    }
}

impl Debug for LispObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let ty = self.get_type();
        let self_ptr = &self as *const _;
        if ty as u8 >= 8 {
            return write!(
                f,
                "#<INVALID-OBJECT @ {:p}: VAL({:#X})>",
                self_ptr,
                self.to_C()
            );
        }
        if self.is_nil() {
            return write!(f, "nil");
        }
        match ty {
            Lisp_Type::Lisp_Symbol => write!(f, "{:?}", self.force_symbol()),
            Lisp_Type::Lisp_Cons => write!(f, "{:?}", self.force_cons()),
            Lisp_Type::Lisp_Float => write!(f, "{}", self.force_float()),
            Lisp_Type::Lisp_Vectorlike => write!(f, "{:?}", self.force_vectorlike()),
            Lisp_Type::Lisp_Int0 | Lisp_Type::Lisp_Int1 => write!(f, "{}", self.force_fixnum()),
            Lisp_Type::Lisp_Misc => write!(f, "{:?}", self.force_misc()),
            Lisp_Type::Lisp_String => write!(f, "{:?}", self.force_string()),
        }
    }
}

extern "C" {
    pub fn defsubr(sname: *const Lisp_Subr);
}

macro_rules! export_lisp_fns {
    ($($f:ident),+) => {
        pub fn rust_init_syms() {
            unsafe {
                $(
                    crate::lisp::defsubr(concat_idents!(S, $f).as_ptr());
                )+
            }
        }
    }
}

#[allow(unused_macros)]
macro_rules! protect_statics_from_GC {
    ($($f:ident),+) => {
        pub fn rust_static_syms() {
            unsafe {
                $(
                    crate::remacs_sys::staticpro(&$f as *const LispObject as *mut LispObject);
                )+
            }
        }
    }
}

#[test]
fn test_lisp_misc_any_size() {
    // Should be 32 bits, which is 4 bytes.
    assert!(mem::size_of::<Lisp_Misc_Any>() == 4);
}
