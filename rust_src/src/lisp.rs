//! This module contains Rust definitions whose C equivalents live in
//! lisp.h.

use libc::{c_char, c_void, intptr_t, uintptr_t};
use std::ffi::CString;

use std::convert::From;
use std::fmt::{Debug, Error, Formatter};
use std::mem;
use std::ops::{Deref, DerefMut};
use std::slice;

use remacs_sys;
use remacs_sys::{build_string, internal_equal, make_float};
use remacs_sys::{pvec_type, EmacsDouble, EmacsInt, EmacsUint, EqualKind, Lisp_Bits,
                 FONT_ENTITY_MAX, FONT_OBJECT_MAX, FONT_SPEC_MAX, USE_LSB_TAG, VALMASK};
use remacs_sys::{Lisp_Misc_Any, Lisp_Misc_Type, Lisp_Subr, Lisp_Type};
use remacs_sys::{Qautoload, Qchar_table_p, Qlistp, Qnil, Qsubrp, Qt, Qunbound, Vbuffer_alist};

use buffers::LispBufferRef;
use chartable::{LispCharTableRef, LispSubCharTableAsciiRef, LispSubCharTableRef};
use eval::FUNCTIONP;
use fonts::LispFontRef;
use lists::{list, CarIter};
use obarray::{check_obarray, LispObarrayRef};
use vectors::LispBoolVecRef;

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
pub struct LispObject(pub EmacsInt);

impl LispObject {
    pub fn from_C(n: EmacsInt) -> LispObject {
        LispObject(n)
    }

    pub fn from_C_unsigned(n: EmacsUint) -> LispObject {
        Self::from_C(n as EmacsInt)
    }

    pub fn to_C(self) -> EmacsInt {
        self.0
    }

    pub fn to_C_unsigned(self) -> EmacsUint {
        self.0 as EmacsUint
    }

    #[inline]
    pub fn constant_unbound() -> LispObject {
        Qunbound
    }

    #[inline]
    pub fn from_bool(v: bool) -> LispObject {
        if v {
            Qt
        } else {
            Qnil
        }
    }

    #[inline]
    pub fn from_float(v: EmacsDouble) -> LispObject {
        unsafe { make_float(v) }
    }
}

impl<T> From<Option<T>> for LispObject
where
    LispObject: From<T>,
{
    #[inline]
    fn from(v: Option<T>) -> Self {
        match v {
            None => Qnil,
            Some(v) => LispObject::from(v),
        }
    }
}

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

impl From<LispObject> for bool {
    #[inline]
    fn from(o: LispObject) -> Self {
        o.is_not_nil()
    }
}

impl From<bool> for LispObject {
    #[inline]
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

/// Copies a Rust str into a new Lisp string
impl<'a> From<&'a str> for LispObject {
    #[inline]
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

    #[inline]
    pub fn get_untaggedptr(self) -> *mut c_void {
        (self.to_C() & VALMASK) as intptr_t as *mut c_void
    }
}

// Obarray support
impl LispObject {
    pub fn as_obarray_or_error(self) -> LispObarrayRef {
        LispObarrayRef::new(check_obarray(self))
    }
}

impl From<LispObject> for LispObarrayRef {
    fn from(o: LispObject) -> LispObarrayRef {
        o.as_obarray_or_error()
    }
}

impl From<LispObject> for Option<LispObarrayRef> {
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.as_obarray_or_error())
        }
    }
}

// Misc support (LispType == Lisp_Misc == 1)

// Lisp_Misc is a union. Now we don't really care about its variants except the
// super type layout. LispMisc is an unsized type for this, and LispMiscAny is
// only the header and a padding, which is consistent with the c version.
// directly creating and moving or copying this struct is simply wrong!
// If needed, we can calculate all variants size and allocate properly.

#[repr(C)]
#[derive(Debug)]
pub struct ExternalPtr<T>(*mut T);

impl<T> Copy for ExternalPtr<T> {}

// Derive fails for this type so do it manually
impl<T> Clone for ExternalPtr<T> {
    fn clone(&self) -> Self {
        ExternalPtr::new(self.0)
    }
}

impl<T> ExternalPtr<T> {
    pub fn new(p: *mut T) -> ExternalPtr<T> {
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
    fn eq(&self, other: &ExternalPtr<T>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

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

pub type LispMiscRef = ExternalPtr<Lisp_Misc_Any>;

impl LispMiscRef {
    #[inline]
    pub fn get_type(self) -> Lisp_Misc_Type {
        self.type_()
    }
}

#[test]
fn test_lisp_misc_any_size() {
    // Should be 32 bits, which is 4 bytes.
    assert!(mem::size_of::<Lisp_Misc_Any>() == 4);
}

impl LispObject {
    #[inline]
    pub fn is_misc(self) -> bool {
        self.get_type() == Lisp_Type::Lisp_Misc
    }

    #[inline]
    pub fn as_misc(self) -> Option<LispMiscRef> {
        if self.is_misc() {
            unsafe { Some(self.to_misc_unchecked()) }
        } else {
            None
        }
    }

    unsafe fn to_misc_unchecked(self) -> LispMiscRef {
        LispMiscRef::new(self.get_untaggedptr() as *mut remacs_sys::Lisp_Misc_Any)
    }
}

impl From<LispObject> for EmacsInt {
    #[inline]
    fn from(o: LispObject) -> Self {
        o.as_fixnum_or_error()
    }
}

impl From<LispObject> for Option<EmacsInt> {
    #[inline]
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.as_fixnum_or_error())
        }
    }
}

impl From<LispObject> for EmacsUint {
    #[inline]
    fn from(o: LispObject) -> Self {
        o.as_natnum_or_error()
    }
}

impl From<LispObject> for Option<EmacsUint> {
    #[inline]
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.as_natnum_or_error())
        }
    }
}

impl From<EmacsInt> for LispObject {
    #[inline]
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

    pub fn is_subr(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_SUBR))
    }

    pub fn as_subr(self) -> Option<LispSubrRef> {
        self.as_vectorlike().and_then(|v| v.as_subr())
    }

    pub fn as_subr_or_error(self) -> LispSubrRef {
        self.as_subr().unwrap_or_else(|| wrong_type!(Qsubrp, self))
    }

    pub fn is_char_table(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_CHAR_TABLE))
    }

    pub fn as_char_table(self) -> Option<LispCharTableRef> {
        self.as_vectorlike().and_then(|v| v.as_char_table())
    }

    pub fn as_char_table_or_error(self) -> LispCharTableRef {
        if let Some(chartable) = self.as_char_table() {
            chartable
        } else {
            wrong_type!(Qchar_table_p, self)
        }
    }

    pub fn as_sub_char_table(self) -> Option<LispSubCharTableRef> {
        self.as_vectorlike().and_then(|v| v.as_sub_char_table())
    }

    pub fn as_sub_char_table_ascii(self) -> Option<LispSubCharTableAsciiRef> {
        self.as_vectorlike()
            .and_then(|v| v.as_sub_char_table_ascii())
    }

    pub fn is_bool_vector(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_BOOL_VECTOR))
    }

    pub fn as_bool_vector(self) -> Option<LispBoolVecRef> {
        self.as_vectorlike().and_then(|v| v.as_bool_vector())
    }

    pub fn is_array(self) -> bool {
        self.is_vector() || self.is_string() || self.is_char_table() || self.is_bool_vector()
    }

    pub fn is_sequence(self) -> bool {
        self.is_cons() || self.is_nil() || self.is_array()
    }

    pub fn is_font(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_FONT))
    }

    pub fn as_font(self) -> Option<LispFontRef> {
        self.as_vectorlike().and_then(|v| {
            if v.is_pseudovector(pvec_type::PVEC_FONT) {
                Some(LispFontRef::from_vectorlike(v))
            } else {
                None
            }
        })
    }

    pub fn is_font_entity(self) -> bool {
        self.is_font() && self.as_vectorlike().map_or(false, |vec| {
            vec.pseudovector_size() == EmacsInt::from(FONT_ENTITY_MAX)
        })
    }

    pub fn is_font_object(self) -> bool {
        self.is_font() && self.as_vectorlike().map_or(false, |vec| {
            vec.pseudovector_size() == EmacsInt::from(FONT_OBJECT_MAX)
        })
    }

    pub fn is_font_spec(self) -> bool {
        self.is_font() && self.as_vectorlike().map_or(false, |vec| {
            vec.pseudovector_size() == EmacsInt::from(FONT_SPEC_MAX)
        })
    }

    pub fn is_record(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_RECORD))
    }
}

impl From<LispObject> for LispCharTableRef {
    fn from(o: LispObject) -> Self {
        o.as_char_table_or_error()
    }
}

impl From<LispObject> for Option<LispCharTableRef> {
    fn from(o: LispObject) -> Self {
        o.as_char_table()
    }
}

impl From<LispCharTableRef> for LispObject {
    fn from(ct: LispCharTableRef) -> Self {
        ct.as_lisp_obj()
    }
}

impl From<LispObject> for LispSubrRef {
    #[inline]
    fn from(o: LispObject) -> Self {
        o.as_subr_or_error()
    }
}

/// From `FOR_EACH_ALIST_VALUE` in `lisp.h`
/// Implement `Iterator` over all values of `$data` yielding `$iter_item` type.
/// `$data` should be an `alist` and `$iter_item` type should implement `From<LispObject>`
macro_rules! impl_alistval_iter {
    ($iter_name:ident, $iter_item:ty, $data: expr) => {
        pub struct $iter_name {
            tails: CarIter,
        }

        impl $iter_name {
            pub fn new() -> Self {
                Self {
                    tails: CarIter::new($data, Some(Qlistp)),
                }
            }
        }

        impl Iterator for $iter_name {
            type Item = $iter_item;

            fn next(&mut self) -> Option<Self::Item> {
                self.tails
                    .next()
                    .and_then(|o| o.as_cons())
                    .map(|p| p.cdr())
                    .and_then(|q| q.into())
            }
        }
    };
}

impl_alistval_iter! {LiveBufferIter, LispBufferRef, unsafe { Vbuffer_alist }}

pub fn is_autoload(function: LispObject) -> bool {
    function
        .as_cons()
        .map_or(false, |cell| cell.car().eq(Qautoload))
}

// Other functions

impl LispObject {
    #[inline]
    pub fn is_nil(self) -> bool {
        self == Qnil
    }

    #[inline]
    pub fn is_not_nil(self) -> bool {
        self != Qnil
    }

    #[inline]
    pub fn is_t(self) -> bool {
        self == Qt
    }

    // The three Emacs Lisp comparison functions.

    #[inline]
    pub fn eq(self, other: LispObject) -> bool {
        self == other
    }

    #[inline]
    pub fn ne(self, other: LispObject) -> bool {
        self != other
    }

    #[inline]
    pub fn eql(self, other: LispObject) -> bool {
        if self.is_float() {
            self.equal_no_quit(other)
        } else {
            self.eq(other)
        }
    }

    #[inline]
    pub fn equal(self, other: LispObject) -> bool {
        unsafe { internal_equal(self, other, EqualKind::Plain, 0, Qnil) }
    }

    #[inline]
    pub fn equal_no_quit(self, other: LispObject) -> bool {
        unsafe { internal_equal(self, other, EqualKind::NoQuit, 0, Qnil) }
    }

    pub fn is_function(self) -> bool {
        FUNCTIONP(self)
    }
}

/// Used to denote functions that have no limit on the maximum number
/// of arguments.
pub const MANY: i16 = -2;

/// Internal function to get a displayable string out of a Lisp string.
fn display_string(obj: LispObject) -> String {
    let s = obj.as_string().unwrap();
    let slice = unsafe { slice::from_raw_parts(s.const_data_ptr(), s.len_bytes() as usize) };
    String::from_utf8_lossy(slice).into_owned()
}

impl Debug for LispObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let ty = self.get_type();
        let self_ptr = &self as *const _ as usize;
        if ty as u8 >= 8 {
            write!(
                f,
                "#<INVALID-OBJECT @ {:#X}: VAL({:#X})>",
                self_ptr,
                self.to_C()
            )?;
            return Ok(());
        }
        if self.is_nil() {
            return write!(f, "nil");
        }
        match ty {
            Lisp_Type::Lisp_Symbol => {
                let name = self.as_symbol_or_error().symbol_name();
                write!(f, "'{}", display_string(name))?;
            }
            Lisp_Type::Lisp_Cons => {
                let mut cdr = *self;
                write!(f, "'(")?;
                while let Some(cons) = cdr.as_cons() {
                    write!(f, "{:?} ", cons.car())?;
                    cdr = cons.cdr();
                }
                if cdr.is_nil() {
                    write!(f, ")")?;
                } else {
                    write!(f, ". {:?}", cdr)?;
                }
            }
            Lisp_Type::Lisp_Float => {
                write!(f, "{}", self.as_float().unwrap())?;
            }
            Lisp_Type::Lisp_Vectorlike => {
                let vl = self.as_vectorlike().unwrap();
                if vl.is_vector() {
                    write!(f, "[")?;
                    for el in vl.as_vector().unwrap().as_slice() {
                        write!(f, "{:?} ", el)?;
                    }
                    write!(f, "]")?;
                } else {
                    write!(
                        f,
                        "#<VECTOR-LIKE @ {:#X}: VAL({:#X})>",
                        self_ptr,
                        self.to_C()
                    )?;
                }
            }
            Lisp_Type::Lisp_Int0 | Lisp_Type::Lisp_Int1 => {
                write!(f, "{}", self.as_fixnum().unwrap())?;
            }
            Lisp_Type::Lisp_Misc => {
                write!(f, "#<MISC @ {:#X}: VAL({:#X})>", self_ptr, self.to_C())?;
            }
            Lisp_Type::Lisp_String => {
                write!(f, "{:?}", display_string(*self))?;
            }
        }
        Ok(())
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
                    defsubr(concat_idents!(S, $f).as_ptr());
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
                    ::remacs_sys::staticpro(&$f as *const LispObject as *mut LispObject);
                )+
            }
        }
    }
}
