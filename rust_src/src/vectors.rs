//! Functions operating on vector(like)s, and general sequences.

use std::cmp::Ordering;
use std::mem;
use std::ptr;

use libc::ptrdiff_t;

use remacs_macros::lisp_fn;
use remacs_sys::{pvec_type, EmacsInt, Lisp_Bool_Vector, Lisp_Type, Lisp_Vector, Lisp_Vectorlike,
                 Lisp_Vectorlike_With_Slots, More_Lisp_Bits, BITS_PER_BITS_WORD,
                 MOST_POSITIVE_FIXNUM, PSEUDOVECTOR_FLAG};
use remacs_sys::Qsequencep;

use buffers::LispBufferRef;
use chartable::{LispCharTableRef, LispSubCharTableAsciiRef, LispSubCharTableRef};
use data::aref;
use frames::LispFrameRef;
use lisp::{ExternalPtr, LispObject, LispSubrRef};
use lisp::defsubr;
use lists::{inorder, nth, sort_list};
use multibyte::MAX_CHAR;
use process::LispProcessRef;
use threads::ThreadStateRef;
use windows::LispWindowRef;

pub type LispVectorlikeRef = ExternalPtr<Lisp_Vectorlike>;
pub type LispVectorRef = ExternalPtr<Lisp_Vector>;
pub type LispBoolVecRef = ExternalPtr<Lisp_Bool_Vector>;
pub type LispVectorlikeSlotsRef = ExternalPtr<Lisp_Vectorlike_With_Slots>;

impl LispVectorlikeRef {
    #[inline]
    pub fn is_vector(self) -> bool {
        self.header.size & (PSEUDOVECTOR_FLAG as isize) == 0
    }

    #[inline]
    pub fn as_vector(&self) -> Option<LispVectorRef> {
        if self.is_vector() {
            Some(unsafe { mem::transmute::<_, LispVectorRef>(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn as_vector_unchecked(&self) -> LispVectorRef {
        mem::transmute::<_, LispVectorRef>(*self)
    }

    #[inline]
    pub fn pseudovector_type(self) -> pvec_type {
        unsafe {
            mem::transmute(
                ((self.header.size & (More_Lisp_Bits::PVEC_TYPE_MASK as isize))
                    >> More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS) as i32,
            )
        }
    }

    #[inline]
    pub fn is_pseudovector(self, tp: pvec_type) -> bool {
        self.header.size & (PSEUDOVECTOR_FLAG | More_Lisp_Bits::PVEC_TYPE_MASK as usize) as isize
            == (PSEUDOVECTOR_FLAG | ((tp as usize) << More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS))
                as isize
    }

    #[inline]
    pub fn pseudovector_size(self) -> EmacsInt {
        (self.header.size & (More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as isize)) as EmacsInt
    }

    #[inline]
    pub fn as_bool_vector(&self) -> Option<LispBoolVecRef> {
        if self.is_pseudovector(pvec_type::PVEC_BOOL_VECTOR) {
            Some(unsafe { mem::transmute::<_, LispBoolVecRef>(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_buffer(&self) -> Option<LispBufferRef> {
        if self.is_pseudovector(pvec_type::PVEC_BUFFER) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_subr(&self) -> Option<LispSubrRef> {
        if self.is_pseudovector(pvec_type::PVEC_SUBR) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_window(&self) -> Option<LispWindowRef> {
        if self.is_pseudovector(pvec_type::PVEC_WINDOW) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_frame(&self) -> Option<LispFrameRef> {
        if self.is_pseudovector(pvec_type::PVEC_FRAME) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_process(&self) -> Option<LispProcessRef> {
        if self.is_pseudovector(pvec_type::PVEC_PROCESS) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_thread(&self) -> Option<ThreadStateRef> {
        if self.is_pseudovector(pvec_type::PVEC_THREAD) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_char_table(&self) -> Option<LispCharTableRef> {
        if self.is_pseudovector(pvec_type::PVEC_CHAR_TABLE) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    pub fn as_sub_char_table(&self) -> Option<LispSubCharTableRef> {
        if self.is_pseudovector(pvec_type::PVEC_SUB_CHAR_TABLE) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    pub fn as_sub_char_table_ascii(&self) -> Option<LispSubCharTableAsciiRef> {
        if self.is_pseudovector(pvec_type::PVEC_SUB_CHAR_TABLE) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_compiled(&self) -> Option<LispVectorlikeSlotsRef> {
        if self.is_pseudovector(pvec_type::PVEC_COMPILED) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }

    pub fn as_record(&self) -> Option<LispVectorlikeSlotsRef> {
        if self.is_pseudovector(pvec_type::PVEC_RECORD) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }
}

macro_rules! impl_vectorlike_ref {
    ($type:ident, $itertype:ident, $size_mask:expr) => {
        impl $type {
            #[inline]
            pub fn len(&self) -> usize {
                (self.header.size & ($size_mask as isize)) as usize
            }

            pub fn as_lisp_obj(self) -> LispObject {
                LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
            }

            #[inline]
            pub fn as_slice(&self) -> &[LispObject] {
                let l = self.len();
                unsafe {
                    &self.contents.as_slice(l)
                }
            }

            #[inline]
            pub fn as_mut_slice(&mut self) -> &mut [LispObject] {
                let l = self.len();
                unsafe {
                    self.contents.as_mut_slice(l)
                }
            }

            #[inline]
            pub fn get(&self, idx: usize) -> LispObject {
                assert!(idx < self.len());
                unsafe { self.get_unchecked(idx) }
            }

            #[inline]
            pub unsafe fn get_unchecked(&self, idx: usize) -> LispObject {
                self.as_slice()[idx]
            }

            #[inline]
            pub fn set(&mut self, idx: usize, item: LispObject) {
                assert!(idx < self.len());
                unsafe { self.set_unchecked(idx, item) };
            }

            pub fn set_checked(&mut self, idx: usize, item: LispObject) {
                if idx >= self.len() {
                    args_out_of_range!(self.as_lisp_obj(), LispObject::from(idx));
                }

                unsafe { self.set_unchecked(idx, item) };
            }

            #[inline]
            pub unsafe fn set_unchecked(&mut self, idx: usize, item: LispObject) {
                self.as_mut_slice()[idx] = item
            }

            pub fn iter(&self) -> $itertype {
                $itertype::new(self)
            }
        }

        pub struct $itertype<'a> {
            vec: &'a $type,
            cur: usize,
            rev: usize,
        }

        impl<'a> $itertype<'a> {
            pub fn new(vec: &'a $type) -> Self {
                Self {
                    vec: vec,
                    cur: 0,
                    rev: vec.len(),
                }
            }
        }

        impl<'a> Iterator for $itertype<'a> {
            type Item = LispObject;

            fn next(&mut self) -> Option<Self::Item> {
                if self.cur < self.rev {
                    let res = unsafe { self.vec.get_unchecked(self.cur) };
                    self.cur += 1;
                    Some(res)
                } else {
                    None
                }
            }
/*
            fn size_hint(&self) -> (usize, Option<usize>) {
                let remaining = (self.rev - self.cur) + 1;
                (remaining, Some(remaining))
            }
*/
        }

        impl<'a> DoubleEndedIterator for $itertype<'a> {
            fn next_back(&mut self) -> Option<Self::Item> {
                if self.rev > self.cur {
                    let res = unsafe { self.vec.get_unchecked(self.rev - 1) };
                    self.rev -= 1;
                    Some(res)
                } else {
                    None
                }
            }
        }

        impl<'a> ExactSizeIterator for $itertype<'a> {}
    }
}

impl_vectorlike_ref! { LispVectorRef, LispVecIterator, ptrdiff_t::max_value() }
impl_vectorlike_ref! { LispVectorlikeSlotsRef, LispVecSlotsIterator,
More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as isize }

impl LispBoolVecRef {
    pub fn as_lisp_obj(self) -> LispObject {
        LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
    }

    pub fn as_slice(&self) -> &[usize] {
        let l = self.len() / BITS_PER_BITS_WORD as usize + 1;
        unsafe { self.data.as_slice(l) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [usize] {
        let l = self.len() / BITS_PER_BITS_WORD as usize + 1;
        unsafe { self.data.as_mut_slice(l) }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.size as usize
    }

    #[inline]
    unsafe fn get_bit(&self, idx: usize) -> bool {
        let limb = self.as_slice()[idx / BITS_PER_BITS_WORD as usize];
        limb & (1 << (idx % BITS_PER_BITS_WORD as usize)) != 0
    }

    #[inline]
    pub fn get(&self, idx: usize) -> LispObject {
        assert!(idx < self.len());
        unsafe { self.get_unchecked(idx) }
    }

    pub unsafe fn get_unchecked(&self, idx: usize) -> LispObject {
        LispObject::from_bool(self.get_bit(idx))
    }

    pub fn set(&mut self, idx: usize, b: bool) {
        assert!(idx < self.len());
        unsafe { self.set_unchecked(idx, b) }
    }

    pub fn set_checked(&mut self, idx: usize, b: bool) {
        if idx >= self.len() {
            args_out_of_range!(self.as_lisp_obj(), LispObject::from(idx));
        }

        unsafe { self.set_unchecked(idx, b) }
    }

    pub unsafe fn set_unchecked(&mut self, idx: usize, b: bool) {
        let limb = self.as_mut_slice()
            .get_unchecked_mut(idx / BITS_PER_BITS_WORD as usize) as *mut usize;
        if b {
            *limb |= 1 << (idx % BITS_PER_BITS_WORD as usize)
        } else {
            *limb &= !(1 << (idx % BITS_PER_BITS_WORD as usize))
        }
    }

    pub fn iter(&self) -> LispBoolVecIterator {
        LispBoolVecIterator {
            bvec: self,
            limb: 0,
            cur: 0,
        }
    }
}

pub struct LispBoolVecIterator<'a> {
    bvec: &'a LispBoolVecRef,
    limb: usize,
    cur: usize,
}

impl<'a> Iterator for LispBoolVecIterator<'a> {
    type Item = LispObject;

    fn next(&mut self) -> Option<LispObject> {
        if self.cur >= self.bvec.len() {
            None
        } else {
            if self.cur % BITS_PER_BITS_WORD as usize == 0 {
                self.limb = self.bvec.get(self.cur).as_fixnum_or_error() as usize;
            }
            let res = LispObject::from_bool(
                self.limb & (1 << (self.cur % BITS_PER_BITS_WORD as usize)) != 0,
            );
            self.cur += 1;
            Some(res)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.bvec.len() - self.cur;
        (remaining, Some(remaining))
    }
}

impl<'a> ExactSizeIterator for LispBoolVecIterator<'a> {}

/// Return the length of vector, list or string SEQUENCE.
/// A byte-code function object is also allowed.
/// If the string contains multibyte characters, this is not necessarily
/// the number of bytes in the string; it is the number of characters.
/// To get the number of bytes, use `string-bytes'.
#[lisp_fn]
pub fn length(sequence: LispObject) -> LispObject {
    if let Some(s) = sequence.as_string() {
        LispObject::from(s.len_chars())
    } else if let Some(vl) = sequence.as_vectorlike() {
        if let Some(v) = vl.as_vector() {
            LispObject::from(v.len())
        } else if let Some(bv) = vl.as_bool_vector() {
            LispObject::from(bv.len())
        } else if vl.is_pseudovector(pvec_type::PVEC_CHAR_TABLE) {
            LispObject::from(EmacsInt::from(MAX_CHAR))
        } else if vl.is_pseudovector(pvec_type::PVEC_COMPILED)
            || vl.is_pseudovector(pvec_type::PVEC_RECORD)
        {
            LispObject::from(vl.pseudovector_size())
        } else {
            wrong_type!(Qsequencep, sequence);
        }
    } else if sequence.is_cons() {
        let len = sequence.iter_tails().count();
        if len > MOST_POSITIVE_FIXNUM as usize {
            error!("List too long");
        }
        LispObject::from(len)
    } else if sequence.is_nil() {
        LispObject::from(0)
    } else {
        wrong_type!(Qsequencep, sequence);
    }
}

/// Return element of SEQUENCE at index N.
#[lisp_fn]
pub fn elt(sequence: LispObject, n: EmacsInt) -> LispObject {
    if sequence.is_cons() || sequence.is_nil() {
        nth(n, sequence)
    } else if sequence.is_array() {
        aref(sequence, n)
    } else {
        wrong_type!(Qsequencep, sequence);
    }
}

/// Sort SEQ, stably, comparing elements using PREDICATE.
/// Returns the sorted sequence.  SEQ should be a list or vector.  SEQ is
/// modified by side effects.  PREDICATE is called with two elements of
/// SEQ, and should return non-nil if the first element should sort before
/// the second.
#[lisp_fn]
pub fn sort(seq: LispObject, predicate: LispObject) -> LispObject {
    if seq.is_cons() {
        sort_list(seq, predicate)
    } else if let Some(mut vec) = seq.as_vectorlike().and_then(|v| v.as_vector()) {
        vec.as_mut_slice().sort_by(|&a, &b| {
            // XXX: since the `sort' predicate is a two-outcome comparison
            // Less/!Less, and slice::sort_by() uses Greater/!Greater
            // (which is not guaranteed anyway), this requires two calls
            // instead of one in some cases.
            if !inorder(predicate, a, b) {
                Ordering::Greater
            } else if !inorder(predicate, b, a) {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        seq
    } else if seq.is_nil() {
        seq
    } else {
        wrong_type!(Qsequencep, seq)
    }
}

/// Return t if OBJECT is a vector.
#[lisp_fn]
pub fn vectorp(object: LispObject) -> bool {
    object.is_vector()
}

/// Return t if OBJECT is a char-table.
#[lisp_fn]
pub fn char_table_p(object: LispObject) -> bool {
    object.is_char_table()
}

/// Return t if OBJECT is a char-table or vector.
#[lisp_fn]
pub fn vector_or_char_table_p(object: LispObject) -> bool {
    object.is_vector() || object.is_char_table()
}

/// Return t if OBJECT is a bool-vector.
#[lisp_fn]
pub fn bool_vector_p(object: LispObject) -> bool {
    object.is_bool_vector()
}

/// Return t if OBJECT is an array (string or vector).
#[lisp_fn]
pub fn arrayp(object: LispObject) -> bool {
    object.is_array()
}

/// Return t if OBJECT is a sequence (list or array).
#[lisp_fn]
pub fn sequencep(object: LispObject) -> bool {
    object.is_sequence()
}

/// Return t if OBJECT is an editor buffer.
#[lisp_fn]
pub fn bufferp(object: LispObject) -> bool {
    object.is_buffer()
}

/// Return t if OBJECT is a built-in function.
#[lisp_fn]
pub fn subrp(object: LispObject) -> bool {
    object.is_subr()
}

/// Return t if OBJECT is a byte-compiled function object.
#[lisp_fn]
pub fn byte_code_function_p(object: LispObject) -> bool {
    object.is_byte_code_function()
}

/// Return t if OBJECT is a thread.
#[lisp_fn]
pub fn threadp(object: LispObject) -> bool {
    object.is_thread()
}

/// Return t if OBJECT is a mutex.
#[lisp_fn]
pub fn mutexp(object: LispObject) -> bool {
    object.is_mutex()
}

/// Return t if OBJECT is a condition variable.
#[lisp_fn]
pub fn condition_variable_p(object: LispObject) -> bool {
    object.is_condition_variable()
}

/// Return t if OBJECT is a record.
#[lisp_fn]
pub fn recordp(object: LispObject) -> bool {
    object.is_record()
}

lazy_static! {
    pub static ref HEADER_SIZE: usize = {
        unsafe { offset_of!(::remacs_sys::Lisp_Vector, contents) }
    };
    pub static ref WORD_SIZE: usize = {
        ::std::mem::size_of::<::lisp::LispObject>()
    };
}

include!(concat!(env!("OUT_DIR"), "/vectors_exports.rs"));
