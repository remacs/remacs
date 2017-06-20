//! Functions operating on vector(like)s.

use std::mem;
use std::ptr;
use std::slice;
use std::cmp::Ordering;

use libc::ptrdiff_t;

use lisp::{ExternalPtr, LispObject, MOST_POSITIVE_FIXNUM};
use multibyte::MAX_CHAR;
use lists::{sort_list, inorder};
use buffers::LispBufferRef;
use remacs_sys::{Lisp_Object, Qsequencep, EmacsInt, wrong_type_argument, error, bits_word,
                 PSEUDOVECTOR_FLAG, PVEC_TYPE_MASK, PSEUDOVECTOR_AREA_BITS,
                 PSEUDOVECTOR_SIZE_MASK, PseudovecType};
use remacs_macros::lisp_fn;

/* The only field contains various pieces of information:
- The MSB (ARRAY_MARK_FLAG) holds the gcmarkbit.
- The next bit (PSEUDOVECTOR_FLAG) indicates whether this is a plain
  vector (0) or a pseudovector (1).
- If PSEUDOVECTOR_FLAG is 0, the rest holds the size (number
  of slots) of the vector.
- If PSEUDOVECTOR_FLAG is 1, the rest is subdivided into three fields:
  - a) pseudovector subtype held in PVEC_TYPE_MASK field;
  - b) number of Lisp_Objects slots at the beginning of the object
    held in PSEUDOVECTOR_SIZE_MASK field.  These objects are always
    traced by the GC;
  - c) size of the rest fields held in PSEUDOVECTOR_REST_MASK and
    measured in word_size units.  Rest fields may also include
    Lisp_Objects, but these objects usually needs some special treatment
    during GC.
  There are some exceptions.  For PVEC_FREE, b) is always zero.  For
  PVEC_BOOL_VECTOR and PVEC_SUBR, both b) and c) are always zero.
  Current layout limits the pseudovectors to 63 PVEC_xxx subtypes,
  4095 Lisp_Objects in GC-ed area and 4095 word-sized other slots.  */

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct Lisp_Vectorlike_Header {
    size: ptrdiff_t,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct Lisp_Vectorlike {
    header: Lisp_Vectorlike_Header,
    // shouldn't look at the contents without knowing the structure...
}

pub type LispVectorlikeRef = ExternalPtr<Lisp_Vectorlike>;

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct Lisp_Vector {
    header: Lisp_Vectorlike_Header,
    // actually any number of items... not sure how to express this
    contents: [Lisp_Object; 1],
}

pub type LispVectorRef = ExternalPtr<Lisp_Vector>;

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct Lisp_Bool_Vector {
    _header: Lisp_Vectorlike_Header,
    size: EmacsInt,
    // actually any number of items again
    _data: [bits_word; 1],
}

pub type LispBoolVecRef = ExternalPtr<Lisp_Bool_Vector>;

impl LispVectorlikeRef {
    #[inline]
    pub fn is_vector(&self) -> bool {
        self.header.size & PSEUDOVECTOR_FLAG == 0
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
    pub fn is_pseudovector(&self, tp: PseudovecType) -> bool {
        self.header.size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK) ==
            (PSEUDOVECTOR_FLAG | ((tp as isize) << PSEUDOVECTOR_AREA_BITS))
    }

    #[inline]
    pub fn as_bool_vector(&self) -> Option<LispBoolVecRef> {
        if self.is_pseudovector(PseudovecType::PVEC_BOOL_VECTOR) {
            Some(unsafe { mem::transmute::<_, LispBoolVecRef>(*self) })
        } else {
            None
        }
    }

    #[inline]
    pub fn as_buffer(&self) -> Option<LispBufferRef> {
        if self.is_pseudovector(PseudovecType::PVEC_BUFFER) {
            Some(unsafe { mem::transmute(*self) })
        } else {
            None
        }
    }
}

impl LispVectorRef {
    #[inline]
    pub fn len(&self) -> usize {
        self.header.size as usize
    }

    #[inline]
    pub fn as_slice(&self) -> &[LispObject] {
        unsafe {
            slice::from_raw_parts(
                mem::transmute::<_, *const LispObject>(&self.contents),
                self.len(),
            )
        }
    }

    #[inline]
    pub fn as_mut_slice(&self) -> &mut [LispObject] {
        unsafe {
            slice::from_raw_parts_mut(
                mem::transmute::<_, *mut LispObject>(&self.contents),
                self.len(),
            )
        }
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, idx: ptrdiff_t) -> LispObject {
        ptr::read(
            mem::transmute::<_, *const LispObject>(&self.contents).offset(idx),
        )
    }

    #[inline]
    pub fn get(&self, idx: ptrdiff_t) -> LispObject {
        assert!(0 <= idx && idx < self.len() as ptrdiff_t);
        unsafe { self.get_unchecked(idx) }
    }
}

impl LispBoolVecRef {
    pub fn len(&self) -> usize {
        self.size as usize
    }
}

/// Return the length of vector, list or string SEQUENCE.
/// A byte-code function object is also allowed.
/// If the string contains multibyte characters, this is not necessarily
/// the number of bytes in the string; it is the number of characters.
/// To get the number of bytes, use `string-bytes'.
#[lisp_fn]
fn length(sequence: LispObject) -> LispObject {
    if let Some(s) = sequence.as_string() {
        return LispObject::from_natnum(s.len_chars() as EmacsInt);
    } else if let Some(vl) = sequence.as_vectorlike() {
        if let Some(v) = vl.as_vector() {
            return LispObject::from_natnum(v.len() as EmacsInt);
        } else if let Some(bv) = vl.as_bool_vector() {
            return LispObject::from_natnum(bv.len() as EmacsInt);
        } else if vl.is_pseudovector(PseudovecType::PVEC_CHAR_TABLE) {
            return LispObject::from_natnum(MAX_CHAR as EmacsInt);
        } else if vl.is_pseudovector(PseudovecType::PVEC_COMPILED) {
            return LispObject::from_natnum((vl.header.size & PSEUDOVECTOR_SIZE_MASK) as EmacsInt);
        }
    } else if let Some(_) = sequence.as_cons() {
        let len = sequence.iter_tails().count();
        if len > MOST_POSITIVE_FIXNUM as usize {
            unsafe {
                error("List too long\0".as_ptr());
            }
        }
        return LispObject::from_natnum(len as EmacsInt);
    } else if sequence.is_nil() {
        return LispObject::from_natnum(0);
    }
    unsafe { wrong_type_argument(Qsequencep, sequence.to_raw()) }
}

/// Sort SEQ, stably, comparing elements using PREDICATE.
/// Returns the sorted sequence.  SEQ should be a list or vector.  SEQ is
/// modified by side effects.  PREDICATE is called with two elements of
/// SEQ, and should return non-nil if the first element should sort before
/// the second.
#[lisp_fn]
fn sort(seq: LispObject, predicate: LispObject) -> LispObject {
    if seq.is_cons() {
        sort_list(seq, predicate)
    } else if let Some(vec) = seq.as_vectorlike().and_then(|v| v.as_vector()) {
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
        unsafe { wrong_type_argument(Qsequencep, seq.to_raw()) }
    }
}

/// Return t if OBJECT is a vector.
#[lisp_fn]
fn vectorp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_vector())
}
