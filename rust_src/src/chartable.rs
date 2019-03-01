//! char table related functions

use libc;

use remacs_macros::lisp_fn;

use crate::{
    hashtable::LispHashTableRef,
    lisp::{ExternalPtr, LispObject, LispStructuralEqual},
    remacs_sys::{
        char_table_specials, equal_kind, pvec_type, EmacsInt, Lisp_Char_Table, Lisp_Sub_Char_Table,
        Lisp_Type, More_Lisp_Bits, CHARTAB_SIZE_BITS,
    },
    remacs_sys::{uniprop_table_uncompress, CHAR_TABLE_SET},
    remacs_sys::{Qchar_code_property_table, Qchar_table_p},
    vectors::LispVectorlikeRef,
};

pub type LispCharTableRef = ExternalPtr<Lisp_Char_Table>;
pub type LispSubCharTableRef = ExternalPtr<Lisp_Sub_Char_Table>;
#[repr(transparent)]
pub struct LispSubCharTableAsciiRef(ExternalPtr<Lisp_Sub_Char_Table>);

impl LispObject {
    pub fn is_char_table(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_CHAR_TABLE))
    }

    pub fn as_char_table(self) -> Option<LispCharTableRef> {
        self.into()
    }
}

impl From<LispObject> for LispCharTableRef {
    fn from(o: LispObject) -> Self {
        if let Some(chartable) = o.as_char_table() {
            chartable
        } else {
            wrong_type!(Qchar_table_p, o)
        }
    }
}

impl From<LispObject> for Option<LispCharTableRef> {
    fn from(o: LispObject) -> Self {
        o.as_vectorlike().and_then(LispVectorlikeRef::as_char_table)
    }
}

impl From<LispCharTableRef> for LispObject {
    fn from(ct: LispCharTableRef) -> Self {
        LispObject::tag_ptr(ct, Lisp_Type::Lisp_Vectorlike)
    }
}

impl LispObject {
    pub fn as_sub_char_table(self) -> Option<LispSubCharTableRef> {
        self.as_vectorlike()
            .and_then(LispVectorlikeRef::as_sub_char_table)
    }

    pub fn as_sub_char_table_ascii(self) -> Option<LispSubCharTableAsciiRef> {
        self.as_vectorlike()
            .and_then(LispVectorlikeRef::as_sub_char_table_ascii)
    }
}

fn chartab_size(depth: i32) -> usize {
    match depth {
        0 => 1 << CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_0 as isize,
        1 => 1 << CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_1 as isize,
        2 => 1 << CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_2 as isize,
        3 => 1 << CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_3 as isize,
        _ => panic!("Invalid depth for chartab"),
    }
}

fn chartab_idx(c: isize, depth: i32, min_char: i32) -> usize {
    // Number of characters (in bits) each element of Nth level char-table covers.
    let bits = match depth {
        0 => {
            CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_1
                + CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_2
                + CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_3
        }
        1 => CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_3,
        2 => CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_3,
        3 => 0,
        _ => {
            error!("Invalid char table depth");
        }
    };

    ((c - min_char as isize) >> bits) as usize
}

/// Nonzero iff OBJ is a string representing uniprop values of 128
/// succeeding characters (the bottom level of a char-table) by a
/// compressed format.  We are sure that no property value has a string
/// starting with '\001' nor '\002'.
fn uniprop_compressed_form_p(obj: LispObject) -> bool {
    match obj.as_string() {
        Some(s) => !s.is_empty() && (s.byte_at(0) == 1 || s.byte_at(0) == 2),
        None => false,
    }
}

impl LispCharTableRef {
    pub fn is_uniprop(self) -> bool {
        self.purpose == Qchar_code_property_table && self.extra_slots() == 5
    }

    pub fn extra_slots(self) -> isize {
        (unsafe { self.header.size } & More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as isize)
            - (1 << CHARTAB_SIZE_BITS::CHARTAB_SIZE_BITS_0 as isize)
    }

    pub fn get(self, c: isize) -> LispObject {
        let mut val = if is_ascii(c) {
            let tmp = self.ascii;
            if let Some(sub) = tmp.as_sub_char_table_ascii() {
                sub.get(c)
            } else {
                tmp
            }
        } else {
            let tmp = self
                .contents
                .get(chartab_idx(c, 0, 0) as usize)
                .map_or_else(|| error!("Index out of range"), |tmp| *tmp);
            if let Some(sub) = tmp.as_sub_char_table() {
                sub.get(c, self.is_uniprop())
            } else {
                tmp
            }
        };

        if val.is_nil() {
            val = self.defalt; // sic
            if val.is_nil() {
                if let Some(parent) = self.parent.as_char_table() {
                    val = parent.get(c);
                }
            }
        }

        val
    }

    pub fn set(self, idx: isize, value: LispObject) {
        verify_lisp_type!(idx as EmacsInt, Qcharacterp);
        self.set_unchecked(idx, value);
    }

    pub fn set_unchecked(self, idx: isize, value: LispObject) {
        unsafe { CHAR_TABLE_SET(self.into(), idx as i32, value) };
    }
}

impl LispStructuralEqual for LispCharTableRef {
    fn equal(
        &self,
        other: Self,
        kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool {
        let mut size1 = (unsafe { self.header.size }
            & More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as isize) as usize;
        let size2 = (unsafe { other.header.size } & More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as isize)
            as usize;
        if size1 != size2 {
            return false;
        }

        let extras = if size1 > char_table_specials::CHAR_TABLE_STANDARD_SLOTS as usize {
            let tmp = size1 - char_table_specials::CHAR_TABLE_STANDARD_SLOTS as usize;
            size1 = char_table_specials::CHAR_TABLE_STANDARD_SLOTS as usize;
            tmp
        } else {
            0
        };

        // char table is 4 LispObjects + an array
        size1 -= 4;

        if !self
            .defalt
            .equal_internal(other.defalt, kind, depth + 1, ht)
        {
            return false;
        }
        if !self
            .parent
            .equal_internal(other.parent, kind, depth + 1, ht)
        {
            return false;
        }
        if !self
            .purpose
            .equal_internal(other.purpose, kind, depth + 1, ht)
        {
            return false;
        }
        if !self.ascii.equal_internal(other.ascii, kind, depth + 1, ht) {
            return false;
        }

        let all_equal = (0..size1).all(|i| {
            let v1 = self.contents[i];
            let v2 = other.contents[i];
            v1.equal_internal(v2, kind, depth + 1, ht)
        });
        if !all_equal {
            return false;
        }
        if extras == 0 {
            true
        } else {
            let self_extras = unsafe { self.extras.as_slice(extras) };
            let other_extras = unsafe { other.extras.as_slice(extras) };

            (0..extras).all(|i| {
                let v1 = self_extras[i];
                let v2 = other_extras[i];
                v1.equal_internal(v2, kind, depth + 1, ht)
            })
        }
    }
}

impl LispSubCharTableAsciiRef {
    fn _get(self, idx: usize) -> LispObject {
        self.0._get(idx)
    }

    pub fn get(self, c: isize) -> LispObject {
        let d = self.0.depth;
        let m = self.0.min_char;
        self._get(chartab_idx(c, d, m))
    }
}

impl LispStructuralEqual for LispSubCharTableAsciiRef {
    fn equal(
        &self,
        other: Self,
        kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool {
        self.0.equal(other.0, kind, depth, ht)
    }
}

impl From<LispSubCharTableAsciiRef> for LispObject {
    fn from(s: LispSubCharTableAsciiRef) -> Self {
        LispObject::tag_ptr(s.0, Lisp_Type::Lisp_Vectorlike)
    }
}

impl From<LispSubCharTableRef> for LispObject {
    fn from(s: LispSubCharTableRef) -> Self {
        LispObject::tag_ptr(s, Lisp_Type::Lisp_Vectorlike)
    }
}

impl LispSubCharTableRef {
    fn _get(self, idx: usize) -> LispObject {
        unsafe {
            let d = self.depth;
            self.contents.as_slice(chartab_size(d))[idx]
        }
    }

    pub fn get(self, c: isize, is_uniprop: bool) -> LispObject {
        let idx = chartab_idx(c, self.depth, self.min_char);

        let mut val = self._get(idx);

        if is_uniprop && uniprop_compressed_form_p(val) {
            val = unsafe { uniprop_table_uncompress(self.into(), idx as libc::c_int) };
        }

        if let Some(sub) = val.as_sub_char_table() {
            val = sub.get(c, is_uniprop)
        }

        val
    }
}

impl LispStructuralEqual for LispSubCharTableRef {
    fn equal(
        &self,
        other: Self,
        kind: equal_kind::Type,
        depth: i32,
        ht: &mut LispHashTableRef,
    ) -> bool {
        let mut size1 =
            unsafe { self.header.size as usize & More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as usize };
        let size2 =
            unsafe { other.header.size as usize & More_Lisp_Bits::PSEUDOVECTOR_SIZE_MASK as usize };
        if size1 != size2 {
            return false;
        }

        size1 -= 2; // account for depth and min_char
        if self.depth != other.depth {
            return false;
        }
        if self.min_char != other.min_char {
            return false;
        }

        let slice1 = unsafe { self.contents.as_slice(size1) };
        let slice2 = unsafe { other.contents.as_slice(size1) };
        (0..size1).all(|i| {
            let v1 = slice1[i];
            let v2 = slice2[i];
            v1.equal_internal(v2, kind, depth + 1, ht)
        })
    }
}

fn is_ascii(c: isize) -> bool {
    c < 128
}

/// Return the subtype of char-table CHARTABLE.  The value is a symbol.
#[lisp_fn]
pub fn char_table_subtype(chartable: LispCharTableRef) -> LispObject {
    chartable.purpose
}

/// Return the parent char-table of CHARTABLE.
/// The value is either nil or another char-table.
/// If CHAR-TABLE holds nil for a given character,
/// then the actual applicable value is inherited from the parent char-table
/// (or from its parents, if necessary).
#[lisp_fn]
pub fn char_table_parent(chartable: LispCharTableRef) -> Option<LispCharTableRef> {
    chartable.parent.as_char_table()
}

/// Set the parent char-table of CHARTABLE to PARENT.
/// Return PARENT.  PARENT must be either nil or another char-table.
#[lisp_fn]
pub fn set_char_table_parent(mut chartable: LispCharTableRef, parent: Option<LispCharTableRef>) {
    let mut temp = parent;
    while temp.is_some() {
        if let Some(p) = temp {
            if chartable.eq(&p) {
                error!("Attempt to make a chartable to be its own parent");
            }
            temp = char_table_parent(p);
        }
    }

    chartable.parent = parent.into();
    //parent
}

include!(concat!(env!("OUT_DIR"), "/chartable_exports.rs"));
