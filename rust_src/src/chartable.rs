//! char table related functions

use std::ptr;

use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{ChartabSize, Lisp_Char_Table, Lisp_Object, Lisp_Sub_Char_Table, Lisp_Type};
use remacs_sys::PSEUDOVECTOR_SIZE_MASK;
use remacs_sys::Qchar_code_property_table;
use remacs_sys::uniprop_table_uncompress;

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use std::mem;

pub type LispCharTableRef = ExternalPtr<Lisp_Char_Table>;
pub type LispSubCharTableRef = ExternalPtr<Lisp_Sub_Char_Table>;
pub struct LispSubCharTableAsciiRef(ExternalPtr<Lisp_Sub_Char_Table>);

fn chartab_idx(c: isize, depth: isize, min_char: isize) -> isize {
    // Number of characters (in bits) each element of Nth level char-table covers.
    let bits = match depth {
        0 => {
            (ChartabSize::Bits1 as isize) + (ChartabSize::Bits2 as isize)
                + (ChartabSize::Bits3 as isize)
        }
        1 => (ChartabSize::Bits2 as isize) + (ChartabSize::Bits3 as isize),
        2 => ChartabSize::Bits3 as isize,
        3 => 0,
        _ => {
            error!("Invalid char table depth");
        }
    };

    (c - min_char) >> bits
}

/// Nonzero iff OBJ is a string representing uniprop values of 128
/// succeeding characters (the bottom level of a char-table) by a
/// compressed format.  We are sure that no property value has a string
/// starting with '\001' nor '\002'.
fn uniprop_compressed_form_p(obj: LispObject) -> bool {
    match obj.as_string() {
        Some(s) => s.len_bytes() > 0 && (s.byte_at(0) == 1 || s.byte_at(0) == 2),
        None => false,
    }
}

impl LispCharTableRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)) }
    }

    pub fn is_uniprop(self) -> bool {
        self.purpose == Qchar_code_property_table && self.extra_slots() == 5
    }

    pub fn extra_slots(self) -> isize {
        (self.header.size & PSEUDOVECTOR_SIZE_MASK) - (1 << ChartabSize::Bits0 as isize)
    }

    pub fn get(self, c: isize) -> LispObject {
        let mut val = if is_ascii(c) {
            let tmp = LispObject::from_raw(self.ascii);
            if let Some(sub) = tmp.as_sub_char_table_ascii() {
                sub.get(c)
            } else {
                tmp
            }
        } else {
            let tmp = self.contents
                .get(chartab_idx(c, 0, 0) as usize)
                .map_or_else(
                    || error!("Index out of range"),
                    |tmp| LispObject::from_raw(*tmp),
                );
            if let Some(sub) = tmp.as_sub_char_table() {
                sub.get(c, self.is_uniprop())
            } else {
                tmp
            }
        };

        if val.is_nil() {
            val = LispObject::from_raw(self.default);
            if val.is_nil() {
                if let Some(parent) = LispObject::from_raw(self.parent).as_char_table() {
                    val = parent.get(c);
                }
            }
        }

        val
    }
}

impl LispSubCharTableAsciiRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(LispObject::tag_ptr(self.0, Lisp_Type::Lisp_Vectorlike)) }
    }

    fn _get(self, idx: isize) -> LispObject {
        let tmp = &self.0.contents as *const [Lisp_Object; 1] as *const LispObject;
        unsafe { ptr::read(tmp.offset(idx)) }
    }

    pub fn get(self, c: isize) -> LispObject {
        self._get(c)
    }
}

impl LispSubCharTableRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)) }
    }

    fn _get(self, idx: isize) -> LispObject {
        let tmp = &self.contents as *const [Lisp_Object; 1] as *const LispObject;
        unsafe { ptr::read(tmp.offset(idx)) }
    }

    pub fn get(self, c: isize, is_uniprop: bool) -> LispObject {
        let idx = chartab_idx(c, self.depth as isize, self.min_char as isize);

        let mut val = self._get(idx);

        if is_uniprop && uniprop_compressed_form_p(val) {
            val = LispObject::from_raw(unsafe {
                uniprop_table_uncompress(self.as_lisp_obj().to_raw(), idx as libc::c_int)
            });
        }

        if let Some(sub) = val.as_sub_char_table() {
            val = sub.get(c, is_uniprop)
        }

        val
    }
}

fn is_ascii(c: isize) -> bool {
    c < 128
}

/// Return the subtype of char-table CHARTABLE.  The value is a symbol.
#[lisp_fn]
pub fn char_table_subtype(chartable: LispCharTableRef) -> LispObject {
    LispObject::from_raw(chartable.purpose)
}

/// Return the parent char-table of CHARTABLE.
/// The value is either nil or another char-table.
/// If CHAR-TABLE holds nil for a given character,
/// then the actual applicable value is inherited from the parent char-table
/// (or from its parents, if necessary).
#[lisp_fn]
pub fn char_table_parent(chartable: LispCharTableRef) -> Option<LispCharTableRef> {
    LispObject::from_raw(chartable.parent).as_char_table()
}

/// Set the parent char-table of CHARTABLE to PARENT.
/// Return PARENT.  PARENT must be either nil or another char-table.
#[lisp_fn]
pub fn set_char_table_parent(
    mut chartable: LispCharTableRef,
    parent: Option<LispCharTableRef>,
) -> () {
    let mut temp = parent;
    while temp.is_some() {
        if let Some(p) = temp {
            if chartable.eq(&p) {
                error!("Attempt to make a chartable to be its own parent");
            }
            temp = char_table_parent(p);
        }
    }

    chartable.parent = if let Some(p) = parent {
        p.as_lisp_obj().to_raw()
    } else {
        LispObject::constant_nil().to_raw()
    };
    //parent
}

include!(concat!(env!("OUT_DIR"), "/chartable_exports.rs"));
