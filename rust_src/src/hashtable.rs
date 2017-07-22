use remacs_macros::lisp_fn;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::{Lisp_Hash_Table, PseudovecType, Fcopy_sequence};
use std::ptr;

pub type LispHashTableRef = ExternalPtr<Lisp_Hash_Table>;

impl LispHashTableRef {
    pub fn allocate() -> LispHashTableRef {
        let vec_ptr =
            allocate_pseudovector!(Lisp_Hash_Table, count, PseudovecType::PVEC_HASH_TABLE);
        LispHashTableRef::new(vec_ptr)
    }

    pub fn copy(&mut self, other: LispHashTableRef) {
        unsafe {
            ptr::copy_nonoverlapping(other.as_ptr(), self.as_mut(), 1);
        };
    }

    pub fn set_next_weak(&mut self, other: LispHashTableRef) {
        self.next_weak = other.as_ptr() as *mut Lisp_Hash_Table;
    }

    pub fn get_next_weak(&self) -> LispHashTableRef {
        LispHashTableRef::new(self.next_weak)
    }

    pub fn set_hash(&mut self, hash: LispObject) {
        self.hash = hash.to_raw();
    }

    pub fn get_hash(&self) -> LispObject {
        LispObject::from_raw(self.hash)
    }

    pub fn set_next(&mut self, next: LispObject) {
        self.next = next.to_raw();
    }

    pub fn get_next(&self) -> LispObject {
        LispObject::from_raw(self.next)
    }

    pub fn set_index(&mut self, index: LispObject) {
        self.index = index.to_raw();
    }

    pub fn get_index(&self) -> LispObject {
        LispObject::from_raw(self.index)
    }
}

/// Return a copy of hash table TABLE.
#[lisp_fn]
fn copy_hash_table(htable: LispObject) -> LispObject {
    let mut table = htable.as_hash_table_or_error();
    let mut vec = LispHashTableRef::allocate();
    vec.copy(table);

    let hash = LispObject::from_raw(unsafe { Fcopy_sequence(vec.get_hash().to_raw()) });
    let next = LispObject::from_raw(unsafe { Fcopy_sequence(vec.get_next().to_raw()) });
    let index = LispObject::from_raw(unsafe { Fcopy_sequence(vec.get_index().to_raw()) });
    vec.set_hash(hash);
    vec.set_next(next);
    vec.set_index(index);

    let returnval = LispObject::from_hash_table(vec);
    if returnval.is_not_nil() {
        vec.set_next_weak(table.get_next_weak());
        table.set_next_weak(vec);
    }

    returnval
}
