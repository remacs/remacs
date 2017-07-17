use lisp::{LispObject, ExternalPtr};
use remacs_sys::{Lisp_Hash_Table, PseudovecType};
use std::mem;

pub type LispHashTableRef = ExternalPtr<Lisp_Hash_Table>;

impl LispHashTableRef {
    pub fn allocate() -> LispHashTableRef {
        let vec_ptr =
            allocate_pseudovector!(Lisp_Hash_Table, count, PseudovecType::PVEC_HASH_TABLE);
        LispHashTableRef::new(unsafe { mem::transmute(vec_ptr) })
    }

    pub fn copy(&mut self, other: &LispHashTableRef) {
        *self = *other
    }

    pub fn set_next_weak(&mut self, other: &mut LispHashTableRef) {
        self.next_weak = other.as_mut();
    }

    pub fn get_next_weak(&self) -> LispHashTableRef {
        LispHashTableRef::new(unsafe { mem::transmute(self.next_weak) })
    }
}

#[allow(dead_code)] // @TODO remove once this function is hooked up.
fn copy_hash_table(htable: LispObject) -> LispObject {
    let mut table = htable.as_hash_table_or_error();
    let mut vec = LispHashTableRef::allocate();
    vec.copy(&table);
    // @TODO call Fcopy_sequence or equiv etc.
    // ...
    // ...
    let returnval = LispObject::from_hash_table(&vec);
    if returnval.is_not_nil() {
        vec.set_next_weak(&mut table.get_next_weak());
        table.set_next_weak(&mut vec);
    }

    returnval
}
