use remacs_macros::lisp_fn;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::{Lisp_Hash_Table, PseudovecType, Fcopy_sequence, ARRAY_MARK_FLAG, Lisp_Vectorlike_Header, Lisp_Type};
use std::ptr;
use std::collections::HashMap;
use alloc::{GCObject, LispGarbageCollector};

pub type LispHashTableRef = ExternalPtr<Lisp_Hash_Table>;

#[allow(dead_code)] // @TODO remove
struct HashTableTest {
    name: LispObject,
    user_hash_function: LispObject,
    user_comp_function: LispObject,
}

impl HashTableTest {
    fn new() -> HashTableTest {
        HashTableTest {
            name: LispObject::constant_nil(),
            user_hash_function: LispObject::constant_nil(),
            user_comp_function: LispObject::constant_nil(),
        }
    }
}

#[allow(dead_code)] // @TODO remove
pub struct LispHashTable {
    header: Lisp_Vectorlike_Header,
    weak: LispObject,
    is_pure: bool,
    table_test: HashTableTest,
    map: HashMap<LispObject, LispObject>, // @TODO implement a custom hasher here for lisp objects.
}

// @TODO make this a #[derive(MarkVectorlike)] since this will be a common impl of this trait
impl GCObject for LispHashTable {
    #[inline]
    fn mark(&mut self) {
        self.header.size = self.header.size | ARRAY_MARK_FLAG;
    }

    #[inline]
    fn unmark(&mut self) {
        self.header.size = self.header.size & !ARRAY_MARK_FLAG;
    }

    #[inline]
    fn is_marked(&self) -> bool {
        self.header.size & ARRAY_MARK_FLAG != 0
    }
}

impl LispHashTable {
    pub fn new() -> LispHashTable {
        LispHashTable {
            header: Lisp_Vectorlike_Header { size: 0 },
            weak: LispObject::constant_nil(),
            is_pure: false,
            table_test: HashTableTest::new(),
            map: HashMap::new(),
        }
    }
}

impl LispHashTableRef {
    pub fn allocate() -> LispHashTableRef {
        let vec_ptr =
            allocate_pseudovector!(Lisp_Hash_Table, count, PseudovecType::PVEC_HASH_TABLE);
        LispHashTableRef::new(vec_ptr)
    }

    pub unsafe fn copy(&mut self, other: LispHashTableRef) {
        ptr::copy_nonoverlapping(other.as_ptr(), self.as_mut(), 1);
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

    pub fn get_key_and_value(&self) -> LispObject {
        LispObject::from_raw(self.key_and_value)
    }

    pub fn set_key_and_value(&mut self, key_and_value: LispObject) {
        self.key_and_value = key_and_value.to_raw();
    }

    pub fn get_weak(&self) -> LispObject {
        LispObject::from_raw(self.weak)
    }
}

/// Return a copy of hash table TABLE.
/// Keys and values are not copied, only the table itself is.
#[lisp_fn]
fn copy_hash_table(htable: LispObject) -> LispObject {
    let mut table = htable.as_hash_table_or_error();
    let mut new_table = LispHashTableRef::allocate();
    unsafe { new_table.copy(table) };
    assert!(new_table.as_ptr() != table.as_ptr());

    let key_and_value = LispObject::from_raw(unsafe {
        Fcopy_sequence(new_table.get_key_and_value().to_raw())
    });
    let hash = LispObject::from_raw(unsafe { Fcopy_sequence(new_table.get_hash().to_raw()) });
    let next = LispObject::from_raw(unsafe { Fcopy_sequence(new_table.get_next().to_raw()) });
    let index = LispObject::from_raw(unsafe { Fcopy_sequence(new_table.get_index().to_raw()) });
    new_table.set_key_and_value(key_and_value);
    new_table.set_hash(hash);
    new_table.set_next(next);
    new_table.set_index(index);

    if new_table.get_weak().is_not_nil() {
        new_table.set_next_weak(table.get_next_weak());
        table.set_next_weak(new_table);
    }

    LispObject::from_hash_table(new_table)
}

#[lisp_fn]
fn make_hash_map() -> LispObject {
    let hashmap = LispHashTable::new();
    let ptr = LispGarbageCollector::manage(hashmap);
    LispObject::tag_ptr(ptr, Lisp_Type::Lisp_Vectorlike)
}
