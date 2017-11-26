//! hashtable support

use libc::c_void;
use std::ptr;

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsDouble, EmacsInt, EmacsUint, Fcopy_sequence, Lisp_Hash_Table, PseudovecType,
                 Qhash_table_test, CHECK_IMPURE, INTMASK, Qkey, Qvalue, Qkey_or_value, Qkey_and_value};
use remacs_sys::{gc_aset, gc_asize, survives_gc_p, mark_object, hash_clear, hash_lookup, hash_put};

use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;
use lists::{list, put};

pub type LispHashTableRef = ExternalPtr<Lisp_Hash_Table>;

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
        LispObject::from(self.hash)
    }

    pub fn set_next(&mut self, next: LispObject) {
        self.next = next.to_raw();
    }

    pub fn get_next(&self) -> LispObject {
        LispObject::from(self.next)
    }

    pub fn set_index(&mut self, index: LispObject) {
        self.index = index.to_raw();
    }

    pub fn get_index(&self) -> LispObject {
        LispObject::from(self.index)
    }

    pub fn get_key_and_value(&self) -> LispObject {
        LispObject::from(self.key_and_value)
    }

    pub fn set_key_and_value(&mut self, key_and_value: LispObject) {
        self.key_and_value = key_and_value.to_raw();
    }

    pub fn get_weak(&self) -> LispObject {
        LispObject::from(self.weak)
    }

    #[inline]
    pub fn get_hash_value(self, idx: isize) -> LispObject {
        unsafe { self.get_key_and_value().aref(2 * idx + 1) }
    }

    #[inline]
    pub fn set_hash_value(self, idx: isize, value: LispObject) {
        unsafe { gc_aset(self.key_and_value, 2 * idx + 1, value.to_raw()) };
    }

    pub fn lookup(self, key: LispObject, hashptr: *mut EmacsUint) -> isize {
        let mutself = self.as_ptr() as *mut Lisp_Hash_Table;
        unsafe { hash_lookup(mutself, key.to_raw(), hashptr) }
    }

    pub fn put(mut self, key: LispObject, value: LispObject, hash: EmacsUint) -> isize {
        unsafe { hash_put(self.as_mut(), key.to_raw(), value.to_raw(), hash) }
    }

    pub fn check_impure(self, object: LispObject) {
        unsafe { CHECK_IMPURE(object.to_raw(), self.as_ptr() as *mut c_void) };
    }

    /// Remove the entry matching KEY from hash table, if there is one.
    pub fn remove(mut self, key: LispObject) {
        // This is calling a function pointer, if that isn't clear.
        let hash_code = (self.test.hashfn)(&mut self.test, key.to_raw());
        debug_assert!((hash_code & !(INTMASK as u64)) == 0);
        let index = unsafe { self.get_index().as_vector_unchecked() };
        let len = index.len() as EmacsUint;

        let start_of_bucket = (hash_code % len) as isize;
        let mut prev = -1;

        let mut i = self.get_index_slot(start_of_bucket);
        while 0 <= i {
            if key.eq(self.get_hash_key(i))
                || (self.test.cmpfn as *mut c_void != ptr::null_mut() && hash_code == unsafe {
                    self.get_hash_hash(i).as_natnum_unchecked()
                }
                    && (self.test.cmpfn)(
                        &mut self.test,
                        key.to_raw(),
                        self.get_hash_key(i).to_raw(),
                    )) {
                if prev < 0 {
                    self.set_index_slot(start_of_bucket, self.get_next_slot(i));
                } else {
                    self.set_next_slot(prev, self.get_next_slot(i));
                }

                self.set_hash_key(i, LispObject::constant_nil());
                self.set_hash_value(i, LispObject::constant_nil());
                self.set_hash_hash(i, LispObject::constant_nil());
                self.set_next_slot(i, self.next_free);
                self.next_free = i;
                self.count -= 1;
                debug_assert!(self.count >= 0);
                break;
            }

            prev = i;
            i = self.get_next_slot(i);
        }
    }

    fn get_next_slot(self, idx: isize) -> isize {
        unsafe { self.get_next().aref(idx).to_fixnum_unchecked() as isize }
    }

    fn set_next_slot(self, idx: isize, value: isize) {
        unsafe {
            gc_aset(
                self.next,
                idx,
                LispObject::from_fixnum(value as EmacsInt).to_raw(),
            )
        }
    }

    fn get_index_slot(self, idx: isize) -> isize {
        unsafe { self.get_index().aref(idx).to_fixnum_unchecked() as isize }
    }

    fn set_index_slot(self, idx: isize, value: isize) {
        unsafe {
            gc_aset(
                self.index,
                idx,
                LispObject::from_fixnum(value as EmacsInt).to_raw(),
            )
        }
    }


    pub fn get_hash_hash(self, idx: isize) -> LispObject {
        unsafe { self.get_hash().aref(idx) }
    }

    #[inline]
    pub fn set_hash_hash(self, idx: isize, hash: LispObject) {
        unsafe { gc_aset(self.hash, idx, hash.to_raw()) };
    }

    #[inline]
    pub fn get_hash_key(self, idx: isize) -> LispObject {
        unsafe { self.get_key_and_value().aref(2 * idx) }
    }

    #[inline]
    pub fn set_hash_key(self, idx: isize, key: LispObject) {
        unsafe { gc_aset(self.key_and_value, 2 * idx, key.to_raw()) };
    }

    #[inline]
    pub fn size(self) -> usize {
        unsafe { self.get_next().as_vector_unchecked().len() }
    }

    #[inline]
    pub fn clear(mut self) {
        unsafe { hash_clear(self.as_mut()) }
    }
}

/// An iterator used for iterating over the indices
/// of the `key_and_value` vector of a `Lisp_Hash_Table`.
/// Equivalent to a `for (i = 0; i < HASH_TABLE_SIZE(h); ++i)`
/// loop in the C layer.
pub struct HashTableIter<'a> {
    table: &'a LispHashTableRef,
    current: usize,
}

impl<'a> Iterator for HashTableIter<'a> {
    type Item = isize;

    fn next(&mut self) -> Option<isize> {
        if self.current < self.table.size() {
            let cur = self.current;
            self.current += 1;
            Some(cur as isize)
        } else {
            None
        }
    }
}

/// An iterator used for looping over the keys and values
/// contained in a `Lisp_Hash_Table`.
pub struct KeyAndValueIter<'a>(HashTableIter<'a>);

impl<'a> Iterator for KeyAndValueIter<'a> {
    type Item = (LispObject, LispObject);

    fn next(&mut self) -> Option<(LispObject, LispObject)> {
        while let Some(idx) = self.0.next() {
            let is_not_nil = self.0.table.get_hash_hash(idx).is_not_nil();
            if is_not_nil {
                let key = self.0.table.get_hash_key(idx);
                let value = self.0.table.get_hash_value(idx);
                return Some((key, value));
            }
        }

        None
    }
}

impl LispHashTableRef {
    pub fn indices(&self) -> HashTableIter {
        HashTableIter {
            table: self,
            current: 0,
        }
    }

    pub fn iter(&self) -> KeyAndValueIter {
        KeyAndValueIter(self.indices())
    }
}

/// Return a copy of hash table TABLE.
/// Keys and values are not copied, only the table itself is.
#[lisp_fn]
fn copy_hash_table(htable: LispObject) -> LispObject {
    let mut table = htable.as_hash_table_or_error();
    let mut new_table = LispHashTableRef::allocate();
    unsafe { new_table.copy(table) };
    debug_assert_ne!(new_table.as_ptr(), table.as_ptr());

    let key_and_value = LispObject::from(unsafe {
        Fcopy_sequence(new_table.get_key_and_value().to_raw())
    });
    let hash = LispObject::from(unsafe { Fcopy_sequence(new_table.get_hash().to_raw()) });
    let next = LispObject::from(unsafe { Fcopy_sequence(new_table.get_next().to_raw()) });
    let index = LispObject::from(unsafe { Fcopy_sequence(new_table.get_index().to_raw()) });
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

/// Look up KEY in TABLE and return its associated value.
/// If KEY is not found, return DFLT which defaults to nil.
#[lisp_fn(min = "2")]
fn gethash(key: LispObject, table: LispObject, dflt: LispObject) -> LispObject {
    let hash_table = table.as_hash_table_or_error();
    let idx = hash_table.lookup(key, ptr::null_mut());

    if idx >= 0 {
        hash_table.get_hash_value(idx)
    } else {
        dflt
    }
}

/// Associate KEY with VALUE in hash table TABLE.
/// If KEY is already present in table, replace its current value with
/// VALUE.  In any case, return VALUE.
#[lisp_fn]
fn puthash(key: LispObject, value: LispObject, table: LispObject) -> LispObject {
    let hash_table = table.as_hash_table_or_error();
    hash_table.check_impure(table);

    let mut hash: EmacsUint = 0;
    let idx = hash_table.lookup(key, &mut hash);

    if idx >= 0 {
        hash_table.set_hash_value(idx, value);
    } else {
        hash_table.put(key, value, hash);
    }

    value
}

/// Remove KEY from TABLE.
#[lisp_fn]
fn remhash(key: LispObject, table: LispObject) -> LispObject {
    let hash_table = table.as_hash_table_or_error();
    hash_table.check_impure(table);
    hash_table.remove(key);

    LispObject::constant_nil()
}

/// Call FUNCTION for all entries in hash table TABLE.
/// FUNCTION is called with two arguments, KEY and VALUE.
/// `maphash' always returns nil.
#[lisp_fn]
fn maphash(function: LispObject, table: LispObject) -> LispObject {
    let hash_table = table.as_hash_table_or_error();
    for (key, value) in hash_table.iter() {
        call!(function, key, value);
    }

    LispObject::constant_nil()
}

/// Return t if OBJ is a Lisp hash table object.
#[lisp_fn]
fn hash_table_p(obj: LispObject) -> LispObject {
    LispObject::from_bool(obj.is_hash_table())
}

/// Return the number of elements in TABLE.
#[lisp_fn]
fn hash_table_count(table: LispObject) -> LispObject {
    LispObject::from_natnum(table.as_hash_table_or_error().count as EmacsInt)
}

/// Return the current rehash threshold of TABLE.
#[lisp_fn]
fn hash_table_rehash_threshold(table: LispObject) -> LispObject {
    LispObject::from_float(table.as_hash_table_or_error().rehash_threshold as EmacsDouble)
}

/// Return the size of TABLE.
/// The size can be used as an argument to `make-hash-table' to create
/// a hash table than can hold as many elements as TABLE holds
/// without need for resizing.
#[lisp_fn]
fn hash_table_size(table: LispObject) -> LispObject {
    LispObject::from_natnum(table.as_hash_table_or_error().size() as EmacsInt)
}

/// Return the test TABLE uses.
#[lisp_fn]
fn hash_table_test(table: LispObject) -> LispObject {
    LispObject::from(table.as_hash_table_or_error().test.name)
}

/// Return the weakness of TABLE.
#[lisp_fn]
fn hash_table_weakness(table: LispObject) -> LispObject {
    table.as_hash_table_or_error().get_weak()
}

/// Clear hash table TABLE and return it.
#[lisp_fn]
fn clrhash(table: LispObject) -> LispObject {
    let hash_table = table.as_hash_table_or_error();
    hash_table.check_impure(table);
    hash_table.clear();
    table
}

/// Define a new hash table test with name NAME, a symbol.
///
/// In hash tables created with NAME specified as test, use TEST to
/// compare keys, and HASH for computing hash codes of keys.
///
/// TEST must be a function taking two arguments and returning non-nil if
/// both arguments are the same.  HASH must be a function taking one
/// argument and returning an object that is the hash code of the argument.
/// It should be the case that if (eq (funcall HASH x1) (funcall HASH x2))
/// returns nil, then (funcall TEST x1 x2) also returns nil.
#[lisp_fn]
fn define_hash_table_test(name: LispObject, test: LispObject, hash: LispObject) -> LispObject {
    let sym = LispObject::from(Qhash_table_test);
    put(name, sym, list(&mut [test, hash]))
}

/// Sweep weak hash table H.  REMOVE_ENTRIES_P means remove
/// entries from the table that don't survive the current GC.
/// !REMOVE_ENTRIES_P means mark entries that are in use.  Value is
/// true if anything was marked.
#[no_mangle]
pub extern "C" fn sweep_weak_table(h: *mut Lisp_Hash_Table, remove_entries_p: bool) -> bool {
    let mut table = LispHashTableRef::new(h);
    let n = unsafe { gc_asize(table.index) };
    let mut marked = false;

    for bucket in 0..n {
        let mut prev = -1;
        let mut next;
        let mut i = table.get_index_slot(bucket);
        while 0 <= i {
            let key_survives = unsafe { survives_gc_p(table.get_hash_key(i).to_raw()) };
            let value_survives = unsafe { survives_gc_p(table.get_hash_value(i).to_raw()) };
            let remove_p = match table.weak {
                Qkey => { !key_survives },
                Qvalue => { !value_survives },
                Qkey_or_value => { !(key_survives || value_survives) },
                Qkey_and_value => { !(key_survives && value_survives) },
                _ => { panic!() }
            };

            next = table.get_next_slot(i);

            if remove_entries_p {
                if remove_p {
                    if prev < 0 {
                        table.set_index_slot(bucket, next);
                    } else {
                        table.set_next_slot(prev, next);
                    }

                    table.set_next_slot(i, table.next_free);
                    table.next_free = i;

                    table.set_hash_key(i, LispObject::constant_nil());
                    table.set_hash_value(i, LispObject::constant_nil());
                    table.set_hash_hash(i, LispObject::constant_nil());

                    table.count -= 1;
                } else {
                    prev = i;
                }
            } else {
                if !remove_p {
                    if !key_survives {
                        unsafe { mark_object(table.get_hash_key(i).to_raw()) };
                        marked = true;
                    }

                    if !value_survives {
                        unsafe { mark_object(table.get_hash_value(i).to_raw()) };
                        marked = true;
                    }
                }
            }

            i = next;
        }

    }

    marked
}

include!(concat!(env!("OUT_DIR"), "/hashtable_exports.rs"));
