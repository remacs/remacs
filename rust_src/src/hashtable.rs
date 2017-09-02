use remacs_macros::lisp_fn;
use lists;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::{PseudovecType, Lisp_Type, QCtest, Qeq, Qeql, Qequal, QCpurecopy, QCsize,
                 QCweakness, EmacsInt, Qhash_table_test, mark_object, mark_vectorlike,
                 Lisp_Vector, Qkey_and_value, Qkey, Qvalue, Qkey_or_value, pure_alloc,
                 survives_gc_p, Lisp_Vectorlike_Header, Lisp_Object, EmacsUint, hash_table_test,
                 ARRAY_MARK_FLAG, CHECK_IMPURE, hashfn_eq, hashfn_eql, hashfn_equal};
use std::ptr;
use fnv::FnvHashMap;
use std::mem;
use std::hash::{Hash, Hasher};
use libc::{c_void, c_int, ptrdiff_t, c_float};
use std::sync::Mutex;
use std::collections::hash_map::Entry::{Occupied, Vacant};

static DEFAULT_TABLE_SIZE: usize = 65;

pub type LispHashTableRef = ExternalPtr<LispHashTable>;

// @TODO now that this has been changed to not use a binary serializer,
// we can have the HashFunction use function pointers again, to avoid having to copy
// this enum across ALL lisp objects.
#[repr(C)]
#[derive(Eq, PartialEq, Copy, Clone)]
enum HashFunction {
    Eq,
    Eql,
    Equal,
    // name, cmp func, hash func
    UserFunc(LispObject, LispObject, LispObject),
}

#[repr(C)]
#[derive(Eq, PartialEq, Copy, Clone)]
enum HashState {
    PreCalculated(u64),
    NeedToCalculate,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct HashableLispObject {
    object: LispObject,
    func: HashFunction,
    state: HashState,
    idx: usize,
}

impl HashableLispObject {
    fn with_object_and_hashfn(o: LispObject, f: HashFunction) -> HashableLispObject {
        HashableLispObject {
            object: o,
            func: f,
            idx: 0,
            state: HashState::NeedToCalculate,
        }
    }

    fn precalculate_hash(&mut self) -> u64 {
        let hash = self.calculate_hash();
        self.set_hash(hash)
    }

    fn set_hash(&mut self, hash: u64) -> u64 {
        self.state = HashState::PreCalculated(hash);
        hash
    }
}

impl HashableLispObject {
    fn calculate_hash(&self) -> u64 {
        match self.func {
            HashFunction::Eq => unsafe { hashfn_eq(ptr::null_mut(), self.object.to_raw()) as u64 },
            HashFunction::Eql => unsafe {
                hashfn_eql(ptr::null_mut(), self.object.to_raw()) as u64
            },
            HashFunction::Equal => unsafe {
                hashfn_equal(ptr::null_mut(), self.object.to_raw()) as u64
            },
            HashFunction::UserFunc(_, _, hashfn) => {
                let result = call!(hashfn, self.object);
                unsafe { hashfn_eq(ptr::null_mut(), result.to_raw()) as u64 }
            }
        }
    }
}

impl Hash for HashableLispObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let hash = match self.state {
            HashState::PreCalculated(value) => value,
            HashState::NeedToCalculate => self.calculate_hash(),
        };

        state.write_u64(hash);
    }
}

impl PartialEq for HashableLispObject {
    fn eq(&self, other: &Self) -> bool {
        match self.func {
            HashFunction::Eq => self.object.eq(other.object),
            HashFunction::Eql => self.object.eql(other.object),
            HashFunction::Equal => self.object.equal(other.object),
            HashFunction::UserFunc(_, cmpfn, _) => {
                call!(cmpfn, self.object, other.object).is_not_nil()
            }
        }
    }
}

impl Eq for HashableLispObject {}

#[repr(C)]
#[derive(Copy, Clone)]
struct KeyAndValueEntry {
    key: LispObject,
    value: LispObject,
    hash: u64,
}

impl KeyAndValueEntry {
    fn new(key: LispObject, value: LispObject, hash: u64) -> KeyAndValueEntry {
        KeyAndValueEntry {
            key: key,
            value: value,
            hash: hash,
        }
    }

    fn nil() -> KeyAndValueEntry {
        Self::new(LispObject::constant_nil(), LispObject::constant_nil(), 0)
    }
}

#[derive(Clone)]
#[repr(C)]
pub struct LispHashTable {
    /// This is for Lisp; the hash table code does not refer to it.
    header: Lisp_Vectorlike_Header,

    /// Nil if table is non-weak.  Otherwise a symbol describing the weakness of the table.
    weak: LispObject,

    /// true if the table can be purecopied.  The table cannot be changed afterwards.
    is_pure: bool,

    /// The comparison and hash functions.
    func: HashFunction,

    /// The Hashmap used to store lisp objects.
    map: FnvHashMap<HashableLispObject, HashableLispObject>,

    /// A vector used for backwards compatability for fast lookups.
    key_and_value: Vec<KeyAndValueEntry>,

    /// Free list for available slots in the key_and_value vector.
    free_list: Vec<usize>,
}

impl LispHashTable {
    pub fn new() -> LispHashTable {
        Self::with_capacity(DEFAULT_TABLE_SIZE)
    }

    pub fn with_capacity(cap: usize) -> LispHashTable {
        LispHashTable {
            header: Lisp_Vectorlike_Header { size: 0 },
            weak: LispObject::constant_nil(),
            is_pure: false,
            func: HashFunction::Eql,
            map: FnvHashMap::with_capacity_and_hasher(cap, Default::default()),
            key_and_value: Vec::with_capacity(cap),
            free_list: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: LispObject, value: LispObject) -> ptrdiff_t {
        let mut hash_key = HashableLispObject::with_object_and_hashfn(key, self.func);
        let hash = hash_key.precalculate_hash();
        match self.map.entry(hash_key) {
            Occupied(mut entry) => {
                let mut hash_value = HashableLispObject::with_object_and_hashfn(value, self.func);
                let idx = entry.get().idx;
                self.key_and_value[idx] = KeyAndValueEntry::new(key, value, hash);
                hash_value.idx = idx;
                entry.insert(hash_value);
                idx as ptrdiff_t
            }

            Vacant(entry) => {
                let mut hash_value = HashableLispObject::with_object_and_hashfn(value, self.func);
                let retval;
                if let Some(idx) = self.free_list.pop() {
                    self.key_and_value[idx] = KeyAndValueEntry::new(key, value, hash);
                    retval = idx;
                } else {
                    self.key_and_value.push(
                        KeyAndValueEntry::new(key, value, hash),
                    );
                    retval = self.key_and_value.len() - 1;
                }

                hash_value.idx = retval;
                entry.insert(hash_value);
                retval as ptrdiff_t
            }
        }
    }

    fn remove_with_hashable(&mut self, hash_key: HashableLispObject) -> Option<LispObject> {
        let remove = self.map.remove(&hash_key);
        if let Some(result) = remove {
            self.clear_key_and_value(result.idx);
        }

        remove.map(|result| result.object)
    }

    pub fn remove(&mut self, key: LispObject) -> Option<LispObject> {
        let func = self.func;
        self.remove_with_hashable(HashableLispObject::with_object_and_hashfn(key, func))
    }

    fn clear_key_and_value(&mut self, idx: usize) {
        self.key_and_value[idx] = KeyAndValueEntry::nil();
        self.free_list.push(idx);
    }

    pub fn get_index(&self, key: LispObject) -> ptrdiff_t {
        let hash_key = HashableLispObject::with_object_and_hashfn(key, self.func);
        self.map.get(&hash_key).map_or(
            -1,
            |result| result.idx as ptrdiff_t,
        )
    }

    pub fn get(&self, key: LispObject) -> Option<LispObject> {
        let hash_key = HashableLispObject::with_object_and_hashfn(key, self.func);
        self.map.get(&hash_key).map(|result| result.object)
    }

    #[inline]
    pub fn get_value_with_index(&self, idx: usize) -> LispObject {
        self.key_and_value[idx].value
    }

    #[inline]
    pub fn get_key_with_index(&self, idx: usize) -> LispObject {
        self.key_and_value[idx].key
    }

    #[inline]
    pub fn set_value_with_index(&mut self, object: LispObject, idx: usize) {
        self.key_and_value[idx].value = object;
    }

    #[inline]
    pub fn set_key_with_index(&mut self, object: LispObject, idx: usize) {
        self.key_and_value[idx].key = object;
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.key_and_value.clear();
        self.free_list.clear();
    }
}

impl LispHashTableRef {
    pub fn is_not_pure_or_error(self, object: LispObject) {
        unsafe { CHECK_IMPURE(object.to_raw(), self.as_ptr() as *mut c_void) };
    }
}

macro_rules! add_weak_table {
    ($ident: ident) => {
        WEAK_TABLES.lock().unwrap().push($ident);
    }
}

#[allow(unused_unsafe)]
#[inline]
unsafe fn allocate_hashtable_unititalized() -> LispHashTableRef {
    ExternalPtr::new(allocate_pseudovector!(
        LispHashTable,
        is_pure,
        PseudovecType::PVEC_HASH_TABLE
    ))
}

fn allocate_hashtable() -> LispHashTableRef {
    let mut table = unsafe { allocate_hashtable_unititalized() };
    // The table is "almost" unititalized, but it has it's header
    // in the correct state. We will copy the header and reassign
    // post memcpy.
    let header = table.header.clone();
    let table_mem = LispHashTable::new();
    unsafe { ptr::copy_nonoverlapping(&table_mem, table.as_mut(), 1) };
    mem::forget(table_mem); // We will manually run drop when the GC cleans up this table.
    table.header = header;
    table
}

/// Create and return a new hash table.
///
/// Arguments are specified as keyword/argument pairs.  The following
/// arguments are defined:

/// :test TEST -- TEST must be a symbol that specifies how to compare
/// keys.  Default is `eql'.  Predefined are the tests `eq', `eql', and
/// `equal'.  User-supplied test and hash functions can be specified via
/// `define-hash-table-test'.

/// :size SIZE -- A hint as to how many elements will be put in the table.
/// Default is 65.

/// :rehash-size REHASH-SIZE - Indicates how to expand the table when it
/// fills up.  If REHASH-SIZE is an integer, increase the size by that
/// amount.  If it is a float, it must be > 1.0, and the new size is the
/// old size multiplied by that factor.  Default is 1.5.

/// :rehash-threshold THRESHOLD -- THRESHOLD must a float > 0, and <= 1.0.
/// Resize the hash table when the ratio (table entries / table size)
/// exceeds an approximation to THRESHOLD.  Default is 0.8125.

/// :weakness WEAK -- WEAK must be one of nil, t, `key', `value',
/// `key-or-value', or `key-and-value'.  If WEAK is not nil, the table
/// returned is a weak table.  Key/value pairs are removed from a weak
/// hash table when there are no non-weak references pointing to their
/// key, value, one of key or value, or both key and value, depending on
/// WEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK
/// is nil.

/// :purecopy PURECOPY -- If PURECOPY is non-nil, the table can be copied
/// to pure storage when Emacs is being dumped, making the contents of the
/// table read only. Any further changes to purified tables will result
/// in an error.

///usage: (make-hash-table &rest KEYWORD-ARGS)
#[lisp_fn]
fn make_hash_table(args: &mut [LispObject]) -> LispObject {
    let mut ptr = allocate_hashtable();
    let len = args.len();
    let mut i = 0;
    while i < len {
        if i + 1 > len {
            panic!("Inproper args list"); // @TODO make this a signal_error
        }

        let key = args[i];
        let value = args[i + 1];
        i += 2;
        if key.to_raw() == unsafe { QCtest } {
            if value.to_raw() == unsafe { Qeq } {
                ptr.func = HashFunction::Eq;
            } else if value.to_raw() == unsafe { Qeql } {
                ptr.func = HashFunction::Eql;
            } else if value.to_raw() == unsafe { Qequal } {
                ptr.func = HashFunction::Equal;
            } else {
                // Custom hash table test
                unsafe {
                    let prop = lists::get(value, LispObject::from_raw(Qhash_table_test));
                    if !prop.is_cons() || !prop.as_cons_unchecked().cdr().is_cons() {
                        panic!("Invalid hash table test"); // @TODO make this signal_erorr
                    }

                    let cons = prop.as_cons_unchecked();
                    let cdr = cons.cdr().as_cons_unchecked();
                    ptr.func = HashFunction::UserFunc(value, cons.car(), cdr.car());
                }
            }
        } else if key.to_raw() == unsafe { QCpurecopy } {
            ptr.is_pure = value.is_not_nil();
        } else if key.to_raw() == unsafe { QCsize } {
            let size = value.as_natnum_or_error() as usize;
            ptr.map.reserve(size);
        } else if key.to_raw() == unsafe { QCweakness } {
            ptr.weak = value;
            if value == LispObject::constant_t() {
                ptr.weak = unsafe { LispObject::from_raw(Qkey_and_value) };
            }

            // @TODO signal error or not Qkey/Qvalue/Qkey_or_value/Qkey_and_value
        }
    }

    if ptr.weak.is_not_nil() {
        add_weak_table!(ptr);
    }

    LispObject::from_hash_table(ptr)
}

/// Associate KEY with VALUE in hash table TABLE.
/// If KEY is already present in table, replace its current value with
/// VALUE.  In any case, return VALUE.
#[lisp_fn]
fn puthash(k: LispObject, v: LispObject, map: LispObject) -> LispObject {
    let mut hashmap = map.as_hash_table_or_error();
    hashmap.is_not_pure_or_error(map);
    hashmap.insert(k, v);
    v
}

/// Look up KEY in TABLE and return its associated value.
/// If KEY is not found, return DFLT which defaults to nil.
#[lisp_fn(min = "2")]
fn gethash(k: LispObject, map: LispObject, default: LispObject) -> LispObject {
    let hashmap = map.as_hash_table_or_error();
    hashmap.get(k).unwrap_or(default)
}

/// Remove KEY from TABLE.
#[lisp_fn]
fn remhash(k: LispObject, map: LispObject) -> LispObject {
    let mut hashmap = map.as_hash_table_or_error();
    hashmap.is_not_pure_or_error(map);
    hashmap.remove(k);
    LispObject::constant_nil()
}

/// Call FUNCTION for all entries in hash table TABLE.
/// FUNCTION is called with two arguments, KEY and VALUE.
/// `maphash' always returns nil.
#[lisp_fn]
fn maphash(function: LispObject, map: LispObject) -> LispObject {
    let table = map.as_hash_table_or_error();
    for (key, value) in table.map.iter() {
        call!(function, key.object, value.object);
    }

    LispObject::constant_nil()
}

/// Return the weakness of TABLE.
#[lisp_fn]
fn hash_table_weakness(map: LispObject) -> LispObject {
    map.as_hash_table_or_error().weak
}

/// Clear hash table TABLE and return it.
#[lisp_fn]
fn clrhash(map: LispObject) -> LispObject {
    let mut hashmap = map.as_hash_table_or_error();
    hashmap.is_not_pure_or_error(map);
    hashmap.clear();
    map
}

/// Return the number of elements in TABLE.
#[lisp_fn]
fn hash_table_count(map: LispObject) -> LispObject {
    let hashmap = map.as_hash_table_or_error();
    LispObject::from_natnum(hashmap.map.len() as EmacsInt)
}

/// Return the size of TABLE.
/// The size can be used as an argument to `make-hash-table' to create
/// a hash table than can hold as many elements as TABLE holds
/// without need for resizing.
#[lisp_fn]
fn hash_table_size(map: LispObject) -> LispObject {
    let table = map.as_hash_table_or_error();
    LispObject::from_natnum(table.map.capacity() as EmacsInt)
}

/// Return a copy of hash table TABLE.
/// Keys and values are not copied, only the table itself
#[lisp_fn]
fn copy_hash_table(table: LispObject) -> LispObject {
    let hashtable = table.as_hash_table_or_error();
    let mut new_ptr = unsafe { allocate_hashtable_unititalized() };
    let ref old_table = *hashtable;
    let new_table = old_table.clone();
    unsafe { ptr::copy_nonoverlapping(&new_table, new_ptr.as_mut(), 1) };
    mem::forget(new_table);

    if new_ptr.weak.is_not_nil() {
        add_weak_table!(new_ptr);
    }

    LispObject::from_hash_table(new_ptr)
}

/// Return the test TABLE uses.
#[lisp_fn]
fn hash_table_test(map: LispObject) -> LispObject {
    let hashmap = map.as_hash_table_or_error();
    match hashmap.func {
        HashFunction::Eq => unsafe { LispObject::from_raw(Qeq) },
        HashFunction::Eql => unsafe { LispObject::from_raw(Qeql) },
        HashFunction::Equal => unsafe { LispObject::from_raw(Qequal) },
        HashFunction::UserFunc(name, _, _) => name,
    }
}


// Remacs has dropped support for controlling rehash size and threshold,
// however for backwards compatability, we will define these functions, and return
// the default values defined in lisp.h

/// Return the current rehash size of TABLE.
/// NOTE: In remacs this always returns the default value of 0.5
#[lisp_fn]
fn hash_table_rehash_size(_map: LispObject) -> LispObject {
    LispObject::from_float(0.5)
}

/// Return the current rehash threshold of TABLE.
/// NOTE: In remacs this always returns the default value of 0.8125
#[lisp_fn]
fn hash_table_rehash_threshold(_map: LispObject) -> LispObject {
    LispObject::from_float(0.8125)
}

#[no_mangle]
pub unsafe extern "C" fn finalize_hashtable(map: *mut c_void) {
    ptr::drop_in_place(map as *mut LispHashTable);
}

#[no_mangle]
pub unsafe extern "C" fn mark_hashtable(map: *mut c_void) {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    mark_vectorlike(map as *mut Lisp_Vector);
    debug_assert!(ptr.header.size & ARRAY_MARK_FLAG != 0);

    if let HashFunction::UserFunc(name, cmp, hash) = ptr.func {
        mark_object(name.to_raw());
        mark_object(cmp.to_raw());
        mark_object(hash.to_raw());
    }

    if ptr.weak.is_nil() {
        for (key, value) in ptr.map.iter() {
            mark_object(key.object.to_raw());
            mark_object(value.object.to_raw());
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hash_lookup(
    map: *mut c_void,
    key: Lisp_Object,
    _: *mut c_void,
) -> ptrdiff_t {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.get_index(LispObject::from_raw(key))
}

#[no_mangle]
pub unsafe extern "C" fn hash_value_lookup(map: *mut c_void, idx: ptrdiff_t) -> Lisp_Object {
    debug_assert!(idx >= 0);
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.get_value_with_index(idx as usize).to_raw()
}

#[no_mangle]
pub unsafe extern "C" fn hash_key_lookup(map: *mut c_void, idx: ptrdiff_t) -> Lisp_Object {
    debug_assert!(idx >= 0);
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.get_key_with_index(idx as usize).to_raw()
}

#[no_mangle]
pub unsafe extern "C" fn hash_hash_lookup(map: *mut c_void, idx: ptrdiff_t) -> Lisp_Object {
    debug_assert!(idx >= 0);
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.get_key_with_index(idx as usize).to_raw()
}

#[no_mangle]
pub unsafe extern "C" fn hash_size(map: *mut c_void) -> ptrdiff_t {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.key_and_value.len() as ptrdiff_t
}

#[no_mangle]
pub unsafe extern "C" fn hash_count(map: *mut c_void) -> ptrdiff_t {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.map.len() as ptrdiff_t
}

#[no_mangle]
pub unsafe extern "C" fn hash_weakness(map: *mut c_void) -> Lisp_Object {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.weak.to_raw()
}

#[no_mangle]
pub unsafe extern "C" fn hash_test_name(map: *mut c_void) -> Lisp_Object {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    match ptr.func {
        HashFunction::Eq => Qeq,
        HashFunction::Eql => Qeql,
        HashFunction::Equal => Qequal,
        HashFunction::UserFunc(name, _, _) => name.to_raw(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hash_purity(map: *mut c_void) -> bool {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.is_pure
}

#[no_mangle]
pub unsafe extern "C" fn set_hash_value_slot(map: *mut c_void, idx: ptrdiff_t, value: Lisp_Object) {
    let mut ptr = ExternalPtr::new(map as *mut LispHashTable);
    debug_assert!(idx >= 0);
    ptr.set_value_with_index(LispObject::from_raw(value), idx as usize);
}

#[no_mangle]
pub unsafe extern "C" fn set_hash_key_slot(map: *mut c_void, idx: ptrdiff_t, value: Lisp_Object) {
    debug_assert!(idx >= 0);
    let mut ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.set_key_with_index(LispObject::from_raw(value), idx as usize);
}

#[no_mangle]
pub unsafe extern "C" fn hash_put(
    map: *mut c_void,
    key: Lisp_Object,
    value: Lisp_Object,
    _: EmacsUint,
) -> ptrdiff_t {
    let mut ptr = ExternalPtr::new(map as *mut LispHashTable);
    ptr.insert(LispObject::from_raw(key), LispObject::from_raw(value))
}

/// Peeks the free list of the current map, returns -1 if free list is empty.
#[no_mangle]
pub unsafe extern "C" fn hash_next_free(map: *mut c_void) -> ptrdiff_t {
    let mut ptr = ExternalPtr::new(map as *mut LispHashTable);
    if let Some(idx) = ptr.free_list.pop() {
        ptr.free_list.push(idx);
        idx as ptrdiff_t
    } else {
        -1
    }
}

#[allow(unused_unsafe)]
#[no_mangle]
pub unsafe extern "C" fn new_hash_table(
    test: hash_table_test,
    size: EmacsInt,
    _: c_float,
    _: c_float,
    weak: Lisp_Object,
    is_pure: bool,
) -> Lisp_Object {
    let mut ptr = allocate_hashtable();
    ptr.map.reserve(size as usize);
    ptr.weak = LispObject::from_raw(weak);
    ptr.is_pure = is_pure;
    if test.name == Qeq {
        ptr.func = HashFunction::Eq;
    } else if test.name == Qeql {
        ptr.func = HashFunction::Eql;
    } else if test.name == Qequal {
        ptr.func = HashFunction::Equal;
    } else {
        ptr.func = HashFunction::UserFunc(
            LispObject::from_raw(test.name),
            LispObject::from_raw(test.user_cmp_function),
            LispObject::from_raw(test.user_hash_function),
        );
    }

    LispObject::from_hash_table(ptr).to_raw()
}

#[no_mangle]
pub unsafe extern "C" fn table_not_weak_or_pure(table: *mut c_void) -> bool {
    let ptr = ExternalPtr::new(table as *mut LispHashTable);
    ptr.weak.is_not_nil() || !ptr.is_pure
}

// @TODO
#[no_mangle]
pub unsafe extern "C" fn get_key_and_value(_: *mut c_void) -> Lisp_Object {
    LispObject::constant_nil().to_raw()
}

unsafe impl Send for LispHashTableRef {}
lazy_static! {
    static ref WEAK_TABLES: Mutex<Vec<LispHashTableRef>> = {
        Mutex::new(Vec::new())
    };
}

/************************************************************************
			   Weak Hash Tables
 ************************************************************************/

/* Sweep weak hash table H.  REMOVE_ENTRIES_P means remove
   entries from the table that don't survive the current GC.
   !REMOVE_ENTRIES_P means mark entries that are in use.  Value is
   true if anything was marked.  */

unsafe extern "C" fn sweep_weak_hashtable(mut ptr: LispHashTableRef, remove_entries: bool) -> bool {
    let weakness = ptr.weak.to_raw();
    let mut to_remove = Vec::<HashableLispObject>::new();
    let mut marked = false;
    let len = ptr.key_and_value.len();
    let mut i = 0;

    while i < len {
        let entry = ptr.key_and_value[i];
        let key = entry.key;
        let value = entry.value;
        let key_survives_gc = survives_gc_p(key.to_raw());
        let value_survives_gc = survives_gc_p(value.to_raw());
        let remove_p;

        if weakness == Qkey {
            remove_p = !key_survives_gc;
        } else if weakness == Qvalue {
            remove_p = !value_survives_gc;
        } else if weakness == Qkey_or_value {
            remove_p = !(key_survives_gc || value_survives_gc);
        } else if weakness == Qkey_and_value {
            remove_p = !(key_survives_gc && value_survives_gc);
        } else {
            panic!();
        }

        if remove_entries {
            if remove_p {
                let mut object = HashableLispObject::with_object_and_hashfn(key, ptr.func);
                object.set_hash(entry.hash);
                to_remove.push(object);
            }
        } else {
            if !remove_p {
                if !key_survives_gc {
                    mark_object(key.to_raw());
                    marked = true;
                }

                if !value_survives_gc {
                    mark_object(value.to_raw());
                    marked = true;
                }
            }
        }

        i += 1;
    }

    for x in to_remove.iter() {
        ptr.remove_with_hashable(*x);
    }

    marked
}

// Programmer note: You CANNOT trigger a hash here, or in
// sweep_weak_table. This function happens during a GC, and
// calling 'sxhash' during a GC is not safe, as it could trigger
// us to hash a vector or string, which uses their size field to
// encode their GC mark bit. Meaning that their "size" will print as
// something like -9285893858, and will trigger all sorts of C horribleness
/// Remove elements from weak hash tables that don't survive the
/// current garbage collection.  Remove weak tables that don't survive
/// from Vweak_hash_tables.  Called from gc_sweep.
#[no_mangle]
pub unsafe extern "C" fn sweep_weak_hash_tables() {
    let mut marked;
    let mut tables = WEAK_TABLES.lock().unwrap();
    /* Mark all keys and values that are in use.  Keep on marking until
    there is no more change.  This is necessary for cases like
    value-weak table A containing an entry X -> Y, where Y is used in a
    key-weak table B, Z -> Y.  If B comes after A in the list of weak
    tables, X -> Y might be removed from A, although when looking at B
    one finds that it shouldn't.  */
    while {
        marked = false;
        for table in tables.iter() {
            if table.header.size & ARRAY_MARK_FLAG != 0 {
                marked = sweep_weak_hashtable(*table, false) || marked;
            }
        }

        marked
    }
    {} // This is basically a Rust "do while" loop,
    // by putting the logic into the while {condition} block.

    // @TODO this could be consolidated into a singular loop
    tables.retain(|x| x.header.size & ARRAY_MARK_FLAG != 0);
    for table in tables.iter_mut() {
        sweep_weak_hashtable(*table, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn purecopy_hash_table(map: *mut c_void) -> *mut c_void {
    let table_ptr = ExternalPtr::new(map as *mut LispHashTable);
    debug_assert!(table_ptr.is_pure);
    debug_assert!(table_ptr.weak.is_nil());

    let mut ptr = ExternalPtr::new(pure_alloc(
        mem::size_of::<LispHashTable>(),
        Lisp_Type::Lisp_Vectorlike as c_int,
    ) as *mut LispHashTable);

    if let HashFunction::UserFunc(name, cmp, hash) = table_ptr.func {
        ptr.func = HashFunction::UserFunc(name.purecopy(), cmp.purecopy(), hash.purecopy());
    } else {
        ptr.func = table_ptr.func;
    }

    ptr.header = table_ptr.header.clone();
    ptr.weak = LispObject::constant_nil().purecopy();
    ptr.is_pure = table_ptr.is_pure;
    ptr.map = FnvHashMap::with_capacity_and_hasher(table_ptr.map.len(), Default::default());
    ptr.key_and_value = Vec::new();
    ptr.free_list = Vec::new();

    for (key, value) in table_ptr.map.iter() {
        ptr.insert(key.object.purecopy(), value.object.purecopy());
    }

    ptr.as_ptr() as *mut c_void
}
