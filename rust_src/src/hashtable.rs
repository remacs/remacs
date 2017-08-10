use remacs_macros::lisp_fn;
use lists;
use lisp::{LispObject, ExternalPtr};
use remacs_sys::{PseudovecType, Lisp_Type, QCtest, Qeq, Qeql,
                 Qequal, QCpurecopy, QCsize, QCweakness, sxhash, EmacsInt, Qhash_table_test,
                 mark_object, mark_vectorlike, Lisp_Vector, Qkey_and_value, Qkey, Qvalue,
                 Qkey_or_value, pure_alloc, survives_gc, Lisp_Vectorlike_Header};
use std::ptr;
use fnv::FnvHashMap;
use std::mem;
use std::hash::{Hash, Hasher};
use libc::{c_void, c_int};

static DEFAULT_TABLE_SIZE: usize = 65;

pub type LispHashTableRef = ExternalPtr<LispHashTable>;

// @TODO now that this has been changed to not use a binary serializer,
// we can have the HashFunction use function pointers again, to avoid having to copy
// this enum across ALL lisp objects.
#[derive(Eq, PartialEq, Copy, Clone)]
enum HashFunction {
    Eq,
    Eql,
    Equal,
    UserFunc(LispObject, LispObject, LispObject),
}

#[derive(Clone)]
struct HashableLispObject {
    object: LispObject,
    func: HashFunction,
}

impl HashableLispObject {
    fn with_hashfunc_and_object(o: LispObject, f: HashFunction) -> HashableLispObject {
        HashableLispObject { object: o, func: f }
    }
}

impl Hash for HashableLispObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.func {
            HashFunction::Eq => {
                self.object.hash(state);
            }
            HashFunction::Eql => {
                if self.object.is_float() {
                    let hash = unsafe { sxhash(ptr::null_mut(), self.object.to_raw()) } as u64;
                    state.write_u64(hash);
                } else {
                    self.object.hash(state);
                }
            }
            HashFunction::Equal => {
                let hash = unsafe { sxhash(ptr::null_mut(), self.object.to_raw()) } as u64;
                state.write_u64(hash);
            }
            HashFunction::UserFunc(_, _, hashfn) => {
                call!(hashfn, self.object).hash(state);
            }
        }

        state.finish();
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

#[derive(Clone)]
#[repr(C)]
pub struct LispHashTable {
    header: Lisp_Vectorlike_Header,
    weak: LispObject,
    is_pure: bool,
    func: HashFunction,
    map: FnvHashMap<HashableLispObject, HashableLispObject>,
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
            func: HashFunction::Eq,
            map: FnvHashMap::with_capacity_and_hasher(cap, Default::default()),
        }
    }
}

#[lisp_fn]
fn make_hash_map(args: &mut [LispObject]) -> LispObject {
    let mut ptr = ExternalPtr::new(allocate_pseudovector!(LispHashTable, map, PseudovecType::PVEC_HASH_TABLE));
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
            ptr.is_pure = true;
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

    LispObject::tag_ptr(ptr, Lisp_Type::Lisp_Vectorlike)
}

#[lisp_fn]
fn map_put(map: LispObject, k: LispObject, v: LispObject) -> LispObject {
    // @TODO replace with with haashtable or erorr
    let mut hashmap = map.as_hash_table_or_error();
    let key = HashableLispObject::with_hashfunc_and_object(k, hashmap.func);
    let value = HashableLispObject::with_hashfunc_and_object(v, hashmap.func);
    hashmap.map.insert(key, value);
    v
}

#[lisp_fn]
fn map_get(map: LispObject, k: LispObject) -> LispObject {
    let hashmap = map.as_hash_table_or_error();
    let key = HashableLispObject::with_hashfunc_and_object(k, hashmap.func);
    hashmap.map.get(&key).map_or(
        LispObject::constant_nil(),
        |key| key.object,
    )
}

#[lisp_fn]
fn map_rm(map: LispObject, k: LispObject) -> LispObject {
    let mut hashmap = map.as_hash_table_or_error();
    let key = HashableLispObject::with_hashfunc_and_object(k, hashmap.func);
    hashmap.map.remove(&key);
    map
}

#[lisp_fn]
fn map_clear(map: LispObject) -> LispObject {
    let mut hashmap = map.as_hash_table_or_error();
    hashmap.map.clear();
    map
}

#[lisp_fn]
fn map_count(map: LispObject) -> LispObject {
    let hashmap = map.as_hash_table_or_error();
    LispObject::from_natnum(hashmap.map.len() as EmacsInt)
}

// @TODO have this use things managed by the GC.
#[lisp_fn]
fn map_copy(map: LispObject) -> LispObject {
    let hashmap = map.as_hash_table_or_error();
    // @TODO if table is weak, add it to weak table data structure.
    let new_map = ExternalPtr::new(Box::into_raw(Box::new(hashmap.clone())));
    LispObject::tag_ptr(new_map, Lisp_Type::Lisp_Vectorlike)
}

#[lisp_fn]
fn map_test(map: LispObject) -> LispObject {
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
#[lisp_fn]
fn map_rehash_size(_map: LispObject) -> LispObject {
    LispObject::from_float(0.5)
}

#[lisp_fn]
fn map_rehash_threshold(_map: LispObject) -> LispObject {
    LispObject::from_float(0.8125)
}

#[no_mangle]
pub unsafe fn hashtable_finalize(map: *mut c_void) {
    let ptr = map as *mut LispHashTable;
    mem::drop(&*ptr);
}

#[no_mangle]
pub unsafe fn mark_hashtable(map: *mut c_void) {
    let ptr = ExternalPtr::new(map as *mut LispHashTable);
    mark_vectorlike(map as *mut Lisp_Vector);
    if let HashFunction::UserFunc(name, cmp, hash) = ptr.func {
        mark_object(name.to_raw());
        mark_object(cmp.to_raw());
        mark_object(hash.to_raw());
    }

    if ptr.weak.is_not_nil() {
        for (key, value) in ptr.map.iter() {
            mark_object(key.object.to_raw());
            mark_object(value.object.to_raw());
        }
    }
}

pub unsafe fn sweep_weak_hashtable(map: *mut c_void, remove_entries: bool) -> bool {
    let mut ptr = ExternalPtr::new(map as *mut LispHashTable);
    let weakness = ptr.weak.to_raw();
    let mut to_remove = Vec::<HashableLispObject>::new();
    let mut marked = false;

    for (key, value) in ptr.map.iter() {
        let key_survives_gc = survives_gc(key.object.to_raw());
        let value_survives_gc = survives_gc(value.object.to_raw());
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
                to_remove.push(key.clone());
            }
        } else {
            if !remove_p {
                if !key_survives_gc {
                    mark_object(key.object.to_raw());
                    marked = true;
                }

                if !value_survives_gc {
                    mark_object(value.object.to_raw());
                    marked = true;
                }
            }
        }
    }

    for x in to_remove.iter() {
        ptr.map.remove(&x);
    }

    marked
}

// @TODO have this function eassert on table purity/weakness etc.
#[no_mangle]
pub unsafe fn pure_copy_hashtable(map: *mut c_void) -> *mut c_void {
    let table_ptr = ExternalPtr::new(map as *mut LispHashTable);

    // @TODO verify that the alignment for this is correct.
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

    for (key, value) in table_ptr.map.iter() {
        let purekey = HashableLispObject {
            object: key.object.purecopy(),
            func: key.func,
        };
        let purevalue = HashableLispObject {
            object: value.object.purecopy(),
            func: value.func,
        };
        ptr.map.insert(purekey, purevalue);
    }

    ptr.as_ptr() as *mut c_void
}
