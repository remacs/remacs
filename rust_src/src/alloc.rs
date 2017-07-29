use lisp::ExternalPtr;
use hashtable::LispHashTable;
use std::sync::Mutex;
use libc::c_void;

pub trait GCObject: Send + Sync {
    fn mark(&mut self);
    fn unmark(&mut self);
    fn is_marked(&self) -> bool;
}

struct ManagedList<T: GCObject + Sized> {
    managed: Vec<Option<T>>,
    free_list: Vec<usize>
}

impl<T: GCObject + Sized> ManagedList<T> {
    fn new() -> ManagedList<T> {
        ManagedList {
            managed: Vec::new(),
            free_list: Vec::new()
        }
    }

    fn free(&mut self, idx: usize) {
        assert!(self.managed.len() > idx);
        self.managed[idx] = None;
        self.free_list.push(idx);
    }

    fn alloc(&mut self, t: T) -> ExternalPtr<T> {
        let addr = match self.free_list.pop() {
            Some(idx) => {
                self.managed[idx] = Some(t);
                &mut self.managed[idx]
            },
            
            None => {
                self.managed.push(Some(t));
                let len = self.managed.len();
                &mut self.managed[len - 1]
            }
            
        };

        ExternalPtr::new(addr.as_mut().unwrap())
    }

    fn sweep(&mut self) {
        let len = self.managed.len();
        let mut one_marked = false;
        for i in 0..len {
            if self.managed[i].is_some() {
                if self.managed[i].as_ref().unwrap().is_marked() {
                    self.managed[i].as_mut().unwrap().unmark();
                    one_marked = true;
                } else {
                    self.free(i);
                }   
            }
        }

        if !one_marked {
            self.managed.clear();
            self.free_list.clear();
        }
    }
}

pub struct LispGarbageCollector {
    managed_hashtables: ManagedList<LispHashTable>
}

lazy_static! {
    pub static ref GC: Mutex<LispGarbageCollector> = {
        Mutex::new(LispGarbageCollector::new())
    };
}

macro_rules! garbage_collector {
    () => { ::alloc::GC.lock().unwrap() }
}

impl LispGarbageCollector {
    pub fn new() -> LispGarbageCollector {
        LispGarbageCollector {
            managed_hashtables: ManagedList::new()
        }
    }
    pub fn manage_hashtable(&mut self, table: LispHashTable) -> ExternalPtr<LispHashTable> {
        self.managed_hashtables.alloc(table)
    }

    fn sweep_hashtables(&mut self) {
        self.managed_hashtables.sweep();
    }

    pub fn sweep(&mut self) {
        self.sweep_hashtables();
    }
}

// Since we have already done a typecheck on the C layer, we can avoid a vtable look up here
// by casting to a specific type and calling mark directly. In the future, as we transition away from the C,
// we can rely on the trait to do what we need to do.
#[no_mangle]
pub unsafe fn rust_mark_hashtable(ptr: *mut c_void) {
    let mut ptr = ExternalPtr::new(ptr as *mut LispHashTable);
    ptr.mark();
}

#[no_mangle]
pub fn rust_sweep() {
    garbage_collector!().sweep();
}

#[cfg(test)]
macro_rules! count_managed {
    ($gc: ident, $map: ident) => ({
        let countvec = $gc.$map.managed.iter()
            .filter(|x| x.is_some())
            .collect::<Vec<_>>();
        countvec.len()
    })
}

#[test]
fn gc_collection() {
    let mut gc = LispGarbageCollector::new();
    let mut table = gc.manage_hashtable(LispHashTable::new());
    table.mark();
    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 1);

    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 0);
}

#[test]
fn gc_ptr_management() {
    let mut gc = LispGarbageCollector::new();
    let mut table = gc.manage_hashtable(LispHashTable::new());
    table.mark();
    assert!(table.is_marked());
    table.unmark();
    assert!(!table.is_marked());
}

#[test]
fn gc_collection_2() {
    let mut gc = LispGarbageCollector::new();
    let mut table = gc.manage_hashtable(LispHashTable::new());
    let mut table2 = gc.manage_hashtable(LispHashTable::new());
    let mut table3 = gc.manage_hashtable(LispHashTable::new());
    table.mark();
    table2.mark();
    table3.mark();
    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 3);
    
    table.mark();
    table2.mark();
    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 2);

    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 0);
}

#[test]
fn gc_collection_3() {
    let mut gc = LispGarbageCollector::new();
    for idx in 0..1024 {
        let mut table = gc.manage_hashtable(LispHashTable::new());
        if idx % 2 == 0 {
            table.mark();
        }
    }

    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 512);
}

