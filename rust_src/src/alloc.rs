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
        Mutex::new(LispGarbageCollector { managed_hashtables: ManagedList::new() })
    };
}

impl LispGarbageCollector {
    pub fn manage_hashtable(table: LispHashTable) -> ExternalPtr<LispHashTable> {
        GC.lock().unwrap().managed_hashtables.alloc(table)
    }

    fn sweep_hashtables() {
        GC.lock().unwrap().managed_hashtables.sweep();
    }

    pub fn sweep() {
        Self::sweep_hashtables();
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
    LispGarbageCollector::sweep();
}

#[cfg(test)]
macro_rules! count_managed {
    ($map: ident) => ({
        let gc = GC.lock().unwrap();
        let countvec = gc.$map.managed.iter()
            .filter(|x| x.is_some())
            .collect::<Vec<_>>();
        countvec.len()
    })
}

#[cfg(test)]
fn gc_collection() {
    let mut table = LispGarbageCollector::manage_hashtable(LispHashTable::new());
    table.mark();
    LispGarbageCollector::sweep();
    assert!(count_managed!(managed_hashtables) == 1);

    LispGarbageCollector::sweep();
    assert!(count_managed!(managed_hashtables) == 0);
}

#[test]
fn gc_ptr_management() {
    let mut table = LispGarbageCollector::manage_hashtable(LispHashTable::new());
    table.mark();
    assert!(table.is_marked());
    table.unmark();
    assert!(!table.is_marked());
}

#[cfg(test)]
fn gc_collection_2() {
    let mut table = LispGarbageCollector::manage_hashtable(LispHashTable::new());
    let mut table2 = LispGarbageCollector::manage_hashtable(LispHashTable::new());
    let mut table3 = LispGarbageCollector::manage_hashtable(LispHashTable::new());
    table.mark();
    table2.mark();
    table3.mark();
    LispGarbageCollector::sweep();
    assert!(count_managed!(managed_hashtables) == 3);
    
    table.mark();
    table2.mark();
    LispGarbageCollector::sweep();
    assert!(count_managed!(managed_hashtables) == 2);

    LispGarbageCollector::sweep();
    assert!(count_managed!(managed_hashtables) == 0);
}

#[cfg(test)]
fn gc_collection_3() {
    for idx in 0..1024 {
        let mut table = LispGarbageCollector::manage_hashtable(LispHashTable::new());
        if idx % 2 == 0 {
            table.mark();
        }
    }

    LispGarbageCollector::sweep();
    assert!(count_managed!(managed_hashtables) == 512);
}

#[test]
fn gc_tests() {
    gc_collection();
    LispGarbageCollector::sweep();
    gc_collection_2();
    LispGarbageCollector::sweep();
    gc_collection_3();
    LispGarbageCollector::sweep();
}

