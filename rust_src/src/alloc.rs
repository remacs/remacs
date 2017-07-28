use lisp::ExternalPtr;
use hashtable::LispHashTable;
use std::sync::Mutex;
use libc::c_void;

use test::Bencher;

pub trait GCObject: Send + Sync {
    fn mark(&mut self);
    fn unmark(&mut self);
    fn is_marked(&self) -> bool;
}

pub struct LispGarbageCollector {
    managed_objects: Vec<Box<GCObject>>
}

lazy_static! {
    pub static ref GC: Mutex<LispGarbageCollector> = {
        Mutex::new(LispGarbageCollector { managed_objects: Vec::with_capacity(256) })
    };
}

impl LispGarbageCollector {
    pub fn manage<T: 'static + GCObject + Sized>(t: T) -> ExternalPtr<T> {
        let mut gc = GC.lock().unwrap();
        let boxed = Box::new(t);
        let ptr = Box::into_raw(boxed);
        gc.managed_objects.push(unsafe { Box::from_raw(ptr) });
        ExternalPtr::new(ptr)
    }

    pub fn sweep() {
        let mut gc = GC.lock().unwrap();
        // This isn't just using Vec::retain because we want to be able to mutate our objects
        // as we iterate over them. This should be equiv in performance to Vec::retain.
        let len = gc.managed_objects.len();
        let mut del = 0;
        for i in 0..len {
            if !gc.managed_objects[i].is_marked() {
                del += 1;
            } else {
                gc.managed_objects[i].unmark();
                
                if del > 0 {
                    gc.managed_objects.swap(i - del, i);
                }
            }
        }
        
        if del > 0 {
            gc.managed_objects.truncate(len - del);
        }
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

#[test]
fn gc_collection() {
    let mut table = LispGarbageCollector::manage(LispHashTable::new());
    table.mark();
    LispGarbageCollector::sweep();
    {
        let gc = GC.lock().unwrap();
        assert!(gc.managed_objects.len() == 1);
    }

    LispGarbageCollector::sweep();
    {
        let gc = GC.lock().unwrap();
        assert!(gc.managed_objects.len() == 0);
    }
}

#[test]
fn gc_collection_2() {
    let mut table = LispGarbageCollector::manage(LispHashTable::new());
    let mut table2 = LispGarbageCollector::manage(LispHashTable::new());
    let mut table3 = LispGarbageCollector::manage(LispHashTable::new());
    table.mark();
    table2.mark();
    table3.mark();
    LispGarbageCollector::sweep();
    {
        let gc = GC.lock().unwrap();
        assert!(gc.managed_objects.len() == 3);
    }

    table.mark();
    table2.mark();
    LispGarbageCollector::sweep();
    {
        let gc = GC.lock().unwrap();
        assert!(gc.managed_objects.len() == 2);
    }

        LispGarbageCollector::sweep();
    {
        let gc = GC.lock().unwrap();
        assert!(gc.managed_objects.len() == 0);
    }
}

#[bench]
fn gc_collection_bench(b: &mut Bencher) {
    b.iter(|| {
        for _ in 0..4096 {
            LispGarbageCollector::manage(LispHashTable::new());
        }

        LispGarbageCollector::sweep();
    });
}

#[bench]
fn gc_no_box_or_vtable(b: &mut Bencher) {
    b.iter(|| {
        let mut vec: Vec<LispHashTable> = Vec::new();
        for _ in 0..4096 {
            vec.push(LispHashTable::new());
        }

        vec.retain(|x| x.is_marked());
    });
}

#[bench]
fn gc_no_box_or_vtable_option(b: &mut Bencher) {
    b.iter(|| {
        let mut vec: Vec<Option<LispHashTable>> = Vec::new();
        for _ in 0..4096 {
            vec.push(Some(LispHashTable::new()));
        }

        vec.retain(|x| x.is_some() && x.as_ref().unwrap().is_marked());
    });
}

#[bench]
fn gc_box_with_no_vtable(b: &mut Bencher) {
    b.iter(|| {
        let mut vec: Vec<Box<LispHashTable>> = Vec::new();
        for _ in 0..4096 {
            vec.push(Box::new(LispHashTable::new()));
        }

        vec.retain(|x| x.is_marked());
    });
}
