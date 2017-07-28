#[macro_use]
use lazy_static;
use lisp::{ExternalPtr, LispObject};
use remacs_sys::{Lisp_Object};
use hashtable::LispHashTable;
use std::sync::Mutex;

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
        Mutex::new(LispGarbageCollector { managed_objects: Vec::new() })
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
        gc.managed_objects.retain(|ref x| x.is_marked());
    }

    pub unsafe fn mark_object<T: 'static + GCObject + Sized>(object: LispObject) {
        let mut ptr = ExternalPtr::new(object.get_untaggedptr() as *mut T);
        ptr.mark();
    }

    pub unsafe fn unmark_object<T: 'static + GCObject + Sized>(object: LispObject) {
        let mut ptr = ExternalPtr::new(object.get_untaggedptr() as *mut T);
        ptr.unmark();
    }

    pub fn mark<T: 'static + GCObject + Sized>(ptr: ExternalPtr<T>) {
        let mut boxed = unsafe { Box::from_raw(ptr.clone().as_mut()) };
        boxed.mark();
        Box::into_raw(boxed);
    }

    pub fn unmark<T: 'static + GCObject + Sized>(ptr: ExternalPtr<T>) {
        let mut boxed = unsafe { Box::from_raw(ptr.clone().as_mut()) };
        boxed.unmark();
        Box::into_raw(boxed);
    }
}

#[no_mangle]
pub unsafe fn rust_mark_hashtable(object: Lisp_Object) {
    LispGarbageCollector::mark_object::<LispHashTable>(LispObject::from_raw(object));
}
