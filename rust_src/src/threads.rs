//! Threading code.

use std::mem;

use remacs_macros::lisp_fn;
use remacs_sys::{current_thread, thread_state};

use buffers::LispBufferRef;
use lisp::{ExternalPtr, LispObject};
use lisp::defsubr;

pub type ThreadStateRef = ExternalPtr<thread_state>;

pub struct ThreadState {}

impl ThreadState {
    pub fn current_buffer() -> LispBufferRef {
        unsafe { mem::transmute((*current_thread).m_current_buffer) }
    }
}

impl ThreadStateRef {
    #[inline]
    pub fn name(&self) -> LispObject {
        LispObject::from(self.name)
    }
}

/// Return the name of the THREAD.
/// The name is the same object that was passed to `make-thread'.
#[lisp_fn]
pub fn thread_name(thread: LispObject) -> LispObject {
    thread.as_thread_or_error().name()
}

pub fn rust_init_syms() {
    unsafe {
        defsubr(&*Sthread_name);
    }
}
