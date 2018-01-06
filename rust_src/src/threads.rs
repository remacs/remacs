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
    pub fn name(self) -> LispObject {
        LispObject::from_raw(self.name)
    }

    #[inline]
    pub fn is_alive(self) -> bool {
        !self.m_specpdl.is_null()
    }
}

/// Return the name of the THREAD.
/// The name is the same object that was passed to `make-thread'.
#[lisp_fn]
pub fn thread_name(thread: ThreadStateRef) -> LispObject {
    thread.name()
}

/// Return t if THREAD is alive, or nil if it has exited.
#[lisp_fn]
pub fn thread_alive_p(thread: ThreadStateRef) -> bool {
    thread.is_alive()
}

include!("threads_exports.rs");
