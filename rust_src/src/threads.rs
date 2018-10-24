//! Threading code.

use std::mem;

use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{current_thread as current_thread_pointer, thread_state, Lisp_Type, SPECPDL_INDEX};

use buffers::LispBufferRef;
use lisp::defsubr;
use lisp::{ExternalPtr, LispObject};

pub type ThreadStateRef = ExternalPtr<thread_state>;

pub struct ThreadState {}

impl ThreadState {
    pub fn current_buffer() -> LispBufferRef {
        unsafe { mem::transmute((*current_thread_pointer).m_current_buffer) }
    }

    pub fn current_thread() -> ThreadStateRef {
        unsafe { mem::transmute(current_thread_pointer) }
    }
}

impl ThreadStateRef {
    #[inline]
    pub fn name(self) -> LispObject {
        self.name
    }

    #[inline]
    pub fn is_alive(self) -> bool {
        !self.m_specpdl.is_null()
    }

    #[inline]
    pub fn as_lisp_obj(self) -> LispObject {
        LispObject::tag_ptr(self, Lisp_Type::Lisp_Vectorlike)
    }
}

impl From<LispObject> for ThreadStateRef {
    #[inline]
    fn from(o: LispObject) -> Self {
        o.as_thread_or_error()
    }
}

impl LispObject {
    pub fn is_thread(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_THREAD))
    }

    pub fn as_thread(self) -> Option<ThreadStateRef> {
        self.as_vectorlike().and_then(|v| v.as_thread())
    }

    pub fn as_thread_or_error(self) -> ThreadStateRef {
        self.as_thread()
            .unwrap_or_else(|| wrong_type!(Qthreadp, self))
    }
}

// FIXME: The right thing to do is start indexing thread.m_specpdl as
// an array instead of depending on C style pointer math.
pub fn c_specpdl_index() -> libc::ptrdiff_t {
    // ThreadStateRef::new(unsafe { current_thread }).specpdl_index()
    unsafe { SPECPDL_INDEX() }
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

/// Return the current thread.
#[lisp_fn]
pub fn current_thread() -> LispObject {
    ThreadState::current_thread().as_lisp_obj()
}

/// Return the object that THREAD is blocking on.
// If THREAD is blocked in `thread-join' on a second thread, return that
// thread.
// If THREAD is blocked in `mutex-lock', return the mutex.
// If THREAD is blocked in `condition-wait', return the condition variable.
// Otherwise, if THREAD is not blocked, return nil.
#[lisp_fn(name = "thread--blocker")]
pub fn thread_blocker(thread: ThreadStateRef) -> LispObject {
    thread.event_object
}

include!(concat!(env!("OUT_DIR"), "/threads_exports.rs"));
