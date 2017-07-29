use std::mem;
use remacs_sys::current_thread;
use buffers::LispBufferRef;

pub struct ThreadState {}

impl ThreadState {
    pub fn current_buffer() -> LispBufferRef {
        unsafe { mem::transmute((*current_thread).m_current_buffer) }
    }
}
