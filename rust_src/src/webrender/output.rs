use libc;
use std::ptr;

use crate::{lisp::ExternalPtr, remacs_sys::wr_output};

use super::display_info::DisplayInfoRef;

#[derive(Default)]
pub struct OutputInner {
    pub display_info: DisplayInfoRef,
}

pub type OutputInnerRef = ExternalPtr<OutputInner>;

pub type Output = wr_output;
impl Output {
    pub fn new() -> Self {
        let mut output = Output::default();

        let inner = Box::new(OutputInner::default());
        output.inner = Box::into_raw(inner) as *mut libc::c_void;

        output
    }

    pub fn get_inner(&self) -> OutputInnerRef {
        OutputInnerRef::new(self.inner as *mut OutputInner)
    }
}

impl Drop for Output {
    fn drop(&mut self) {
        if self.inner != ptr::null_mut() {
            unsafe {
                Box::from_raw(self.inner as *mut OutputInner);
            }
        }
    }
}

pub type OutputRef = ExternalPtr<Output>;
