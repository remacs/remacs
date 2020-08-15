use libc;
use std::ptr;

use crate::{lisp::ExternalPtr, remacs_sys::wr_display_info};

use super::term::TerminalRef;

#[derive(Default)]
pub struct DisplayInfoInner {
    pub terminal: TerminalRef,
}

pub type DisplayInfoInnerRef = ExternalPtr<DisplayInfoInner>;

pub type DisplayInfo = wr_display_info;

impl DisplayInfo {
    pub fn new() -> Self {
        let mut df = DisplayInfo::default();

        let inner = Box::new(DisplayInfoInner::default());
        df.inner = Box::into_raw(inner) as *mut libc::c_void;

        df
    }

    pub fn get_inner(&self) -> DisplayInfoInnerRef {
        DisplayInfoInnerRef::new(self.inner as *mut DisplayInfoInner)
    }
}

impl Drop for DisplayInfo {
    fn drop(&mut self) {
        if self.inner != ptr::null_mut() {
            unsafe {
                Box::from_raw(self.inner as *mut DisplayInfoInner);
            }
        }
    }
}

pub type DisplayInfoRef = ExternalPtr<DisplayInfo>;
unsafe impl Sync for DisplayInfoRef {}
