use libc;
use std::ptr;

use crate::{
    lisp::ExternalPtr,
    remacs_sys::{terminal, wr_display_info, KBOARD},
};

pub mod term;

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

    pub fn get_ref(&mut self) -> DisplayInfoRef {
        DisplayInfoRef::new(self as *mut DisplayInfo)
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

impl From<DisplayInfo> for DisplayInfoRef {
    fn from(mut df: DisplayInfo) -> DisplayInfoRef {
        df.get_ref()
    }
}

pub type TerminalRef = ExternalPtr<terminal>;
impl Default for TerminalRef {
    fn default() -> Self {
        Self::new(ptr::null_mut())
    }
}

pub type KboardRef = ExternalPtr<KBOARD>;

impl KboardRef {
    pub fn add_ref(&mut self) {
        (*self).reference_count = (*self).reference_count + 1;
    }
}
