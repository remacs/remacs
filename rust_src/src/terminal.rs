use lisp::ExternalPtr;
use remacs_sys::{terminal, kboard as Kboard, ns_display_info as NsDisplayInfo,
                 tty_display_info as TtyDisplayInfo, w32_display_info as W32DisplayInfo,
                 x_display_info as XDisplayInfo};

pub type Terminal = ExternalPtr<terminal>;

pub enum DisplayType {
    Ns,
    Tty,
    W32,
    X,
}

#[derive(PartialEq)]
pub enum DisplayInfo {
    Ns(*const NsDisplayInfo),
    Tty(*const TtyDisplayInfo),
    W32(*const W32DisplayInfo),
    X(*const XDisplayInfo),
}

impl Terminal {
    #[inline]
    pub fn display_info(&self, display_type: DisplayType) -> DisplayInfo {
        match display_type {
            DisplayType::Ns => DisplayInfo::Ns(unsafe { self.display_info.ns }),
            DisplayType::Tty => DisplayInfo::Tty(unsafe { self.display_info.tty }),
            DisplayType::W32 => DisplayInfo::W32(unsafe { self.display_info.w32 }),
            DisplayType::X => DisplayInfo::X(unsafe { self.display_info.x }),
        }
    }

    #[inline]
    pub fn kboard(&self) -> &Kboard {
        &self.kboard
    }

    #[inline]
    pub fn tty(&self) -> *const TtyDisplayInfo {
        unsafe { self.display_info.tty }
    }
}
