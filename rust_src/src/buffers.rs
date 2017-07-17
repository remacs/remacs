//! Functions operating on buffers.

use libc::{c_uchar, ptrdiff_t};

use lisp::{LispObject, ExternalPtr, Qnil};
use remacs_sys::Lisp_Buffer;
use strings::string_equal;
use lists::car;

use remacs_sys::EmacsInt;

use remacs_macros::lisp_fn;

pub const BEG_BYTE: ptrdiff_t = 1;

pub type LispBufferRef = ExternalPtr<Lisp_Buffer>;

impl LispBufferRef {
    #[inline]
    pub fn beg_addr(&self) -> *mut c_uchar {
        unsafe { (*self.text).beg }
    }

    #[inline]
    pub fn beg_byte(&self) -> ptrdiff_t {
        BEG_BYTE
    }

    #[inline]
    pub fn gpt_byte(&self) -> ptrdiff_t {
        unsafe { (*self.text).gpt_byte }
    }

    #[inline]
    pub fn gap_end_addr(&self) -> *mut c_uchar {
        unsafe {
            (*self.text).beg.offset(
                (*self.text).gpt_byte + (*self.text).gap_size -
                    BEG_BYTE,
            )
        }
    }

    #[inline]
    pub fn z_addr(&self) -> *mut c_uchar {
        unsafe {
            (*self.text).beg.offset(
                (*self.text).gap_size + (*self.text).z_byte -
                    BEG_BYTE,
            )
        }
    }

    #[inline]
    pub fn z_byte(&self) -> ptrdiff_t {
        unsafe { (*self.text).z_byte }
    }

    // Check if buffer is live
    #[inline]
    pub fn is_live(self) -> bool {
        LispObject::from_raw(self.name).is_not_nil()
    }
}

/// Return t if OBJECT is an overlay.
#[lisp_fn]
pub fn overlayp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_overlay())
}

/// Return non-nil if OBJECT is a buffer which has not been killed.
/// Value is nil if OBJECT is not a buffer or if it has been killed.
#[lisp_fn]
pub fn buffer_live_p(object: LispObject) -> LispObject {
    LispObject::from_bool(object.as_buffer().map_or(false, |m| m.is_live()))
}

/// Like Fassoc, but use Fstring_equal to compare
/// (which ignores text properties), and don't ever quit.
#[no_mangle]
pub fn assoc_ignore_text_properties(key: LispObject, list: LispObject) -> LispObject {
    let result = list.iter_tails_safe().find(|&item| {
        string_equal(car(item.car()), key).is_not_nil()
    });
    if let Some(elt) = result {
        elt.car()
    } else {
        Qnil
    }
}
