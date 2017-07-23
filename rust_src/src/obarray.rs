use libc;
use lisp::LispObject;
use remacs_sys::{Lisp_Object, Lisp_Symbol, check_obarray, check_vobarray, intern_driver,
                 make_unibyte_string, oblookup};

/// A lisp object containing an `obarray`.
pub struct LispObarrayRef(LispObject);

impl LispObarrayRef {
    /// Return a reference to the Lisp variable `obarray`.
    pub fn constant_obarray() -> LispObarrayRef {
        LispObarrayRef(LispObject::from_raw(unsafe { check_vobarray() }))
    }

    /// Return a reference corresponding to OBJECT, or raise an error if it is
    /// not a `LispObarrayRef`.
    ///
    /// # C Porting Notes
    ///
    /// This is a wrapper around the C function `check_obarray` instead of a
    /// translation, so that fatal errors are handled in the same fashion.
    pub fn from_object_or_error(object: LispObject) -> LispObarrayRef {
        LispObarrayRef(LispObject::from_raw(
            unsafe { check_obarray(object.to_raw()) },
        ))
    }

    /// Return the symbol that matches C string S, of length LEN. If there is no
    /// such symbol, return the integer bucket number of where the symbol would
    /// be if it were present.
    ///
    /// # C Porting Notes
    ///
    /// This is a wrapper around the C function `oblookup`. It uses LEN as both
    /// the byte and char length, i.e. S is treated as a unibyte string.
    pub fn lookup_cstring(&self, s: *const libc::c_char, len: libc::ptrdiff_t) -> LispObject {
        LispObject::from_raw(unsafe { oblookup(self.0.to_raw(), s, len, len) })
    }

    /// Intern the C string S, of length LEN. That is, return the new or
    /// existing symbol with that name in this `LispObarrayRef`.
    ///
    /// # C Porting Notes
    ///
    /// This is makes use of the C function `intern_driver`. It uses LEN as both
    /// the byte and char length, i.e. S is treated as a unibyte string.
    pub fn intern_cstring(&mut self, s: *const libc::c_char, len: libc::ptrdiff_t) -> LispObject {
        let tem = self.lookup_cstring(s, len);
        if tem.is_symbol() {
            tem
        } else {
            // The above `oblookup' was done on the basis of nchars==nbytes, so
            // the string has to be unibyte.
            LispObject::from_raw(unsafe {
                intern_driver(make_unibyte_string(s, len), self.0.to_raw(), tem.to_raw())
            })
        }
    }
}

/// Intern the C string `s`: return a symbol with that name, interned in the
/// current obarray.
#[no_mangle]
pub extern "C" fn intern_1(s: *const libc::c_char, len: libc::ptrdiff_t) -> Lisp_Object {
    LispObarrayRef::constant_obarray()
        .intern_cstring(s, len)
        .to_raw()
}
