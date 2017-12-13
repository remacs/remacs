//! obarray code
use libc;

use remacs_macros::lisp_fn;
use remacs_sys::{Fpurecopy, Lisp_Object};
use remacs_sys::{check_obarray, check_vobarray, globals, intern_driver, make_unibyte_string,
                 oblookup};

use lisp::LispObject;
use lisp::defsubr;

/// A lisp object containing an `obarray`.
pub struct LispObarrayRef(LispObject);

impl LispObarrayRef {
    /// Return a reference to the Lisp variable `obarray`.
    pub fn constant_obarray() -> LispObarrayRef {
        LispObarrayRef(LispObject::from(unsafe { check_vobarray() }))
    }

    /// Return a reference corresponding to OBJECT, or raise an error if it is
    /// not a `LispObarrayRef`.
    ///
    /// # C Porting Notes
    ///
    /// This is a wrapper around the C function `check_obarray` instead of a
    /// translation, so that fatal errors are handled in the same fashion.
    pub fn from_object_or_error(object: LispObject) -> LispObarrayRef {
        LispObarrayRef(LispObject::from(unsafe { check_obarray(object.to_raw()) }))
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
        LispObject::from(unsafe { oblookup(self.0.to_raw(), s, len, len) })
    }

    /// Intern the C string S, of length LEN. That is, return the new or
    /// existing symbol with that name in this `LispObarrayRef`.
    ///
    /// # C Porting Notes
    ///
    /// This is makes use of the C function `intern_driver`. It uses LEN as both
    /// the byte and char length, i.e. S is treated as a unibyte string.
    pub fn intern_cstring(self, s: *const libc::c_char, len: libc::ptrdiff_t) -> LispObject {
        let tem = self.lookup_cstring(s, len);
        if tem.is_symbol() {
            tem
        } else {
            // The above `oblookup' was done on the basis of nchars==nbytes, so
            // the string has to be unibyte.
            LispObject::from(unsafe {
                intern_driver(make_unibyte_string(s, len), self.0.to_raw(), tem.to_raw())
            })
        }
    }

    /// Return the symbol that matches NAME (either a symbol or string). If
    /// there is no such symbol, return the integer bucket number of where the
    /// symbol would be if it were present.
    ///
    /// # C Porting Notes
    ///
    /// This is a wrapper around the C function `oblookup`. It is capable of
    /// handling multibyte strings, unlike intern_1 and friends.
    pub fn lookup(&self, name: LispObject) -> LispObject {
        let string = name.symbol_or_string_as_string();
        LispObject::from(unsafe {
            oblookup(
                self.0.to_raw(),
                string.const_sdata_ptr(),
                string.len_chars(),
                string.len_bytes(),
            )
        })
    }

    /// Intern the string or symbol STRING. That is, return the new or existing
    /// symbol with that name in this `LispObarrayRef`. If Emacs is loading Lisp
    /// code to dump to an executable (ie. `purify-flag` is `t`), the symbol
    /// name will be transferred to pure storage.
    ///
    /// # C Porting Notes
    ///
    /// This is makes use of the C function `intern_driver`. It is capable of
    /// handling multibyte strings, unlike `intern_cstring`.
    pub fn intern(self, string: LispObject) -> LispObject {
        let tem = self.lookup(string);
        if tem.is_symbol() {
            tem
        } else if LispObject::from(unsafe { globals.f_Vpurify_flag }).is_not_nil() {
            // When Emacs is running lisp code to dump to an executable, make
            // use of pure storage.
            LispObject::from(unsafe {
                intern_driver(Fpurecopy(string.to_raw()), self.0.to_raw(), tem.to_raw())
            })
        } else {
            LispObject::from(unsafe {
                intern_driver(string.to_raw(), self.0.to_raw(), tem.to_raw())
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

/// Return the canonical symbol named NAME, or nil if none exists.
/// NAME may be a string or a symbol.  If it is a symbol, that exact
/// symbol is searched for.
/// A second optional argument specifies the obarray to use;
/// it defaults to the value of `obarray'.
#[lisp_fn(min = "1")]
pub fn intern_soft(name: LispObject, obarray: LispObject) -> LispObject {
    let tem = if obarray.is_nil() {
        LispObarrayRef::constant_obarray().lookup(name)
    } else {
        LispObarrayRef::from_object_or_error(obarray).lookup(name)
    };

    if tem.is_integer() || (name.is_symbol() && !name.eq(tem)) {
        LispObject::constant_nil()
    } else {
        tem
    }
}

/// Return the canonical symbol whose name is STRING.
/// If there is none, one is created by this function and returned.
/// A second optional argument specifies the obarray to use;
/// it defaults to the value of `obarray'.
#[lisp_fn(min = "1")]
pub fn intern(string: LispObject, obarray: LispObject) -> LispObject {
    if obarray.is_nil() {
        LispObarrayRef::constant_obarray().intern(string)
    } else {
        LispObarrayRef::from_object_or_error(obarray).intern(string)
    }
}

include!(concat!(env!("OUT_DIR"), "/obarray_exports.rs"));
