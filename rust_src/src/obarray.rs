//! obarray code
use libc;

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    multibyte::{LispStringRef, LispSymbolOrString},
    remacs_sys::{
        fatal_error_in_progress, globals, initial_obarray, initialized, intern_sym,
        make_pure_c_string, make_unibyte_string, oblookup,
    },
    remacs_sys::{Fmake_symbol, Fpurecopy},
    remacs_sys::{Qnil, Qvectorp},
    symbols::LispSymbolRef,
    vectors::LispVectorRef,
};

/// A lisp object containing an `obarray`.
#[repr(transparent)]
pub struct LispObarrayRef(LispObject);

impl From<LispObarrayRef> for LispObject {
    fn from(o: LispObarrayRef) -> LispObject {
        o.0
    }
}

impl From<&LispObarrayRef> for LispObject {
    fn from(o: &LispObarrayRef) -> LispObject {
        o.0
    }
}

impl LispObarrayRef {
    pub fn new(obj: LispObject) -> LispObarrayRef {
        LispObarrayRef(obj)
    }

    /// Return a reference to the Lisp variable `obarray`.
    pub fn global() -> LispObarrayRef {
        LispObarrayRef(check_obarray(unsafe { globals.Vobarray }))
    }

    /// Return the symbol that matches NAME (either a symbol or string). If
    /// there is no such symbol, return the integer bucket number of where the
    /// symbol would be if it were present.
    pub fn lookup(&self, name: LispSymbolOrString) -> LispObject {
        let string: LispStringRef = name.into();
        unsafe {
            oblookup(
                self.into(),
                string.const_sdata_ptr(),
                string.len_chars(),
                string.len_bytes(),
            )
        }
    }

    /// Intern the string or symbol STRING. That is, return the new or existing
    /// symbol with that name in this `LispObarrayRef`. If Emacs is loading Lisp
    /// code to dump to an executable (ie. `purify-flag` is `t`), the symbol
    /// name will be transferred to pure storage.
    pub fn intern(&self, string: impl Into<LispSymbolOrString>) -> LispObject {
        let string = string.into();
        let tem = self.lookup(string);
        if tem.is_symbol() {
            tem
        } else {
            let string_copy: LispObject = if unsafe { globals.Vpurify_flag }.is_not_nil() {
                // When Emacs is running lisp code to dump to an executable, make
                // use of pure storage.
                unsafe { Fpurecopy(string.into()) }
            } else {
                string.into()
            };
            intern_driver(string_copy, self.into(), tem)
        }
    }
}

impl From<LispObject> for LispObarrayRef {
    fn from(o: LispObject) -> LispObarrayRef {
        LispObarrayRef::new(check_obarray(o))
    }
}

impl From<LispObject> for Option<LispObarrayRef> {
    fn from(o: LispObject) -> Self {
        if o.is_nil() {
            None
        } else {
            Some(o.into())
        }
    }
}

/// Intern (e.g. create a symbol from) a string.
pub fn intern<T: AsRef<str>>(string: T) -> LispSymbolRef {
    let s = string.as_ref();
    unsafe {
        intern_1(
            s.as_ptr() as *const libc::c_char,
            s.len() as libc::ptrdiff_t,
        )
    }
    .into()
}

#[no_mangle]
pub extern "C" fn loadhist_attach(x: LispObject) {
    unsafe {
        if initialized {
            globals.Vcurrent_load_list = (x, globals.Vcurrent_load_list).into();
        }
    }
}

/// Get an error if OBARRAY is not an obarray.
/// If it is one, return it.
#[no_mangle]
pub extern "C" fn check_obarray(obarray: LispObject) -> LispObject {
    // We don't want to signal a wrong-type error when we are shutting
    // down due to a fatal error and we don't want to hit assertions
    // if the fatal error was during GC.
    if unsafe { fatal_error_in_progress } {
        return obarray;
    }

    // A valid obarray is a non-empty vector.
    let v = obarray.as_vector();
    if v.map_or(0, LispVectorRef::len) == 0 {
        // If Vobarray is now invalid, force it to be valid.
        if unsafe { globals.Vobarray }.eq(obarray) {
            unsafe { globals.Vobarray = initial_obarray };
        }
        wrong_type!(Qvectorp, obarray);
    }

    obarray
}

#[no_mangle]
pub extern "C" fn map_obarray(
    obarray: LispObject,
    func: extern "C" fn(LispObject, LispObject),
    arg: LispObject,
) {
    let v = obarray.as_vector_or_error();
    for item in v.iter().rev() {
        if let Some(sym) = item.as_symbol() {
            for s in sym.iter() {
                func(s.into(), arg);
            }
        }
    }
}

/// Intern the C string `s`: return a symbol with that name, interned in the
/// current obarray.
#[no_mangle]
pub unsafe extern "C" fn intern_1(s: *const libc::c_char, len: libc::ptrdiff_t) -> LispObject {
    let obarray = LispObject::from(LispObarrayRef::global());
    let tem = oblookup(obarray, s, len, len);

    if tem.is_symbol() {
        tem
    } else {
        // The above `oblookup' was done on the basis of nchars==nbytes, so
        // the string has to be unibyte.
        intern_driver(make_unibyte_string(s, len), obarray, tem)
    }
}

/// Intern the C string STR: return a symbol with that name,
/// interned in the current obarray.
#[no_mangle]
pub unsafe extern "C" fn intern_c_string_1(
    s: *const libc::c_char,
    len: libc::ptrdiff_t,
) -> LispObject {
    let obarray = LispObject::from(LispObarrayRef::global());
    let tem = oblookup(obarray, s, len, len);

    if tem.is_symbol() {
        tem
    } else {
        // Creating a non-pure string from a string literal not implemented yet.
        // We could just use make_string here and live with the extra copy.
        assert!(globals.Vpurify_flag.is_not_nil());
        intern_driver(make_pure_c_string(s, len), obarray, tem)
    }
}

/// Intern a symbol with name STRING in OBARRAY using bucket INDEX.
#[no_mangle]
pub extern "C" fn intern_driver(
    string: LispObject,
    obarray: LispObject,
    index: LispObject,
) -> LispObject {
    unsafe { intern_sym(Fmake_symbol(string), obarray, index) }
}

/// Return the canonical symbol named NAME, or nil if none exists.
/// NAME may be a string or a symbol.  If it is a symbol, that exact
/// symbol is searched for.
/// A second optional argument specifies the obarray to use;
/// it defaults to the value of `obarray'.
#[lisp_fn(min = "1")]
pub fn intern_soft(name: LispSymbolOrString, obarray: Option<LispObarrayRef>) -> LispObject {
    let obarray = obarray.unwrap_or_else(LispObarrayRef::global);
    let tem = obarray.lookup(name);

    if tem.is_integer() || (name.is_symbol() && !name.eq(&tem)) {
        Qnil
    } else {
        tem
    }
}

/// Return the canonical symbol whose name is STRING.
/// If there is none, one is created by this function and returned.
/// A second optional argument specifies the obarray to use;
/// it defaults to the value of `obarray'.
#[lisp_fn(name = "intern", c_name = "intern", min = "1")]
pub fn lisp_intern(string: LispStringRef, obarray: Option<LispObarrayRef>) -> LispObject {
    let obarray_ref = obarray.unwrap_or_else(LispObarrayRef::global);

    obarray_ref.intern(string)
}

extern "C" fn mapatoms_1(sym: LispObject, function: LispObject) {
    call!(function, sym);
}

/// Call FUNCTION on every symbol in OBARRAY.
/// OBARRAY defaults to the value of `obarray'.
#[lisp_fn(min = "1")]
pub fn mapatoms(function: LispObject, obarray: Option<LispObarrayRef>) {
    let obarray = obarray.unwrap_or_else(LispObarrayRef::global);

    map_obarray(obarray.into(), mapatoms_1, function);
}

include!(concat!(env!("OUT_DIR"), "/obarray_exports.rs"));
