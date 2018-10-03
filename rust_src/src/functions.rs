/// This module is loaded only in #[cfg(test)].
/// It contains the definitions of C functions to be mocked in our tests
/// Due to the fact that cfg(test) does not cascade to dependent crates,
/// we need to duplicate the extern "C" block found in remacs-sys,
/// so that our the #[mock] macro can generate the code
/// we need for mocking in our tests.

/// Adding a function to this block is harmless.
/// This module is only for testing, and you should add all
/// definitions to remacs-sys first and foremost.
use libc::*;
use remacs_sys::*;

use lisp::LispObject;

// The linker needs the symbol "lispsym" to exist, since certain
// codepaths lead to it's usage.
#[no_mangle]
pub static mut lispsym: EmacsInt = 0;

#[warn(unused_macros)]
macro_rules! mock_float {
    () => {
        mock_float!(0.0)
    };

    ($f: expr) => {{
        // Fake an allocated float by just putting it on the heap and leaking it.
        let boxed = Box::new(::remacs_sys::Lisp_Float {
            u: ::remacs_sys::Lisp_Float__bindgen_ty_1 {
                data: unsafe { ::std::mem::transmute($f) },
            },
        });
        let raw = ::lisp::ExternalPtr::new(Box::into_raw(boxed));
        ::lisp::LispObject::tag_ptr(raw, ::remacs_sys::Lisp_Type::Lisp_Float)
    }};
}

#[macro_export]
macro_rules! mock_unibyte_string {
    () => {
        mock_unibyte_string!("")
    };
    ($string: expr) => {{
        let strcopy = ::std::ffi::CString::new($string).unwrap();
        let len = strcopy.as_bytes().len() as ::libc::ptrdiff_t;
        let boxed = Box::new(::remacs_sys::Lisp_String {
            size: len,
            size_byte: -1,
            intervals: ::std::ptr::null_mut(),
            data: strcopy.into_raw() as *mut u8,
        });

        let ptr = ::lisp::ExternalPtr::new(Box::into_raw(boxed));
        ::lisp::LispObject::tag_ptr(ptr, ::remacs_sys::Lisp_Type::Lisp_String)
    }};
}

#[macro_export]
macro_rules! mock_multibyte_string {
    () => {
        mock_multibyte_string!("")
    };
    ($string: expr) => {{
        let strcopy = ::std::ffi::CString::new($string).unwrap();
        let len = strcopy.as_bytes().len() as ::libc::ptrdiff_t;
        let boxed = Box::new(::remacs_sys::Lisp_String {
            size: len,
            size_byte: len,
            intervals: ::std::ptr::null_mut(),
            data: strcopy.into_raw() as *mut u8,
        });

        let ptr = ::lisp::ExternalPtr::new(Box::into_raw(boxed));
        ::lisp::LispObject::tag_ptr(ptr, ::remacs_sys::Lisp_Type::Lisp_String)
    }};
}

#[allow(unused_macros)]
macro_rules! assert_t {
    ($arg: expr) => {{
        assert!($arg == ::lisp::LispObject::constant_t());
    }};
}

#[allow(unused_macros)]
macro_rules! assert_nil {
    ($arg: expr) => {{
        assert!($arg == ::remacs_sys::Qnil);
    }};
}

// Note(db48x): see if we can go back to using mock-derive for these
#[cfg(test)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn Fcons(car: LispObject, cdr: LispObject) -> LispObject {
    return Qnil;
}

#[cfg(test)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn Fsignal(error_symbol: LispObject, data: LispObject) -> ! {
    panic!("Fsignal called during tests");
}

#[cfg(test)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn make_string(s: *const c_char, length: isize) -> LispObject {
    mock_multibyte_string!()
}

#[cfg(test)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[no_mangle]
pub extern "C" fn make_unibyte_string(s: *const c_char, length: isize) -> LispObject {
    mock_unibyte_string!()
}
