/// This module is loaded only in #[cfg(test)]. It contains C functions to be mocked in our tests
/// as part of our mocking library. Due to the fact that cfg(test) does not cascade to dependent crates,
/// we need to duplicate the extern "C" block found in remacs-sys here, so that our coding proc macro
/// can generate the boilerplate we need for testing.

/// Adding a function to this block does not real harm, however we should always define these
/// functions in remacs-sys, and all use directives should reference remacs-sys and not this module.
use libc::{c_char, c_int, c_double, c_void, ptrdiff_t};
use remacs_sys::*;
use mock_derive::mock;

#[allow(dead_code)]
#[mock]
extern "C" {
    pub fn make_float(float_value: c_double) -> Lisp_Object;
    pub fn make_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn make_lisp_ptr(ptr: *const c_void, ty: Lisp_Type) -> Lisp_Object;
    pub fn build_string(s: *const c_char) -> Lisp_Object;
    pub fn make_unibyte_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn make_uninit_string(length: EmacsInt) -> Lisp_Object;
    pub fn make_uninit_multibyte_string(nchars: EmacsInt, nbytes: EmacsInt) -> Lisp_Object;
    pub fn make_specified_string(
        contents: *const c_char,
        nchars: ptrdiff_t,
        nbytes: ptrdiff_t,
        multibyte: bool,
    ) -> Lisp_Object;
    pub fn string_to_multibyte(string: Lisp_Object) -> Lisp_Object;

    pub fn preferred_coding_system() -> Lisp_Object;
    pub fn Fcoding_system_p(o: Lisp_Object) -> Lisp_Object;
    pub fn code_convert_string(
        string: Lisp_Object,
        coding_system: Lisp_Object,
        dst_object: Lisp_Object,
        encodep: bool,
        nocopy: bool,
        norecord: bool,
    ) -> Lisp_Object;
    pub fn validate_subarray(
        array: Lisp_Object,
        from: Lisp_Object,
        to: Lisp_Object,
        size: ptrdiff_t,
        ifrom: &mut ptrdiff_t,
        ito: &mut ptrdiff_t,
    );
    pub fn string_char_to_byte(string: Lisp_Object, char_index: ptrdiff_t)
                               -> ptrdiff_t;

    pub fn record_unwind_current_buffer();
    pub fn set_buffer_internal(buffer: *const c_void); // TODO: buffer*
    pub fn make_buffer_string(
        start: ptrdiff_t,
        end: ptrdiff_t,
        props: bool,
    ) -> Lisp_Object;

    pub fn check_obarray(obarray: Lisp_Object) -> Lisp_Object;
    pub fn check_vobarray() -> Lisp_Object;
    pub fn intern_driver(
        string: Lisp_Object,
        obarray: Lisp_Object,
        index: Lisp_Object,
    ) -> Lisp_Object;
    pub fn oblookup(
        obarray: Lisp_Object,
        s: *const c_char,
        size: ptrdiff_t,
        size_bytes: ptrdiff_t,
    ) -> Lisp_Object;

    pub fn SYMBOL_NAME(s: Lisp_Object) -> Lisp_Object;
    pub fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const c_void);
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: c_int,
        ht: Lisp_Object,
    ) -> bool;

    pub fn allocate_pseudovector(
        vecsize: c_int,
        offset1: c_int,
        offset2: c_int,
        pvec_type: PseudovecType,
    ) -> *mut Lisp_Vector;
}
