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
use mock_derive::mock;
use remacs_sys::*;

#[no_mangle]
pub static mut lispsym: Lisp_Object = 0;

#[mock]
extern "C" {
    pub fn Faref(array: Lisp_Object, idx: Lisp_Object) -> Lisp_Object;
    pub fn Fcons(car: Lisp_Object, cdr: Lisp_Object) -> Lisp_Object;
    pub fn Fsignal(error_symbol: Lisp_Object, data: Lisp_Object) ;
    pub fn Fcopy_sequence(seq: Lisp_Object) -> Lisp_Object;
    pub fn Ffind_operation_coding_system(nargs: ptrdiff_t, args: *mut Lisp_Object) -> Lisp_Object;
    pub fn Flocal_variable_p(variable: Lisp_Object, buffer: Lisp_Object) -> Lisp_Object;
    pub fn Flookup_key(
        keymap: Lisp_Object,
        key: Lisp_Object,
        accept_default: Lisp_Object,
    ) -> Lisp_Object;
    pub fn Ffuncall(nargs: ptrdiff_t, args: *mut Lisp_Object) -> Lisp_Object;
    pub fn Fpurecopy(string: Lisp_Object) -> Lisp_Object;
    pub fn Fmapcar(function: Lisp_Object, sequence: Lisp_Object) -> Lisp_Object;
    pub fn Fset(symbol: Lisp_Object, newval: Lisp_Object) -> Lisp_Object;
    pub fn make_float(float_value: c_double) -> Lisp_Object;
    pub fn make_string(s: *const c_char, length: ptrdiff_t) -> Lisp_Object;
    pub fn make_lisp_ptr(ptr: *const c_void, ty: Lisp_Type) -> Lisp_Object;
    pub fn make_lisp_symbol(ptr: *mut Lisp_Symbol) -> Lisp_Object;
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
    pub fn set_buffer_internal(buffer: *mut Lisp_Buffer);
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

    pub fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const c_void);
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: c_int,
        ht: Lisp_Object,
    ) -> bool;

    // These signal an error, therefore are marked as non-returning.
    pub fn nsberror(spec: Lisp_Object) ;

    pub fn emacs_abort() ;

    pub fn base64_encode_1(
        from: *const c_char,
        to: *mut c_char,
        length: ptrdiff_t,
        line_break: bool,
        multibyte: bool,
    ) -> ptrdiff_t;
    pub fn base64_decode_1(
        from: *const c_char,
        to: *mut c_char,
        length: ptrdiff_t,
        multibyte: bool,
        nchars_return: *mut ptrdiff_t,
    ) -> ptrdiff_t;

    pub fn allocate_pseudovector(
        vecsize: c_int,
        offset1: c_int,
        offset2: c_int,
        pvec_type: PseudovecType,
    ) -> *mut Lisp_Vector;

    pub fn extract_data_from_object(
        spec: Lisp_Object,
        start_byte: *mut ptrdiff_t,
        end_byte: *mut ptrdiff_t,
    ) -> *mut c_char;

    pub fn hash_lookup(
        h: *mut Lisp_Hash_Table,
        key: Lisp_Object,
        hash: *mut EmacsUint,
    ) -> ptrdiff_t;

    pub fn hash_put(
        h: *mut Lisp_Hash_Table,
        key: Lisp_Object,
        value: Lisp_Object,
        hash: EmacsUint,
    ) -> ptrdiff_t;
    pub fn hash_clear(h: *mut Lisp_Hash_Table);

    pub fn gc_aset(array: Lisp_Object, idx: ptrdiff_t, val: Lisp_Object);

    pub fn hash_remove_from_table(h: *mut Lisp_Hash_Table, key: Lisp_Object);
    pub fn set_point_both(charpos: ptrdiff_t, bytepos: ptrdiff_t);
    pub fn set_point(charpos: ptrdiff_t);
    pub fn Fline_beginning_position(n: Lisp_Object) -> Lisp_Object;
    pub fn buf_charpos_to_bytepos(buffer: *const Lisp_Buffer, charpos: ptrdiff_t) -> ptrdiff_t;

    pub fn Finsert_char(
        character: Lisp_Object,
        count: Lisp_Object,
        inherit: Lisp_Object,
    ) -> Lisp_Object;

    pub fn wait_reading_process_output(
        time_limit: intmax_t,
        nsecs: c_int,
        read_kbd: c_int,
        do_display: bool,
        wait_for_cell: Lisp_Object,
        wait_proc: *const Lisp_Process,
        just_wait_proc: c_int,
    ) -> c_int;

    pub fn dtotimespec(sec: c_double) -> timespec;
    pub fn current_timespec() -> timespec;
    pub fn timespec_sub(a: timespec, b: timespec) -> timespec;
    pub fn timespec_add(a: timespec, b: timespec) -> timespec;

    pub fn current_column() -> Lisp_Object;

    pub fn Fadd_text_properties(
        start: Lisp_Object,
        end: Lisp_Object,
        properties: Lisp_Object,
        object: Lisp_Object,
    ) -> Lisp_Object;

    pub fn find_symbol_value(symbol: Lisp_Object) -> Lisp_Object;
    pub fn symbol_is_interned(symbol: *const Lisp_Symbol) -> bool;
    pub fn symbol_is_alias(symbol: *const Lisp_Symbol) -> bool;
    pub fn symbol_is_constant(symbol: *const Lisp_Symbol) -> bool;
    pub fn misc_get_ty(any: *const Lisp_Misc_Any) -> u16;
    pub fn is_minibuffer(w: *const Lisp_Window) -> bool;
    pub fn xmalloc(size: size_t) -> *mut c_void;

    pub fn Fmapc(function: Lisp_Object, sequence: Lisp_Object) -> Lisp_Object;

    pub fn Fpos_visible_in_window_p(
        pos: Lisp_Object,
        window: Lisp_Object,
        partially: Lisp_Object,
    ) -> Lisp_Object;
    pub fn Fposn_at_x_y(
        x: Lisp_Object,
        y: Lisp_Object,
        frame_or_window: Lisp_Object,
        whole: Lisp_Object,
    ) -> Lisp_Object;
    pub fn find_before_next_newline(
        from: ptrdiff_t,
        to: ptrdiff_t,
        cnt: ptrdiff_t,
        bytepos: *mut ptrdiff_t,
    ) -> ptrdiff_t;
    pub fn Fconstrain_to_field(
        new_pos: Lisp_Object,
        old_pos: Lisp_Object,
        escape_from_edge: Lisp_Object,
        only_in_line: Lisp_Object,
        inhibit_capture_property: Lisp_Object,
    ) -> Lisp_Object;
    pub fn Fline_end_position(n: Lisp_Object) -> Lisp_Object;
    pub fn get_process(name: Lisp_Object) -> Lisp_Object;
    pub fn update_status(p: *const Lisp_Process);
    pub fn setup_process_coding_systems(process: Lisp_Object);
    pub fn send_process(
        process: Lisp_Object,
        buf: *const c_char,
        len: ptrdiff_t,
        object: Lisp_Object,
    );
    pub fn STRING_BYTES(s: *const Lisp_String) -> ptrdiff_t;
}

macro_rules! mock_float {
    () => { mock_float!(0.0) };
    
    ($f: expr) => {{
        // Fake an allocated float by just putting it on the heap and leaking it.
        let boxed = Box::new(::remacs_sys::Lisp_Float {
            data: unsafe { ::std::mem::transmute($f) },
        });
        let raw = ::lisp::ExternalPtr::new(Box::into_raw(boxed));
        ::lisp::LispObject::tag_ptr(raw, ::remacs_sys::Lisp_Type::Lisp_Float)
    }};
}

macro_rules! mock_unibyte_string {
    () => { mock_unibyte_string!("") };
    ($string: expr) => {{
        let strcopy = ::std::ffi::CString::new($string).unwrap();
        let len = strcopy.as_bytes().len() as ::libc::ptrdiff_t;
        let boxed = Box::new(::remacs_sys::Lisp_String {
            size: len,
            size_byte: -1,
            intervals: ::std::ptr::null_mut(),
            data: strcopy.into_raw(),
        });

        let ptr = ::lisp::ExternalPtr::new(Box::into_raw(boxed));
        ::lisp::LispObject::tag_ptr(ptr, ::remacs_sys::Lisp_Type::Lisp_String)
    }};
}

macro_rules! mock_multibyte_string {
    () => { mock_multibyte_string!("") };
    ($string: expr) => {{
        let strcopy = ::std::ffi::CString::new($string).unwrap();
        let len = strcopy.as_bytes().len() as ::libc::ptrdiff_t;
        let boxed = Box::new(::remacs_sys::Lisp_String {
            size: len,
            size_byte: len,
            intervals: ::std::ptr::null_mut(),
            data: strcopy.into_raw(),
        });

        let ptr = ::lisp::ExternalPtr::new(Box::into_raw(boxed));
        ::lisp::LispObject::tag_ptr(ptr, ::remacs_sys::Lisp_Type::Lisp_String)
    }};
}

macro_rules! assert_t {
    ($arg: expr) => {{ assert!($arg == ::lisp::LispObject::constant_t()); }};
}

macro_rules! assert_nil {
    ($arg: expr) => {{ assert!($arg == ::lisp::LispObject::constant_nil()); }};
}

