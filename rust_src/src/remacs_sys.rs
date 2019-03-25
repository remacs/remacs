#![allow(unused)]

//! This module contains all FFI declarations.
//!
//! These types and constants are generated at build time to mimic how they are
//! in C:
//!
//! - `EmacsInt`
//! - `EmacsUint`
//! - `EmacsDouble`
//! - `EMACS_INT_MAX`
//! - `EMACS_INT_SIZE`
//! - `EMACS_FLOAT_SIZE`
//! - `GCTYPEBITS`
//! - `USE_LSB_TAG`
//! - `BoolBF`

use libc::{self, c_char, c_void, ptrdiff_t};
use std;

use libc::timespec;
use remacs_lib::current_timespec;

use crate::{
    data::{
        Lisp_Boolfwd, Lisp_Buffer_Objfwd, Lisp_Fwd, Lisp_Intfwd, Lisp_Kboard_Objfwd, Lisp_Objfwd,
    },
    lisp::LispObject,
};

include!("../generated/definitions.rs");

type Lisp_Object = LispObject;

include!("../generated/bindings.rs");
include!("../generated/globals.rs");

pub const VAL_MAX: EmacsInt = (EMACS_INT_MAX >> (GCTYPEBITS - 1));
pub const VALMASK: EmacsInt = [VAL_MAX, -(1 << GCTYPEBITS)][USE_LSB_TAG as usize];
pub const INTMASK: EmacsInt = (EMACS_INT_MAX >> (Lisp_Bits::INTTYPEBITS - 1));
pub const PSEUDOVECTOR_FLAG: usize = 0x4000_0000_0000_0000;

// These signal an error, therefore are marked as non-returning.
extern "C" {
    pub fn circular_list(tail: Lisp_Object) -> !;
    pub fn wrong_type_argument(predicate: Lisp_Object, value: Lisp_Object) -> !;
    // defined in eval.c, where it can actually take an arbitrary
    // number of arguments.
    // TODO: define a Rust version of this that uses Rust strings.
    pub fn error(m: *const u8, ...) -> !;
    pub fn memory_full(nbytes: libc::size_t) -> !;
    pub fn wrong_choice(choice: LispObject, wrong: LispObject) -> !;
    pub fn wrong_range(min: LispObject, max: LispObject, wrong: LispObject) -> !;
}

#[repr(C)]
pub enum BoolVectorOp {
    BoolVectorExclusiveOr,
    BoolVectorUnion,
    BoolVectorIntersection,
    BoolVectorSetDifference,
    BoolVectorSubsetp,
}

// bindgen apparently misses these, for various reasons
extern "C" {
    // these weren't declared in a header, for example
    pub static Vprocess_alist: Lisp_Object;
    pub fn update_buffer_defaults(objvar: *mut LispObject, newval: LispObject);
    pub fn concat(
        nargs: ptrdiff_t,
        args: *mut LispObject,
        target_type: Lisp_Type,
        last_special: bool,
    ) -> LispObject;
    pub fn map_keymap_item(
        fun: map_keymap_function_t,
        args: LispObject,
        key: LispObject,
        val: LispObject,
        data: *const c_void,
    );
    pub fn map_keymap_char_table_item(args: LispObject, key: LispObject, val: LispObject);
    pub static initial_obarray: LispObject;
    pub fn scan_lists(
        from: EmacsInt,
        count: EmacsInt,
        depth: EmacsInt,
        sexpflag: bool,
    ) -> LispObject;
    pub fn read_minibuf(
        map: Lisp_Object,
        initial: Lisp_Object,
        prompt: Lisp_Object,
        expflag: bool,
        histvar: Lisp_Object,
        histpos: Lisp_Object,
        defalt: Lisp_Object,
        allow_props: bool,
        inherit_input_method: bool,
    ) -> Lisp_Object;
    pub static minibuf_prompt: LispObject;
    pub fn add_process_read_fd(fd: libc::c_int);
    pub fn allocate_misc(t: Lisp_Misc_Type) -> LispObject;
    #[cfg(windows)]
    pub fn file_attributes_c(filename: LispObject, id_format: LispObject) -> LispObject;
    pub fn getloadaverage(loadavg: *mut libc::c_double, nelem: libc::c_int) -> libc::c_int;
    #[cfg(unix)]
    pub fn file_attributes_c_internal(
        name: *const c_char,
        directory: LispObject,
        filename: LispObject,
        id_format: LispObject,
    ) -> LispObject;
    #[cfg(unix)]
    pub fn filemode_string(f: LispObject) -> LispObject;

    pub fn unchain_both(b: *mut Lisp_Buffer, ov: LispObject);
    pub fn emacs_get_tty_pgrp(p: *mut Lisp_Process) -> libc::pid_t;
    pub fn update_buffer_properties(start: ptrdiff_t, end: ptrdiff_t);
    pub fn set_window_hscroll(w: *mut Lisp_Window, hscroll: EMACS_INT) -> Lisp_Object;
    pub fn scroll_command(n: Lisp_Object, direction: libc::c_int);
    pub fn bool_vector_binop_driver(
        a: Lisp_Object,
        b: Lisp_Object,
        dest: Lisp_Object,
        op: BoolVectorOp,
    ) -> Lisp_Object;
}

// Max value for the first argument of wait_reading_process_output.
pub const WAIT_READING_MAX: i64 = std::i64::MAX;

// In order to use `lazy_static!` with LispSubr, it must be Sync. Raw
// pointers are not Sync, but it isn't a problem to define Sync if we
// never mutate LispSubr values. If we do, we will need to create
// these objects at runtime, perhaps using forget().
//
// Based on http://stackoverflow.com/a/28116557/509706
unsafe impl Sync for Lisp_Subr {}

pub type Lisp_Buffer = buffer;
pub type Lisp_Font_Object = font;
pub type Lisp_Frame = frame;
pub type Lisp_Glyph = glyph;
pub type Lisp_Terminal = terminal;
pub type Lisp_Window = window;

#[repr(C)]
pub struct Lisp_Vectorlike {
    pub header: vectorlike_header,
    // shouldn't look at the contents without knowing the structure...
}

// No C equivalent.  Generic type for a vectorlike with one or more
// LispObject slots after the header.
#[repr(C)]
pub struct Lisp_Vectorlike_With_Slots {
    pub header: vectorlike_header,
    // actually any number of items... not sure how to express this
    pub contents: __IncompleteArrayField<Lisp_Object>,
}

//// declare this ourselves so that the arg isn't mutable
//extern "C" {
//    pub fn staticpro(arg1: *const Lisp_Object);
//}
