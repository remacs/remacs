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

use data::{Lisp_Boolfwd, Lisp_Buffer_Objfwd, Lisp_Fwd, Lisp_Intfwd, Lisp_Kboard_Objfwd,
           Lisp_Objfwd};
use lisp::LispObject;

include!(concat!(env!("OUT_DIR"), "/definitions.rs"));

type Lisp_Object = LispObject;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
include!(concat!(env!("OUT_DIR"), "/globals.rs"));

pub const VAL_MAX: EmacsInt = (EMACS_INT_MAX >> (GCTYPEBITS - 1));
pub const VALMASK: EmacsInt = [VAL_MAX, -(1 << GCTYPEBITS)][USE_LSB_TAG as usize];
pub const INTMASK: EmacsInt = (EMACS_INT_MAX >> (Lisp_Bits::INTTYPEBITS - 1));
pub const PSEUDOVECTOR_FLAG: usize = 0x4000000000000000;

// These signal an error, therefore are marked as non-returning.
extern "C" {
    pub fn circular_list(tail: Lisp_Object) -> !;
    pub fn wrong_type_argument(predicate: Lisp_Object, value: Lisp_Object) -> !;
    // defined in eval.c, where it can actually take an arbitrary
    // number of arguments.
    // TODO: define a Rust version of this that uses Rust strings.
    pub fn error(m: *const u8, ...) -> !;
    pub fn nsberror(spec: Lisp_Object) -> !;
    pub fn emacs_abort() -> !;
    pub fn Fsignal(error_symbol: Lisp_Object, data: Lisp_Object) -> !;
    pub fn memory_full(nbytes: libc::size_t) -> !;
    pub fn bitch_at_user() -> !;
    pub fn wrong_choice(choice: LispObject, wrong: LispObject) -> !;
    pub fn wrong_range(min: LispObject, max: LispObject, wrong: LispObject) -> !;
}

/// Type of comparison for `internal_equal()`.
#[repr(C)]
pub enum EqualKind {
    NoQuit,
    Plain,
    IncludingProperties,
}

// bindgen apparently misses these, for various reasons
extern "C" {
    // these weren't declared in a header, for example
    pub static Vprocess_alist: Lisp_Object;
    pub fn hash_clear(h: *mut Lisp_Hash_Table);
    pub fn internal_equal(
        o1: Lisp_Object,
        o2: Lisp_Object,
        kind: EqualKind,
        depth: libc::c_int,
        ht: Lisp_Object,
    ) -> bool;
    pub fn uniprop_table_uncompress(table: Lisp_Object, idx: u32) -> Lisp_Object;
    pub fn update_buffer_defaults(objvar: *mut LispObject, newval: LispObject);
    pub fn find_field(
        pos: LispObject,
        merge_at_boundary: LispObject,
        beg_limit: LispObject,
        beg: *mut ptrdiff_t,
        end_limit: LispObject,
        end: *mut ptrdiff_t,
    );
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
    pub fn mset_charpos(m: *const Lisp_Marker, charpos: ptrdiff_t);
    pub fn mset_bytepos(m: *const Lisp_Marker, bytepos: ptrdiff_t);
    pub static initial_obarray: LispObject;
    pub fn scan_lists(
        from: EmacsInt,
        count: EmacsInt,
        depth: EmacsInt,
        sexpflag: bool,
    ) -> LispObject;
    pub fn is_minibuffer(w: *const Lisp_Window) -> bool;
    pub static minibuf_prompt: LispObject;
    pub fn add_process_read_fd(fd: libc::c_int);
    #[cfg(windows)]
    pub fn file_attributes_c(filename: LispObject, id_format: LispObject) -> LispObject;
    #[cfg(unix)]
    pub fn file_attributes_c_internal(
        name: *const c_char,
        directory: LispObject,
        filename: LispObject,
        id_format: LispObject,
    ) -> LispObject;
    #[cfg(unix)]
    pub fn filemode_string(f: LispObject) -> LispObject;

}

// Largest and smallest numbers that can be represented as fixnums in
// Emacs lisp.
pub const MOST_POSITIVE_FIXNUM: EmacsInt = EMACS_INT_MAX >> Lisp_Bits::INTTYPEBITS as u32;
pub const MOST_NEGATIVE_FIXNUM: EmacsInt = (-1 - MOST_POSITIVE_FIXNUM);

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
pub type Lisp_Window = window;
pub type Lisp_Frame = frame;

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
