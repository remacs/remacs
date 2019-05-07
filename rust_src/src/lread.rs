//! Lisp parsing and input streams.

use field_offset::FieldOffset;
use libc;
use std::ffi::CString;
use std::ptr;

use errno::errno;

use remacs_macros::lisp_fn;

use crate::{
    data::{
        Lisp_Boolfwd, Lisp_Buffer_Objfwd, Lisp_Fwd, Lisp_Fwd_Bool, Lisp_Fwd_Buffer_Obj,
        Lisp_Fwd_Int, Lisp_Fwd_Kboard_Obj, Lisp_Fwd_Obj, Lisp_Intfwd, Lisp_Kboard_Objfwd,
        Lisp_Objfwd,
    },
    eval::unbind_to,
    lisp::LispObject,
    multibyte::{char_resolve_modifier_mask, LispSymbolOrString},
    obarray::{check_obarray, intern, intern_c_string_1, LispObarrayRef},
    remacs_sys,
    remacs_sys::infile,
    remacs_sys::{
        block_input, build_string, getc_unlocked, maybe_quit, oblookup_last_bucket_number,
        read_filtered_event, read_internal_start, readevalloop, specbind, staticpro,
        symbol_redirect, unblock_input,
    },
    remacs_sys::{globals, EmacsInt},
    remacs_sys::{Qeval_buffer_list, Qnil, Qread_char, Qstandard_output, Qsymbolp},
    symbols::LispSymbolRef,
    threads::{c_specpdl_index, ThreadState},
};

#[cfg(target_os = "macos")]
extern "C" {
    #[link_name = "\u{1}_clearerr_unlocked"]
    pub fn clearerr_unlocked(arg1: *mut crate::remacs_sys::FILE);
}

#[cfg(target_os = "macos")]
extern "C" {
    #[link_name = "\u{1}_ferror_unlocked"]
    pub fn ferror_unlocked(arg1: *mut crate::remacs_sys::FILE) -> ::libc::c_int;
}

#[cfg(not(target_os = "macos"))]
use crate::remacs_sys::{clearerr_unlocked, ferror_unlocked};

// Define an "integer variable"; a symbol whose value is forwarded to a
// C variable of type EMACS_INT.  Sample call (with "xx" to fool make-docfile):
// DEFxxVAR_INT ("emacs-priority", &emacs_priority, "Documentation");
#[no_mangle]
pub unsafe extern "C" fn defvar_int(
    i_fwd: *mut Lisp_Intfwd,
    namestring: *const libc::c_char,
    address: *mut EmacsInt,
) {
    (*i_fwd).ty = Lisp_Fwd_Int;
    (*i_fwd).intvar = address;
    let sym: LispSymbolRef =
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t).into();
    sym.set_declared_special(true);
    sym.set_redirect(symbol_redirect::SYMBOL_FORWARDED);
    sym.set_fwd(i_fwd as *mut Lisp_Fwd);
}

// Similar but define a variable whose value is t if address contains 1,
// nil if address contains 0.
#[no_mangle]
pub unsafe extern "C" fn defvar_bool(
    b_fwd: *mut Lisp_Boolfwd,
    namestring: *const libc::c_char,
    address: *mut bool,
) {
    (*b_fwd).ty = Lisp_Fwd_Bool;
    (*b_fwd).boolvar = address;
    let sym: LispSymbolRef =
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t).into();
    sym.set_declared_special(true);
    sym.set_redirect(symbol_redirect::SYMBOL_FORWARDED);
    sym.set_fwd(b_fwd as *mut Lisp_Fwd);
}

/// Similar but define a variable whose value is the Lisp Object stored
/// at address.  Two versions: with and without gc-marking of the C
/// variable.  The nopro version is used when that variable will be
/// gc-marked for some other reason, since marking the same slot twice
/// can cause trouble with strings.
#[no_mangle]
pub unsafe extern "C" fn defvar_lisp_nopro(
    o_fwd: *mut Lisp_Objfwd,
    namestring: *const libc::c_char,
    address: *mut LispObject,
) {
    (*o_fwd).ty = Lisp_Fwd_Obj;
    (*o_fwd).objvar = address;
    let sym: LispSymbolRef =
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t).into();
    sym.set_declared_special(true);
    sym.set_redirect(symbol_redirect::SYMBOL_FORWARDED);
    sym.set_fwd(o_fwd as *mut Lisp_Fwd);
}

#[no_mangle]
pub unsafe extern "C" fn defvar_lisp(
    o_fwd: *mut Lisp_Objfwd,
    namestring: *const libc::c_char,
    address: *mut LispObject,
) {
    defvar_lisp_nopro(o_fwd, namestring, address);
    staticpro(address);
}

/// Similar but define a variable whose value is the Lisp Object stored
/// at a particular offset in the current kboard object.
#[no_mangle]
pub unsafe extern "C" fn defvar_kboard(
    ko_fwd: *mut Lisp_Kboard_Objfwd,
    namestring: *const libc::c_char,
    offset: i32,
) {
    defvar_kboard_offset(
        ko_fwd,
        namestring,
        FieldOffset::<remacs_sys::kboard, LispObject>::new_from_offset(offset as usize),
    )
}

pub unsafe fn defvar_kboard_offset(
    ko_fwd: *mut Lisp_Kboard_Objfwd,
    namestring: *const libc::c_char,
    offset: FieldOffset<remacs_sys::kboard, LispObject>,
) {
    (*ko_fwd).ty = Lisp_Fwd_Kboard_Obj;
    (*ko_fwd).offset = offset;
    let sym: LispSymbolRef =
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t).into();
    sym.set_declared_special(true);
    sym.set_redirect(symbol_redirect::SYMBOL_FORWARDED);
    sym.set_fwd(ko_fwd as *mut Lisp_Fwd);
}

#[no_mangle]
pub unsafe extern "C" fn defvar_per_buffer(
    bo_fwd: *mut Lisp_Buffer_Objfwd,
    namestring: *const libc::c_char,
    offset: FieldOffset<remacs_sys::Lisp_Buffer, LispObject>,
    predicate: LispObject,
) {
    defvar_per_buffer_offset(bo_fwd, namestring, offset, predicate);
}

pub unsafe fn defvar_per_buffer_offset(
    bo_fwd: *mut Lisp_Buffer_Objfwd,
    namestring: *const libc::c_char,
    offset: FieldOffset<remacs_sys::Lisp_Buffer, LispObject>,
    predicate: LispObject,
) {
    (*bo_fwd).ty = Lisp_Fwd_Buffer_Obj;
    (*bo_fwd).offset = offset;
    (*bo_fwd).predicate = predicate;
    let sym: LispSymbolRef =
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t).into();
    sym.set_declared_special(true);
    sym.set_redirect(symbol_redirect::SYMBOL_FORWARDED);
    sym.set_fwd(bo_fwd as *mut Lisp_Fwd);
    let local = offset.apply_mut(&mut remacs_sys::buffer_local_symbols);
    *local = sym.into();
    let flags = offset.apply(&remacs_sys::buffer_local_flags);
    if flags.is_nil() {
        panic!(
            "Did a DEFVAR_PER_BUFFER without initializing
             the corresponding slot of buffer_local_flags."
        );
    }
}

/// Read a byte from stdio. If it has lookahead, use the stored value.
/// If read over network is interrupted, keep trying until read succeeds.
#[no_mangle]
pub unsafe extern "C" fn readbyte_from_stdio() -> i32 {
    let file = &mut *infile;

    // If infile has lookahead, use stored value
    if file.lookahead > 0 {
        file.lookahead -= 1;
        return file.buf[file.lookahead as usize].into();
    }

    let instream = file.stream;

    block_input();

    // Interrupted read have been observed while reading over the network.
    let c = loop {
        let c = getc_unlocked(instream);
        if c == libc::EOF && errno().0 == libc::EINTR && ferror_unlocked(instream) > 0 {
            unblock_input();
            maybe_quit();
            block_input();
            clearerr_unlocked(instream);
        } else {
            break c;
        }
    };

    unblock_input();

    if c != libc::EOF {
        c
    } else {
        -1
    }
}

/// Read one Lisp expression as text from STREAM, return as Lisp object.
/// If STREAM is nil, use the value of `standard-input' (which see).
/// STREAM or the value of `standard-input' may be:
///  a buffer (read from point and advance it)
///  a marker (read from where it points and advance it)
///  a function (call it with no arguments for each character,
///      call it with a char as argument to push a char back)
///  a string (takes text from string, starting at the beginning)
///  t (read text line using minibuffer and use it, or read from
///     standard input in batch mode).
#[lisp_fn(min = "0")]
pub fn read(stream: LispObject) -> LispObject {
    // This function ends with a call either to read_internal_start or
    // read-minibuffer.
    //
    // read_internal_start will be called in two circumstances:
    //   1) stream is something other than t, nil, or 'read-char;
    //   2) stream is nil and standard-input is something other than t
    //      or 'read-char.
    // In all other cases, read-minibuffer will be called.

    let input = if stream.is_not_nil() {
        stream
    } else {
        unsafe { globals.Vstandard_input }
    };

    if input.is_t() || input.eq(Qread_char) {
        let cs = CString::new("Lisp expression: ").unwrap();
        call!(intern("read-minibuffer").into(), unsafe {
            build_string(cs.as_ptr())
        })
    } else {
        unsafe { read_internal_start(input, Qnil, Qnil) }
    }
}

/// Don't use this yourself.
#[lisp_fn]
pub fn get_file_char() -> EmacsInt {
    if unsafe { infile }.is_null() {
        error!("get-file-char misused");
    }
    unsafe { readbyte_from_stdio().into() }
}

/// Execute the region as Lisp code.
/// When called from programs, expects two arguments,
/// giving starting and ending indices in the current buffer
/// of the text to be executed.
/// Programs can pass third argument PRINTFLAG which controls output:
///  a value of nil means discard it; anything else is stream for printing it.
///  See Info node `(elisp)Output Streams' for details on streams.
/// Also the fourth argument READ-FUNCTION, if non-nil, is used
/// instead of `read' to read each expression.  It gets one argument
/// which is the input stream for reading characters.
///
/// This function does not move point.
#[lisp_fn(min = "2", intspec = "r")]
pub fn eval_region(
    start: LispObject,
    end: LispObject,
    printflag: LispObject,
    read_function: LispObject,
) {
    // FIXME: Do the eval-sexp-add-defvars dance!
    let count = c_specpdl_index();
    let cur_buf = ThreadState::current_buffer_unchecked();
    let cur_buf_obj = cur_buf.into();

    let tem = if printflag.is_nil() {
        Qsymbolp
    } else {
        printflag
    };
    unsafe {
        specbind(Qstandard_output, tem);
        specbind(
            Qeval_buffer_list,
            (cur_buf_obj, globals.Veval_buffer_list).into(),
        );

        // `readevalloop' calls functions which check the type of start and end.
        readevalloop(
            cur_buf_obj,
            ptr::null_mut(),
            cur_buf.filename(),
            printflag.is_not_nil(),
            Qnil,
            read_function,
            start,
            end,
        );
        unbind_to(count, Qnil);
    }
}

/// Read a character from the command input (keyboard or macro).
/// It is returned as a number.
/// If the character has modifiers, they are resolved and reflected to the
/// character code if possible (e.g. C-SPC -> 0).
///
/// If the user generates an event which is not a character (i.e. a mouse
/// click or function key event), `read-char' signals an error.  As an
/// exception, switch-frame events are put off until non-character events
/// can be read.
/// If you want to read non-character events, or ignore them, call
/// `read-event' or `read-char-exclusive' instead.
///
/// If the optional argument PROMPT is non-nil, display that as a prompt.
/// If the optional argument INHERIT-INPUT-METHOD is non-nil and some
/// input method is turned on in the current buffer, that input method
/// is used for reading a character.
/// If the optional argument SECONDS is non-nil, it should be a number
/// specifying the maximum number of seconds to wait for input.  If no
/// input arrives in that time, return nil.  SECONDS may be a
/// floating-point value.
#[lisp_fn(min = "0")]
pub fn read_char(
    prompt: LispObject,
    inherit_input_method: LispObject,
    seconds: LispObject,
) -> Option<EmacsInt> {
    if !prompt.is_nil() {
        message_with_string!("%s", prompt, false);
    }

    let val =
        unsafe { read_filtered_event(true, true, true, !inherit_input_method.is_nil(), seconds) };

    match val.into() {
        Some(num) => Some(char_resolve_modifier_mask(num)),
        None => None,
    }
}
def_lisp_sym!(Qread_char, "read-char");

/// Read an event object from the input stream.
/// If the optional argument PROMPT is non-nil, display that as a prompt.
/// If the optional argument INHERIT-INPUT-METHOD is non-nil and some
/// input method is turned on in the current buffer, that input method
/// is used for reading a character.
/// If the optional argument SECONDS is non-nil, it should be a number
/// specifying the maximum number of seconds to wait for input.  If no
/// input arrives in that time, return nil.  SECONDS may be a
/// floating-point value.
#[lisp_fn(min = "0")]
pub fn read_event(
    prompt: LispObject,
    inherit_input_method: LispObject,
    seconds: LispObject,
) -> LispObject {
    if !prompt.is_nil() {
        message_with_string!("%s", prompt, false);
    }

    unsafe { read_filtered_event(false, false, false, !inherit_input_method.is_nil(), seconds) }
}

/// Read a character from the command input (keyboard or macro).
/// It is returned as a number.  Non-character events are ignored.
/// If the character has modifiers, they are resolved and reflected to the
/// character code if possible (e.g. C-SPC -> 0).
///
/// If the optional argument PROMPT is non-nil, display that as a prompt.
/// If the optional argument INHERIT-INPUT-METHOD is non-nil and some
/// input method is turned on in the current buffer, that input method
/// is used for reading a character.
/// If the optional argument SECONDS is non-nil, it should be a number
/// specifying the maximum number of seconds to wait for input.  If no
/// input arrives in that time, return nil.  SECONDS may be a
/// floating-point value.
#[lisp_fn(min = "0")]
pub fn read_char_exclusive(
    prompt: LispObject,
    inherit_input_method: LispObject,
    seconds: LispObject,
) -> Option<EmacsInt> {
    if !prompt.is_nil() {
        message_with_string!("%s", prompt, false);
    }

    let val =
        unsafe { read_filtered_event(true, true, false, !inherit_input_method.is_nil(), seconds) };

    match val.into() {
        Some(num) => Some(char_resolve_modifier_mask(num)),
        None => None,
    }
}

/// Delete the symbol named NAME, if any, from OBARRAY.
/// The value is t if a symbol was found and deleted, nil otherwise.
/// NAME may be a string or a symbol.  If it is a symbol, that symbol
/// is deleted, if it belongs to OBARRAY--no other symbol is deleted.
/// OBARRAY, if nil, defaults to the value of the variable `obarray'.
/// usage: (unintern NAME OBARRAY)
#[lisp_fn(min = "1")]
pub fn unintern(name: LispSymbolOrString, obarray: Option<LispObarrayRef>) -> bool {
    let obarray = obarray.unwrap_or_else(LispObarrayRef::global);
    let obarray: LispObarrayRef = check_obarray(obarray.into()).into();

    let tem = obarray.lookup(name);
    if tem.is_integer() {
        return false;
    }
    // If arg was a symbol, don't delete anything but that symbol itself.
    if name.is_symbol() && name != tem {
        return false;
    }

    let mut temp: LispSymbolRef = tem.into();
    temp.set_uninterned();

    let hash = unsafe { oblookup_last_bucket_number };

    let mut obarray = LispObject::from(obarray).as_vector_or_error();
    let symbol: LispSymbolRef = obarray.get(hash).into();

    if symbol == temp {
        match symbol.get_next() {
            Some(sym) => obarray.set(hash, sym.into()),
            None => obarray.set(hash, LispObject::from_natnum(0)),
        };
    } else {
        for tail in symbol.iter() {
            if let Some(following) = tail.get_next() {
                if following == temp {
                    let next = following.get_next();
                    tail.set_next(next);
                    break;
                }
            }
        }
    }

    true
}

include!(concat!(env!("OUT_DIR"), "/lread_exports.rs"));
