use libc;
use lisp::LispObject;
use obarray::intern_c_string_1;
use remacs_sys;
use remacs_sys::{EmacsInt};
use remacs_sys::SYMBOL_FORWARDED;
use remacs_sys::staticpro;

use data::{Lisp_Boolfwd, Lisp_Fwd, Lisp_Fwd_Bool, Lisp_Fwd_Int, Lisp_Buffer_Objfwd, Lisp_Fwd_Buffer_Obj,
           Lisp_Fwd_Kboard_Obj, Lisp_Fwd_Obj, Lisp_Intfwd, Lisp_Kboard_Objfwd, Lisp_Objfwd};
use field_offset::FieldOffset;

// Define an "integer variable"; a symbol whose value is forwarded to a
// C variable of type EMACS_INT.  Sample call (with "xx" to fool make-docfile):
// DEFxxVAR_INT ("emacs-priority", &emacs_priority, "Documentation");
#[no_mangle]
pub extern "C" fn defvar_int(
    i_fwd: *mut Lisp_Intfwd,
    namestring: *const libc::c_schar,
    address: *mut EmacsInt,
) {
    unsafe {
        (*i_fwd).ty = Lisp_Fwd_Int;
        (*i_fwd).intvar = address;
    }
    let sym = unsafe {
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t)
            .as_symbol_or_error()
    };
    sym.set_declared_special(true);
    sym.set_redirect(SYMBOL_FORWARDED);
    sym.set_fwd(i_fwd as *mut Lisp_Fwd);
}

/* Similar but define a variable whose value is t if address contains 1,
   nil if address contains 0.  */
#[no_mangle]
pub extern "C" fn defvar_bool(
    b_fwd: *mut Lisp_Boolfwd,
    namestring: *const libc::c_schar,
    address: *mut bool,
) {
    unsafe {
        (*b_fwd).ty = Lisp_Fwd_Bool;
        (*b_fwd).boolvar = address;
    }
    let sym = unsafe {
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t)
            .as_symbol_or_error()
    };
    sym.set_declared_special(true);
    sym.set_redirect(SYMBOL_FORWARDED);
    sym.set_fwd(b_fwd as *mut Lisp_Fwd);
}

/// Similar but define a variable whose value is the Lisp Object stored
/// at address.  Two versions: with and without gc-marking of the C
/// variable.  The nopro version is used when that variable will be
/// gc-marked for some other reason, since marking the same slot twice
/// can cause trouble with strings.
#[no_mangle]
pub extern "C" fn defvar_lisp_nopro(
    o_fwd: *mut Lisp_Objfwd,
    namestring: *const libc::c_schar,
    address: *mut LispObject,
) {
    unsafe {
        (*o_fwd).ty = Lisp_Fwd_Obj;
        (*o_fwd).objvar = address;
    }
    let sym = unsafe {
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t)
            .as_symbol_or_error()
    };
    sym.set_declared_special(true);
    sym.set_redirect(SYMBOL_FORWARDED);
    sym.set_fwd(o_fwd as *mut Lisp_Fwd);
}

#[no_mangle]
pub extern "C" fn defvar_lisp(
    o_fwd: *mut Lisp_Objfwd,
    namestring: *const libc::c_schar,
    address: *mut LispObject,
) {
    defvar_lisp_nopro(o_fwd, namestring, address);
    unsafe { staticpro(address) };
}

/// Similar but define a variable whose value is the Lisp Object stored
/// at a particular offset in the current kboard object.
#[no_mangle]
pub extern "C" fn defvar_kboard(
    ko_fwd: *mut Lisp_Kboard_Objfwd,
    namestring: *const libc::c_schar,
    offset: i32,
) {
    defvar_kboard_offset(ko_fwd, namestring, unsafe { FieldOffset::<remacs_sys::kboard, LispObject>::new_from_offset(offset as usize) })
}

pub fn defvar_kboard_offset(
    ko_fwd: *mut Lisp_Kboard_Objfwd,
    namestring: *const libc::c_schar,
    offset: FieldOffset<remacs_sys::kboard, LispObject>,
) {
    unsafe {
        (*ko_fwd).ty = Lisp_Fwd_Kboard_Obj;
        (*ko_fwd).offset = offset;
    }
    let sym = intern_c_string_1(namestring, unsafe {
        libc::strlen(namestring) as libc::ptrdiff_t
    }).as_symbol_or_error();
    sym.set_declared_special(true);
    sym.set_redirect(SYMBOL_FORWARDED);
    sym.set_fwd(ko_fwd as *mut Lisp_Fwd);
}

#[no_mangle]
pub extern "C" fn defvar_per_buffer(bo_fwd: *mut Lisp_Buffer_Objfwd, namestring: *const libc::c_schar,
                                    offset: FieldOffset<remacs_sys::Lisp_Buffer, LispObject>, predicate: LispObject)
{
    defvar_per_buffer_offset(bo_fwd, namestring, offset, predicate);
}

pub fn defvar_per_buffer_offset(bo_fwd: *mut Lisp_Buffer_Objfwd, namestring: *const libc::c_schar,
                                offset: FieldOffset<remacs_sys::Lisp_Buffer, LispObject>, predicate: LispObject)
{
    unsafe {
        (*bo_fwd).ty = Lisp_Fwd_Buffer_Obj;
        (*bo_fwd).offset = offset;
        (*bo_fwd).predicate = predicate;
    }
    let sym = unsafe {
        intern_c_string_1(namestring, libc::strlen(namestring) as libc::ptrdiff_t)
            .as_symbol_or_error()
    };
    sym.set_declared_special(true);
    sym.set_redirect(SYMBOL_FORWARDED);
    sym.set_fwd(bo_fwd as *mut Lisp_Fwd);
    let local = offset.apply_mut(unsafe { &mut remacs_sys::buffer_local_symbols });
    *local = sym.as_lisp_obj();
    let flags = offset.apply(unsafe { &remacs_sys::buffer_local_flags });
    if flags.is_nil() {
        /* Did a DEFVAR_PER_BUFFER without initializing the corresponding
           slot of buffer_local_flags.  */
        unsafe { remacs_sys::emacs_abort () };
    }
}
