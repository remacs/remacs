use libc;
use lisp::LispObject;
use obarray::intern_c_string_1;
use remacs_sys::{EmacsInt, Lisp_Boolfwd, Lisp_Fwd, Lisp_Fwd_Bool, Lisp_Fwd_Int,
                 Lisp_Fwd_Kboard_Obj, Lisp_Fwd_Obj, Lisp_Intfwd, Lisp_Kboard_Objfwd, Lisp_Objfwd};
use remacs_sys::SYMBOL_FORWARDED;
use remacs_sys::staticpro;

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
