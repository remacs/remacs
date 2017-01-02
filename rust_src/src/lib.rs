extern crate libc;

mod lisp;
mod marker;
mod eval;
mod floatfns;

use std::os::raw::c_char;
use lisp::{LispObject, LispSubr, PvecType, defsubr, make_number, PSEUDOVECTOR_AREA_BITS, XINT,
           VectorLikeHeader, Qt, Qarith_error};
use eval::xsignal0;

#[no_mangle]
pub unsafe extern "C" fn rust_return_t() -> LispObject {
    println!("hello from rust!");
    Qt
}

#[no_mangle]
#[allow(unused_variables)]
pub unsafe extern "C" fn rust_mod(x: LispObject, y: LispObject) -> LispObject {
    let x = lisp::check_number_coerce_marker(x);
    let y = lisp::check_number_coerce_marker(y);

    if lisp::FLOATP(x) || lisp::FLOATP(y) {
        return floatfns::fmod_float(x, y);
    }

    let mut i1 = XINT(x);
    let i2 = XINT(y);

    if i2 == 0 {
        xsignal0(Qarith_error);
    }

    i1 %= i2;

    // Ensure that the remainder has the correct sign.
    if i2 < 0 && i1 > 0 || i2 > 0 && i1 < 0 {
        i1 += i2
    }

    make_number(i1)
}

#[no_mangle]
#[allow(non_snake_case)]
pub unsafe extern "C" fn rust_init_syms() {
    println!("init rust syms start");

    // TODO: to be consistent with Emacs, we should consider
    // statically allocating our LispSubr values. However:
    //
    // * we can't call .as_ptr() for a static value
    // * Rust would force us to define Sync on LispSubr
    //   see http://stackoverflow.com/a/28116557/509706
    // * the lazy_static crate might be a good fit, but
    //   we'd need to deref so make sure the data is
    //   initialised.
    //
    // TODO: this is blindly hoping we have the correct alignment.
    // We should ensure we have GCALIGNMENT (8 bytes).
    let mut Srust_return_t = Box::new(LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        // TODO: rust_return_t as standard Emacs naming.
        function: (rust_return_t as *mut libc::c_void),
        min_args: 0,
        max_args: 0,
        symbol_name: ("return-t\0".as_ptr()) as *const c_char,
        intspec: "\0".as_ptr() as *const c_char,
        doc: ("hello world\0".as_ptr()) as *const c_char,
    });

    defsubr(Srust_return_t.as_mut());

    // Shameful kludge to ensure Srust_return_t lives long enough.
    std::mem::forget(Srust_return_t);

    let mut Srust_mod = Box::new(LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (rust_mod as *mut libc::c_void),
        min_args: 2,
        max_args: 2,
        symbol_name: ("rust-mod\0".as_ptr()) as *const c_char,
        intspec: "\0".as_ptr() as *const c_char,
        doc: ("Calculate mod in rust\0".as_ptr()) as *const c_char,
    });

    defsubr(Srust_mod.as_mut());

    // Shameful kludge to ensure Srust_mod lives long enough.
    std::mem::forget(Srust_mod);

    println!("init rust syms end");
}
