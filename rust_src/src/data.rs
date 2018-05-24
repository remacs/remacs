//! data helpers

use libc::{self, c_int};

use remacs_macros::lisp_fn;
use remacs_sys::{aset_multibyte_string, build_string, emacs_abort, fget_terminal, globals,
                 set_per_buffer_value, update_buffer_defaults, wrong_choice, wrong_range,
                 CHAR_TABLE_SET, CHECK_IMPURE};
use remacs_sys::{EmacsInt, Lisp_Misc_Type, Lisp_Type, PseudovecType};
use remacs_sys::{Fcons, Ffset, Fget, Fpurecopy};
use remacs_sys::{Lisp_Buffer, Lisp_Fwd, Lisp_Fwd_Bool, Lisp_Fwd_Buffer_Obj, Lisp_Fwd_Int,
                 Lisp_Fwd_Kboard_Obj, Lisp_Fwd_Obj, Lisp_Subr_Lang_C, Lisp_Subr_Lang_Rust};
use remacs_sys::{Qargs_out_of_range, Qarrayp, Qautoload, Qbool_vector, Qbuffer, Qchar_table,
                 Qchoice, Qcompiled_function, Qcondition_variable, Qcons,
                 Qcyclic_function_indirection, Qdefalias_fset_function, Qdefun, Qfinalizer,
                 Qfloat, Qfont, Qfont_entity, Qfont_object, Qfont_spec, Qframe,
                 Qfunction_documentation, Qhash_table, Qinteger, Qmany, Qmarker, Qmodule_function,
                 Qmutex, Qnil, Qnone, Qoverlay, Qprocess, Qrange, Qstring, Qsubr, Qsymbol, Qt,
                 Qterminal, Qthread, Qunevalled, Quser_ptr, Qvector, Qwindow,
                 Qwindow_configuration};

use frames::selected_frame;
use keymap::get_keymap;
use lisp::{LispObject, LispSubrRef};
use lisp::{defsubr, is_autoload};
use lists::{get, memq, put};
use math::leq;
use multibyte::{is_ascii, is_single_byte_char};
use obarray::loadhist_attach;
use threads::ThreadState;

/// Find the function at the end of a chain of symbol function indirections.

/// If OBJECT is a symbol, find the end of its function chain and
/// return the value found there.  If OBJECT is not a symbol, just
/// return it.  If there is a cycle in the function chain, signal a
/// cyclic-function-indirection error.
///
/// This is like `Findirect_function`, except that it doesn't signal an
/// error if the chain ends up unbound.
pub fn indirect_function(object: LispObject) -> LispObject {
    let mut tortoise = object;
    let mut hare = object;
    loop {
        if !hare.is_symbol() || hare.is_nil() {
            return hare;
        }
        hare = hare.as_symbol_or_error().get_function();
        if !hare.is_symbol() || hare.is_nil() {
            return hare;
        }
        hare = hare.as_symbol_or_error().get_function();
        tortoise = tortoise.as_symbol_or_error().get_function();
        if hare == tortoise {
            xsignal!(Qcyclic_function_indirection, object);
        }
    }
}

/// Return the function at the end of OBJECT's function chain.
/// If OBJECT is not a symbol, just return it.  Otherwise, follow all
/// function indirections to find the final function binding and return it.
/// Signal a cyclic-function-indirection error if there is a loop in the
/// function chain of symbols.
#[lisp_fn(min = "1", c_name = "indirect_function", name = "indirect-function")]
pub fn indirect_function_lisp(object: LispObject, _noerror: LispObject) -> LispObject {
    match object.as_symbol() {
        None => object,
        Some(symbol) => symbol.get_indirect_function(),
    }
}

/// Return a symbol representing the type of OBJECT.
/// The symbol returned names the object's basic type;
/// for example, (type-of 1) returns `integer'.
#[lisp_fn]
pub fn type_of(object: LispObject) -> LispObject {
    let ty = match object.get_type() {
        Lisp_Type::Lisp_Cons => Qcons,
        Lisp_Type::Lisp_Int0 | Lisp_Type::Lisp_Int1 => Qinteger,
        Lisp_Type::Lisp_Symbol => Qsymbol,
        Lisp_Type::Lisp_String => Qstring,
        Lisp_Type::Lisp_Float => Qfloat,
        Lisp_Type::Lisp_Misc => {
            let m = object.as_misc().unwrap();
            match m.get_type() {
                Lisp_Misc_Type::Marker => Qmarker,
                Lisp_Misc_Type::Overlay => Qoverlay,
                Lisp_Misc_Type::Finalizer => Qfinalizer,
                Lisp_Misc_Type::UserPtr => Quser_ptr,
                _ => Qnone,
            }
        }
        Lisp_Type::Lisp_Vectorlike => {
            let vec = unsafe { object.as_vectorlike_unchecked() };
            match vec.pseudovector_type() {
                PseudovecType::PVEC_NORMAL_VECTOR => Qvector,
                PseudovecType::PVEC_WINDOW_CONFIGURATION => Qwindow_configuration,
                PseudovecType::PVEC_PROCESS => Qprocess,
                PseudovecType::PVEC_WINDOW => Qwindow,
                PseudovecType::PVEC_SUBR => Qsubr,
                PseudovecType::PVEC_COMPILED => Qcompiled_function,
                PseudovecType::PVEC_BUFFER => Qbuffer,
                PseudovecType::PVEC_CHAR_TABLE => Qchar_table,
                PseudovecType::PVEC_BOOL_VECTOR => Qbool_vector,
                PseudovecType::PVEC_FRAME => Qframe,
                PseudovecType::PVEC_HASH_TABLE => Qhash_table,
                PseudovecType::PVEC_THREAD => Qthread,
                PseudovecType::PVEC_MUTEX => Qmutex,
                PseudovecType::PVEC_CONDVAR => Qcondition_variable,
                PseudovecType::PVEC_TERMINAL => Qterminal,
                PseudovecType::PVEC_MODULE_FUNCTION => Qmodule_function,
                PseudovecType::PVEC_FONT => if object.is_font_spec() {
                    Qfont_spec
                } else if object.is_font_entity() {
                    Qfont_entity
                } else if object.is_font_object() {
                    Qfont_object
                } else {
                    Qfont
                },
                PseudovecType::PVEC_RECORD => unsafe {
                    let vec = object.as_vector_unchecked();
                    let t = vec.get_unchecked(0);
                    if t.is_record() {
                        let v = t.as_vector_unchecked();
                        if v.len() > 1 {
                            return v.get_unchecked(1);
                        }
                    }
                    return t;
                },
                _ => Qnone,
            }
        }
    };
    LispObject::from_raw(ty)
}

#[lisp_fn]
pub fn subr_lang(subr: LispSubrRef) -> LispObject {
    if subr.lang == Lisp_Subr_Lang_C {
        LispObject::from("C")
    } else if subr.lang == Lisp_Subr_Lang_Rust {
        LispObject::from("Rust")
    } else {
        unreachable!()
    }
}

/// Return the element of ARG at index IDX.
/// ARG may be a vector, a string, a char-table, a bool-vector, a record,
/// or a byte-code object.  IDX starts at 0.
#[lisp_fn]
pub fn aref(array: LispObject, idx: EmacsInt) -> LispObject {
    if idx < 0 {
        xsignal!(Qargs_out_of_range, array, idx.into());
    }

    let idx_i = idx as isize;
    let idx_u = idx as usize;

    if let Some(s) = array.as_string() {
        match s.char_indices().nth(idx_u) {
            None => {
                xsignal!(Qargs_out_of_range, array, idx.into());
            }
            Some((_, cp)) => EmacsInt::from(cp).into(),
        }
    } else if let Some(bv) = array.as_bool_vector() {
        if idx_u >= bv.len() {
            xsignal!(Qargs_out_of_range, array, idx.into());
        }

        unsafe { bv.get_unchecked(idx_u) }
    } else if let Some(ct) = array.as_char_table() {
        ct.get(idx as isize)
    } else if let Some(v) = array.as_vector() {
        if idx_u >= v.len() {
            xsignal!(Qargs_out_of_range, array, idx.into());
        }
        unsafe { v.get_unchecked(idx_i) }
    } else if array.is_byte_code_function() || array.is_record() {
        let vl = array.as_vectorlike().unwrap();
        if idx >= vl.pseudovector_size() {
            xsignal!(Qargs_out_of_range, array, idx.into());
        }
        let v = unsafe { vl.as_vector_unchecked() };
        unsafe { v.get_unchecked(idx_i) }
    } else {
        wrong_type!(Qarrayp, array);
    }
}

/// Store into the element of ARRAY at index IDX the value NEWELT.
/// Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
/// bool-vector.  IDX starts at 0.
#[lisp_fn]
pub fn aset(array: LispObject, idx: EmacsInt, newelt: LispObject) -> LispObject {
    if let Some(vl) = array.as_vectorlike() {
        if let Some(mut v) = vl.as_vector() {
            unsafe { CHECK_IMPURE(array.to_raw(), array.get_untaggedptr()) };
            v.set_checked(idx as isize, newelt);
        } else if let Some(mut bv) = vl.as_bool_vector() {
            bv.set_checked(idx as isize, newelt.is_not_nil());
        } else if let Some(_tbl) = vl.as_char_table() {
            verify_lisp_type!(idx, Qcharacterp);
            unsafe { CHAR_TABLE_SET(array.to_raw(), idx as c_int, newelt.to_raw()) };
        } else if let Some(mut record) = vl.as_record() {
            record.set_checked(idx as isize, newelt);
        } else {
            unreachable!();
        }
    } else if let Some(mut s) = array.as_string() {
        unsafe { CHECK_IMPURE(array.to_raw(), array.get_untaggedptr()) };
        if idx < 0 || idx >= s.len_chars() as EmacsInt {
            args_out_of_range!(array, LispObject::from(idx));
        }

        let c = newelt.as_character_or_error();

        if s.is_multibyte() {
            unsafe { aset_multibyte_string(array.to_raw(), idx, c as c_int) };
        } else if is_single_byte_char(c) {
            s.set_byte(idx as isize, c as u8);
        } else {
            if s.chars().any(|i| !is_ascii(i)) {
                args_out_of_range!(array, newelt);
            }
            s.mark_as_multibyte();
            unsafe { aset_multibyte_string(array.to_raw(), idx, c as c_int) };
        }
    } else {
        wrong_type!(Qarrayp, array);
    }

    newelt
}

/// Set SYMBOL's function definition to DEFINITION.
/// Associates the function with the current load file, if any.
/// The optional third argument DOCSTRING specifies the documentation string
/// for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
/// determined by DEFINITION.
///
/// Internally, this normally uses `fset', but if SYMBOL has a
/// `defalias-fset-function' property, the associated value is used instead.
///
/// The return value is undefined.
#[lisp_fn(min = "2")]
pub fn defalias(sym: LispObject, mut definition: LispObject, docstring: LispObject) -> LispObject {
    let symbol = sym.as_symbol_or_error();

    unsafe {
        if globals.f_Vpurify_flag != Qnil
            // If `definition' is a keymap, immutable (and copying) is wrong.
            && get_keymap(definition.to_raw(), false, false) == Qnil
        {
            definition = LispObject::from_raw(Fpurecopy(definition.to_raw()));
        }
    }

    let autoload = is_autoload(definition);
    if unsafe { globals.f_Vpurify_flag == Qnil } || !autoload {
        // Only add autoload entries after dumping, because the ones before are
        // not useful and else we get loads of them from the loaddefs.el.

        if is_autoload(LispObject::from_raw(symbol.function)) {
            // Remember that the function was already an autoload.
            loadhist_attach(unsafe { Fcons(Qt, sym.to_raw()) });
        }
        loadhist_attach(unsafe { Fcons(if autoload { Qautoload } else { Qdefun }, sym.to_raw()) });
    }

    // Handle automatic advice activation.
    let hook = get(symbol, LispObject::from_raw(Qdefalias_fset_function));
    if hook.is_not_nil() {
        call!(hook, sym, definition);
    } else {
        unsafe { Ffset(sym.to_raw(), definition.to_raw()) };
    }

    if docstring.is_not_nil() {
        put(
            sym,
            LispObject::from_raw(Qfunction_documentation),
            docstring,
        );
    }

    // We used to return `definition', but now that `defun' and `defmacro' expand
    // to a call to `defalias', we return `symbol' for backward compatibility
    // (bug#11686).
    sym
}

/// Return minimum and maximum number of args allowed for SUBR.
/// SUBR must be a built-in function.
/// The returned value is a pair (MIN . MAX).  MIN is the minimum number
/// of args.  MAX is the maximum number or the symbol `many', for a
/// function with `&rest' args, or `unevalled' for a special form.
#[lisp_fn]
pub fn subr_arity(subr: LispSubrRef) -> LispObject {
    let minargs = subr.min_args();
    let maxargs = if subr.is_many() {
        LispObject::from_raw(Qmany)
    } else if subr.is_unevalled() {
        LispObject::from_raw(Qunevalled)
    } else {
        LispObject::from(subr.max_args() as EmacsInt)
    };

    LispObject::cons(LispObject::from(minargs as EmacsInt), maxargs)
}

/// Return name of subroutine SUBR.
/// SUBR must be a built-in function.
#[lisp_fn]
pub fn subr_name(subr: LispSubrRef) -> LispObject {
    let name = subr.symbol_name();
    LispObject::from_raw(unsafe { build_string(name) })
}

/***********************************************************************
                Getting and Setting Values of Symbols
 ***********************************************************************/

/// Given the raw contents of a symbol value cell,
/// return the Lisp value of the symbol.
/// This does not handle buffer-local variables; use
/// swap_in_symval_forwarding for that.
#[no_mangle]
pub extern "C" fn do_symval_forwarding(valcontents: *mut Lisp_Fwd) -> LispObject {
    unsafe {
        match (*valcontents).u_intfwd.ty {
            Lisp_Fwd_Int => LispObject::from(*(*valcontents).u_intfwd.intvar),
            Lisp_Fwd_Bool => LispObject::from(*(*valcontents).u_boolfwd.boolvar),
            Lisp_Fwd_Obj => (*(*valcontents).u_objfwd.objvar),
            Lisp_Fwd_Buffer_Obj => {
                let base = ThreadState::current_buffer().as_mut() as *const libc::c_schar;
                let offset = (*valcontents).u_buffer_objfwd.offset;
                let p: *const libc::c_schar = base.offset(offset as isize);
                *(p as *const LispObject)
            }
            Lisp_Fwd_Kboard_Obj => {
                // We used to simply use current_kboard here, but from Lisp
                // code, its value is often unexpected.  It seems nicer to
                // allow constructions like this to work as intuitively expected:
                //
                // (with-selected-frame frame
                // (define-key local-function-map "\eOP" [f1]))
                //
                // On the other hand, this affects the semantics of
                // last-command and real-last-command, and people may rely on
                // that.  I took a quick look at the Lisp codebase, and I
                // don't think anything will break.  --lorentey
                let frame = selected_frame().as_frame_or_error();
                if !frame.is_live() {
                    emacs_abort();
                }
                let kboard = (*fget_terminal(frame.as_ptr())).kboard;
                let base = kboard as *const libc::c_schar;
                let offset = (*valcontents).u_kboard_objfwd.offset as isize;
                let p: *const libc::c_schar = base.offset(offset);
                *(p as *const LispObject)
            }
            _ => emacs_abort(),
        }
    }
}

/// Store NEWVAL into SYMBOL, where VALCONTENTS is found in the value cell
/// of SYMBOL.  If SYMBOL is buffer-local, VALCONTENTS should be the
/// buffer-independent contents of the value cell: forwarded just one
/// step past the buffer-localness.
///
/// BUF non-zero means set the value in buffer BUF instead of the
/// current buffer.  This only plays a role for per-buffer variables.
#[no_mangle]
pub extern "C" fn store_symval_forwarding(
    valcontents: *mut Lisp_Fwd,
    newval: LispObject,
    mut buf: *mut Lisp_Buffer,
) {
    match unsafe { (*valcontents).u_intfwd.ty } {
        Lisp_Fwd_Int => unsafe { (*(*valcontents).u_intfwd.intvar) = newval.as_fixnum_or_error() },
        Lisp_Fwd_Bool => unsafe { (*(*valcontents).u_boolfwd.boolvar) = newval.is_not_nil() },
        Lisp_Fwd_Obj => {
            unsafe { (*(*valcontents).u_objfwd.objvar) = newval };
            unsafe { update_buffer_defaults((*valcontents).u_objfwd.objvar, newval) };
        }
        Lisp_Fwd_Buffer_Obj => {
            let offset = unsafe { (*valcontents).u_buffer_objfwd.offset };
            let predicate = unsafe { (*valcontents).u_buffer_objfwd.predicate };

            if newval.is_not_nil() {
                if predicate.is_symbol() {
                    let mut prop = unsafe { Fget(predicate, Qchoice) };
                    if prop.is_not_nil() {
                        if memq(newval, prop).is_not_nil() {
                            unsafe { wrong_choice(prop, newval) };
                        }
                    } else {
                        prop = unsafe { Fget(predicate, Qrange) };
                        if prop.is_cons() {
                            let (min, max) = prop.as_cons_or_error().as_tuple();
                            let args = [min, newval, max];
                            if !newval.is_number() || leq(&args) {
                                unsafe { wrong_range(min, max, newval) };
                            }
                        } else if predicate.is_function() {
                            if call!(predicate, newval).is_nil() {
                                wrong_type!(predicate, newval);
                            }
                        }
                    }
                }
            }
            if buf.is_null() {
                buf = ThreadState::current_buffer().as_mut();
            }
            unsafe { set_per_buffer_value(buf, offset as isize, newval) };
        }
        Lisp_Fwd_Kboard_Obj => {
            let frame = selected_frame().as_frame_or_error();
            if !frame.is_live() {
                unsafe { emacs_abort() };
            }
            unsafe {
                let kboard = (*fget_terminal(frame.as_ptr())).kboard;
                let base = kboard as *mut libc::c_schar;
                let offset = (*valcontents).u_kboard_objfwd.offset as isize;
                let p: *mut libc::c_schar = base.offset(offset);
                *(p as *mut LispObject) = newval;
            }
        }
        _ => unsafe { emacs_abort() },
    }
}

include!(concat!(env!("OUT_DIR"), "/data_exports.rs"));
