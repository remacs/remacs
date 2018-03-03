//! data helpers

use libc::c_int;

use remacs_macros::lisp_fn;
use remacs_sys::{aset_multibyte_string, globals, CHAR_TABLE_SET, CHECK_IMPURE};
use remacs_sys::{EmacsInt, Lisp_Misc_Type, Lisp_Type, PseudovecType};
use remacs_sys::{Fcons, Ffset, Fpurecopy};
use remacs_sys::{Lisp_Subr_Lang_C, Lisp_Subr_Lang_Rust};
use remacs_sys::{Qargs_out_of_range, Qarrayp, Qautoload, Qbool_vector, Qbuffer, Qchar_table,
                 Qcompiled_function, Qcondition_variable, Qcons, Qcyclic_function_indirection,
                 Qdefalias_fset_function, Qdefun, Qfinalizer, Qfloat, Qfont, Qfont_entity,
                 Qfont_object, Qfont_spec, Qframe, Qfunction_documentation, Qhash_table, Qinteger,
                 Qmarker, Qmodule_function, Qmutex, Qnil, Qnone, Qoverlay, Qprocess, Qstring,
                 Qsubr, Qsymbol, Qt, Qterminal, Qthread, Quser_ptr, Qvector, Qwindow,
                 Qwindow_configuration};

use keymap::get_keymap;
use lisp::{LispObject, LispSubrRef};
use lisp::{defsubr, is_autoload};
use lists::{get, put};
use multibyte::{is_ascii, is_single_byte_char};
use obarray::loadhist_attach;

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

include!(concat!(env!("OUT_DIR"), "/data_exports.rs"));
