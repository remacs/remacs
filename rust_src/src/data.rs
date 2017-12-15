//! data helpers

use std::ffi::CString;

use remacs_macros::lisp_fn;
use remacs_sys::{Lisp_Misc_Type, Lisp_Type, PseudovecType};
use remacs_sys::{Lisp_Subr_Lang_C, Lisp_Subr_Lang_Rust};
use remacs_sys::{Qbool_vector, Qbuffer, Qchar_table, Qcompiled_function, Qcondition_variable,
                 Qcons, Qcyclic_function_indirection, Qfinalizer, Qfloat, Qfont, Qfont_entity,
                 Qfont_object, Qfont_spec, Qframe, Qhash_table, Qinteger, Qmarker,
                 Qmodule_function, Qmutex, Qnone, Qoverlay, Qprocess, Qstring, Qsubr, Qsymbol,
                 Qterminal, Qthread, Quser_ptr, Qvector, Qwindow, Qwindow_configuration};
use remacs_sys::build_string;

use lisp::LispObject;
use lisp::defsubr;

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
    // Optimize for no indirection.
    let mut result = object;

    if let Some(symbol) = result.as_symbol() {
        result = symbol.get_function();
        if result.is_symbol() {
            result = indirect_function(result)
        }
    }
    return result;
}

/// Return a symbol representing the type of OBJECT.
/// The symbol returned names the object's basic type;
/// for example, (type-of 1) returns `integer'.
#[lisp_fn]
pub fn type_of(object: LispObject) -> LispObject {
    let ty = match object.get_type() {
        Lisp_Type::Lisp_Cons => Qcons,
        Lisp_Type::Lisp_Int0 => Qinteger,
        Lisp_Type::Lisp_Int1 => Qinteger,
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
    LispObject::from(ty)
}

#[lisp_fn]
pub fn subr_lang(subr: LispObject) -> LispObject {
    let subr = subr.as_subr_or_error();
    if subr.lang == Lisp_Subr_Lang_C {
        LispObject::from(unsafe { build_string(CString::new("C").unwrap().as_ptr()) })
    } else if subr.lang == Lisp_Subr_Lang_Rust {
        LispObject::from(unsafe {
            build_string(CString::new("Rust").unwrap().as_ptr())
        })
    } else {
        unreachable!()
    }
}

include!(concat!(env!("OUT_DIR"), "/data_exports.rs"));
