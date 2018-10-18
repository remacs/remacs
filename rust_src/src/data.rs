//! data helpers

use libc::c_int;

use remacs_macros::lisp_fn;
use remacs_sys;
use remacs_sys::{aset_multibyte_string, bool_vector_binop_driver, build_string, emacs_abort,
                 globals, update_buffer_defaults, wrong_choice, wrong_range, CHAR_TABLE_SET,
                 CHECK_IMPURE};
use remacs_sys::{pvec_type, BoolVectorOp, EmacsInt, Lisp_Misc_Type, Lisp_Type};
use remacs_sys::{Fcons, Ffset, Fget, Fpurecopy};
use remacs_sys::{Lisp_Buffer, Lisp_Subr_Lang};
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
use lisp::{defsubr, is_autoload};
use lisp::{LispObject, LispSubrRef};
use lists::{get, memq, put};
use math::leq;
use multibyte::{is_ascii, is_single_byte_char};
use obarray::loadhist_attach;
use threads::ThreadState;

use field_offset::FieldOffset;

// Lisp_Fwd predicates which can go away as the callers are ported to Rust
#[no_mangle]
pub unsafe extern "C" fn KBOARD_OBJFWDP(a: *const Lisp_Fwd) -> bool {
    (*a).u_intfwd.ty == Lisp_Fwd_Kboard_Obj
}

#[no_mangle]
pub unsafe extern "C" fn OBJFWDP(a: *const Lisp_Fwd) -> bool {
    (*a).u_intfwd.ty == Lisp_Fwd_Obj
}

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
#[lisp_fn(
    min = "1",
    c_name = "indirect_function",
    name = "indirect-function"
)]
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
    match object.get_type() {
        Lisp_Type::Lisp_Cons => Qcons,
        Lisp_Type::Lisp_Int0 | Lisp_Type::Lisp_Int1 => Qinteger,
        Lisp_Type::Lisp_Symbol => Qsymbol,
        Lisp_Type::Lisp_String => Qstring,
        Lisp_Type::Lisp_Float => Qfloat,
        Lisp_Type::Lisp_Misc => {
            let m = object.as_misc().unwrap();
            match m.get_type() {
                Lisp_Misc_Type::Lisp_Misc_Marker => Qmarker,
                Lisp_Misc_Type::Lisp_Misc_Overlay => Qoverlay,
                Lisp_Misc_Type::Lisp_Misc_Finalizer => Qfinalizer,
                Lisp_Misc_Type::Lisp_Misc_User_Ptr => Quser_ptr,
                _ => Qnone,
            }
        }
        Lisp_Type::Lisp_Vectorlike => {
            let vec = unsafe { object.as_vectorlike_unchecked() };
            match vec.pseudovector_type() {
                pvec_type::PVEC_NORMAL_VECTOR => Qvector,
                pvec_type::PVEC_WINDOW_CONFIGURATION => Qwindow_configuration,
                pvec_type::PVEC_PROCESS => Qprocess,
                pvec_type::PVEC_WINDOW => Qwindow,
                pvec_type::PVEC_SUBR => Qsubr,
                pvec_type::PVEC_COMPILED => Qcompiled_function,
                pvec_type::PVEC_BUFFER => Qbuffer,
                pvec_type::PVEC_CHAR_TABLE => Qchar_table,
                pvec_type::PVEC_BOOL_VECTOR => Qbool_vector,
                pvec_type::PVEC_FRAME => Qframe,
                pvec_type::PVEC_HASH_TABLE => Qhash_table,
                pvec_type::PVEC_THREAD => Qthread,
                pvec_type::PVEC_MUTEX => Qmutex,
                pvec_type::PVEC_CONDVAR => Qcondition_variable,
                pvec_type::PVEC_TERMINAL => Qterminal,
                pvec_type::PVEC_MODULE_FUNCTION => Qmodule_function,
                pvec_type::PVEC_FONT => {
                    if object.is_font_spec() {
                        Qfont_spec
                    } else if object.is_font_entity() {
                        Qfont_entity
                    } else if object.is_font_object() {
                        Qfont_object
                    } else {
                        Qfont
                    }
                }
                pvec_type::PVEC_RECORD => unsafe {
                    let vec = object.as_vector_unchecked();
                    let t = vec.get_unchecked(0);
                    if t.is_record() {
                        let v = t.as_vector_unchecked();
                        if v.len() > 1 {
                            return v.get_unchecked(1);
                        }
                    }
                    t
                },
                _ => Qnone,
            }
        }
    }
}

#[lisp_fn]
pub fn subr_lang(subr: LispSubrRef) -> LispObject {
    if subr.lang == Lisp_Subr_Lang::Lisp_Subr_Lang_C {
        LispObject::from("C")
    } else if subr.lang == Lisp_Subr_Lang::Lisp_Subr_Lang_Rust {
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
        unsafe { v.get_unchecked(idx_u) }
    } else if array.is_byte_code_function() || array.is_record() {
        let vl = array.as_vectorlike().unwrap();
        if idx >= vl.pseudovector_size() {
            xsignal!(Qargs_out_of_range, array, idx.into());
        }
        let v = unsafe { vl.as_vector_unchecked() };
        unsafe { v.get_unchecked(idx_u) }
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
            unsafe { CHECK_IMPURE(array, array.get_untaggedptr()) };
            v.set_checked(idx as usize, newelt);
        } else if let Some(mut bv) = vl.as_bool_vector() {
            bv.set_checked(idx as usize, newelt.is_not_nil());
        } else if let Some(_tbl) = vl.as_char_table() {
            verify_lisp_type!(idx, Qcharacterp);
            unsafe { CHAR_TABLE_SET(array, idx as c_int, newelt) };
        } else if let Some(mut record) = vl.as_record() {
            record.set_checked(idx as usize, newelt);
        } else {
            unreachable!();
        }
    } else if let Some(mut s) = array.as_string() {
        unsafe { CHECK_IMPURE(array, array.get_untaggedptr()) };
        if idx < 0 || idx >= s.len_chars() as EmacsInt {
            args_out_of_range!(array, LispObject::from(idx));
        }

        let c = newelt.as_character_or_error();

        if s.is_multibyte() {
            unsafe { aset_multibyte_string(array, idx, c as c_int) };
        } else if is_single_byte_char(c) {
            s.set_byte(idx as isize, c as u8);
        } else {
            if s.chars().any(|i| !is_ascii(i)) {
                args_out_of_range!(array, newelt);
            }
            s.mark_as_multibyte();
            unsafe { aset_multibyte_string(array, idx, c as c_int) };
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
        if globals.Vpurify_flag != Qnil
            // If `definition' is a keymap, immutable (and copying) is wrong.
            && get_keymap(definition, false, false) == Qnil
        {
            definition = Fpurecopy(definition);
        }
    }

    let autoload = is_autoload(definition);
    if unsafe { globals.Vpurify_flag == Qnil } || !autoload {
        // Only add autoload entries after dumping, because the ones before are
        // not useful and else we get loads of them from the loaddefs.el.

        if is_autoload(symbol.function) {
            // Remember that the function was already an autoload.
            loadhist_attach(unsafe { Fcons(Qt, sym) });
        }
        loadhist_attach(unsafe { Fcons(if autoload { Qautoload } else { Qdefun }, sym) });
    }

    // Handle automatic advice activation.
    let hook = get(symbol, Qdefalias_fset_function);
    if hook.is_not_nil() {
        call!(hook, sym, definition);
    } else {
        unsafe { Ffset(sym, definition) };
    }

    if docstring.is_not_nil() {
        put(sym, Qfunction_documentation, docstring);
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
        Qmany
    } else if subr.is_unevalled() {
        Qunevalled
    } else {
        LispObject::from(EmacsInt::from(subr.max_args()))
    };

    LispObject::cons(LispObject::from(EmacsInt::from(minargs)), maxargs)
}

/// Return name of subroutine SUBR.
/// SUBR must be a built-in function.
#[lisp_fn]
pub fn subr_name(subr: LispSubrRef) -> LispObject {
    let name = subr.symbol_name();
    unsafe { build_string(name) }
}

/// Return the byteorder for the machine.
/// Returns 66 (ASCII uppercase B) for big endian machines or 108
/// (ASCII lowercase l) for small endian machines.
#[lisp_fn]
pub fn byteorder() -> u8 {
    if cfg!(endian = "big") {
        b'B'
    } else {
        b'l'
    }
}

/***********************************************************************
               Getting and Setting Values of Symbols
***********************************************************************/

/// These are the types of forwarding objects used in the value slot
/// of symbols for special built-in variables whose value is stored in
/// C/Rust static variables.
pub type Lisp_Fwd_Type = u32;
pub const Lisp_Fwd_Int: Lisp_Fwd_Type = 0; // Fwd to a C `int' variable.
pub const Lisp_Fwd_Bool: Lisp_Fwd_Type = 1; // Fwd to a C boolean var.
pub const Lisp_Fwd_Obj: Lisp_Fwd_Type = 2; // Fwd to a C LispObject variable.
pub const Lisp_Fwd_Buffer_Obj: Lisp_Fwd_Type = 3; // Fwd to a LispObject field of buffers.
pub const Lisp_Fwd_Kboard_Obj: Lisp_Fwd_Type = 4; // Fwd to a LispObject field of kboards.

// these structs will still need to be compatible with their C
// counterparts until all the C callers of the DEFVAR macros are
// ported to Rust. However, as do_symval_forwarding and
// store_symval_forwarding have been ported, some Rust-isms have
// started to happen.

#[repr(C)]
pub union Lisp_Fwd {
    pub u_intfwd: Lisp_Intfwd,
    pub u_boolfwd: Lisp_Boolfwd,
    pub u_objfwd: Lisp_Objfwd,
    pub u_buffer_objfwd: Lisp_Buffer_Objfwd,
    pub u_kboard_objfwd: Lisp_Kboard_Objfwd,
}

/// Forwarding pointer to an int variable.
/// This is allowed only in the value cell of a symbol,
/// and it means that the symbol's value really lives in the
/// specified int variable.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Lisp_Intfwd {
    pub ty: Lisp_Fwd_Type, // = Lisp_Fwd_Int
    pub intvar: *mut EmacsInt,
}

/// Boolean forwarding pointer to an int variable.
/// This is like Lisp_Intfwd except that the ostensible
/// "value" of the symbol is t if the bool variable is true,
/// nil if it is false.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Lisp_Boolfwd {
    pub ty: Lisp_Fwd_Type, // = Lisp_Fwd_Bool
    pub boolvar: *mut bool,
}

/// Forwarding pointer to a LispObject variable.
/// This is allowed only in the value cell of a symbol,
/// and it means that the symbol's value really lives in the
/// specified variable.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Lisp_Objfwd {
    pub ty: Lisp_Fwd_Type, // = Lisp_Fwd_Obj
    pub objvar: *mut LispObject,
}

/// Like Lisp_Objfwd except that value lives in a slot in the
/// current buffer.  Value is byte index of slot within buffer.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Lisp_Buffer_Objfwd {
    pub ty: Lisp_Fwd_Type, // = Lisp_Fwd_Buffer_Obj
    pub offset: FieldOffset<remacs_sys::Lisp_Buffer, LispObject>,
    // One of Qnil, Qintegerp, Qsymbolp, Qstringp, Qfloatp or Qnumberp.
    pub predicate: LispObject,
}

/// Like Lisp_Objfwd except that value lives in a slot in the
/// current kboard.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Lisp_Kboard_Objfwd {
    pub ty: Lisp_Fwd_Type, // = Lisp_Fwd_Kboard_Obj
    pub offset: FieldOffset<remacs_sys::kboard, LispObject>,
}

/// Given the raw contents of a symbol value cell,
/// return the Lisp value of the symbol.
/// This does not handle buffer-local variables; use
/// swap_in_symval_forwarding for that.
#[no_mangle]
pub unsafe extern "C" fn do_symval_forwarding(valcontents: *mut Lisp_Fwd) -> LispObject {
    match (*valcontents).u_intfwd.ty {
        Lisp_Fwd_Int => LispObject::from(*(*valcontents).u_intfwd.intvar),
        Lisp_Fwd_Bool => LispObject::from(*(*valcontents).u_boolfwd.boolvar),
        Lisp_Fwd_Obj => (*(*valcontents).u_objfwd.objvar),
        Lisp_Fwd_Buffer_Obj => *(*valcontents)
            .u_buffer_objfwd
            .offset
            .apply_ptr(ThreadState::current_buffer().as_mut()),
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
            let kboard = (*frame.terminal).kboard;
            *(*valcontents).u_kboard_objfwd.offset.apply_ptr(kboard)
        }
        _ => emacs_abort(),
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
pub unsafe extern "C" fn store_symval_forwarding(
    valcontents: *mut Lisp_Fwd,
    newval: LispObject,
    mut buf: *mut Lisp_Buffer,
) {
    match (*valcontents).u_intfwd.ty {
        Lisp_Fwd_Int => (*(*valcontents).u_intfwd.intvar) = newval.as_fixnum_or_error(),
        Lisp_Fwd_Bool => (*(*valcontents).u_boolfwd.boolvar) = newval.is_not_nil(),
        Lisp_Fwd_Obj => {
            (*(*valcontents).u_objfwd.objvar) = newval;
            update_buffer_defaults((*valcontents).u_objfwd.objvar, newval);
        }
        Lisp_Fwd_Buffer_Obj => {
            let predicate = (*valcontents).u_buffer_objfwd.predicate;

            if newval.is_not_nil() && predicate.is_symbol() {
                let mut prop = Fget(predicate, Qchoice);
                if prop.is_not_nil() {
                    if memq(newval, prop).is_nil() {
                        wrong_choice(prop, newval);
                    }
                } else {
                    prop = Fget(predicate, Qrange);
                    if prop.is_cons() {
                        let (min, max) = prop.as_cons_or_error().as_tuple();
                        let args = [min, newval, max];
                        if !newval.is_number() || leq(&args) {
                            wrong_range(min, max, newval);
                        }
                    } else if predicate.is_function() && call!(predicate, newval).is_nil() {
                        wrong_type!(predicate, newval);
                    }
                }
            }

            if buf.is_null() {
                buf = ThreadState::current_buffer().as_mut();
            }
            *(*valcontents).u_buffer_objfwd.offset.apply_ptr_mut(buf) = newval;
        }
        Lisp_Fwd_Kboard_Obj => {
            let frame = selected_frame().as_frame_or_error();
            if !frame.is_live() {
                emacs_abort();
            }
            let kboard = (*frame.terminal).kboard;
            *(*valcontents).u_kboard_objfwd.offset.apply_ptr_mut(kboard) = newval;
        }
        _ => emacs_abort(),
    }
}

/// Return A ^ B, bitwise exclusive or.
/// If optional third argument C is given, store result into C.
/// A, B, and C must be bool vectors of the same length.
/// Return the destination vector if it changed or nil otherwise.
#[lisp_fn(min = "2")]
pub fn bool_vector_exclusive_or(a: LispObject, b: LispObject, c: LispObject) -> LispObject {
    unsafe { bool_vector_binop_driver(a, b, c, BoolVectorOp::BoolVectorExclusiveOr) }
}

/// Return A | B, bitwise or.
/// If optional third argument C is given, store result into C.
/// A, B, and C must be bool vectors of the same length.
/// Return the destination vector if it changed or nil otherwise.
#[lisp_fn(min = "2")]
pub fn bool_vector_union(a: LispObject, b: LispObject, c: LispObject) -> LispObject {
    unsafe { bool_vector_binop_driver(a, b, c, BoolVectorOp::BoolVectorUnion) }
}

/// Return A & B, bitwise and.
/// If optional third argument C is given, store result into C.
/// A, B, and C must be bool vectors of the same length.
/// Return the destination vector if it changed or nil otherwise.
#[lisp_fn(min = "2")]
pub fn bool_vector_intersection(a: LispObject, b: LispObject, c: LispObject) -> LispObject {
    unsafe { bool_vector_binop_driver(a, b, c, BoolVectorOp::BoolVectorIntersection) }
}

/// Return A &~ B, set difference.
/// If optional third argument C is given, store result into C.
/// A, B, and C must be bool vectors of the same length.
/// Return the destination vector if it changed or nil otherwise.
#[lisp_fn(min = "2")]
pub fn bool_vector_set_difference(a: LispObject, b: LispObject, c: LispObject) -> LispObject {
    unsafe { bool_vector_binop_driver(a, b, c, BoolVectorOp::BoolVectorSetDifference) }
}

/// Return t if every t value in A is also t in B, nil otherwise.
/// A and B must be bool vectors of the same length.
#[lisp_fn]
pub fn bool_vector_subsetp(a: LispObject, b: LispObject) -> LispObject {
    unsafe { bool_vector_binop_driver(a, b, b, BoolVectorOp::BoolVectorSubsetp) }
}

include!(concat!(env!("OUT_DIR"), "/data_exports.rs"));
