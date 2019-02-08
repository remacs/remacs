//! data helpers

use field_offset::FieldOffset;
use libc::{c_char, c_int};

use remacs_macros::lisp_fn;

use crate::{
    buffers::{per_buffer_idx, per_buffer_idx_from_field_offset},
    frames::selected_frame,
    keymap::get_keymap,
    lisp::is_autoload,
    lisp::{LispObject, LispSubrRef, LiveBufferIter},
    lists::{get, member, memq, put},
    math::leq,
    multibyte::{is_ascii, is_single_byte_char, LispStringRef},
    obarray::{loadhist_attach, map_obarray},
    remacs_sys,
    remacs_sys::Vautoload_queue,
    remacs_sys::{
        aset_multibyte_string, bool_vector_binop_driver, buffer_defaults, build_string, globals,
        rust_count_one_bits, set_default_internal, set_internal, string_to_number,
        symbol_trapped_write, valid_lisp_object_p, wrong_choice, wrong_range, CHAR_TABLE_SET,
        CHECK_IMPURE,
    },
    remacs_sys::{per_buffer_default, symbol_redirect},
    remacs_sys::{pvec_type, BoolVectorOp, EmacsInt, Lisp_Misc_Type, Lisp_Type, Set_Internal_Bind},
    remacs_sys::{Fdelete, Fpurecopy},
    remacs_sys::{Lisp_Buffer, Lisp_Subr_Lang},
    remacs_sys::{
        Qarrayp, Qautoload, Qbool_vector, Qbuffer, Qchar_table, Qchoice, Qcompiled_function,
        Qcondition_variable, Qcons, Qcyclic_function_indirection, Qdefalias_fset_function, Qdefun,
        Qfinalizer, Qfloat, Qfont, Qfont_entity, Qfont_object, Qfont_spec, Qframe,
        Qfunction_documentation, Qhash_table, Qinteger, Qmany, Qmarker, Qmodule_function, Qmutex,
        Qnil, Qnone, Qoverlay, Qprocess, Qrange, Qstring, Qsubr, Qsymbol, Qterminal, Qthread,
        Qunbound, Qunevalled, Quser_ptr, Qvector, Qwatchers, Qwindow, Qwindow_configuration,
    },
    symbols::LispSymbolRef,
    threads::ThreadState,
};

// Lisp_Fwd predicates which can go away as the callers are ported to Rust

#[no_mangle]
pub unsafe extern "C" fn KBOARD_OBJFWDP(a: *const Lisp_Fwd) -> bool {
    is_kboard_objfwd(a)
}

pub unsafe fn is_kboard_objfwd(a: *const Lisp_Fwd) -> bool {
    (*a).u_intfwd.ty == Lisp_Fwd_Kboard_Obj
}

pub unsafe fn as_buffer_objfwd(a: *const Lisp_Fwd) -> Option<Lisp_Buffer_Objfwd> {
    match (*a).u_intfwd.ty {
        Lisp_Fwd_Buffer_Obj => Some((*a).u_buffer_objfwd),
        _ => None,
    }
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
        hare = LispSymbolRef::from(hare).get_function();
        if !hare.is_symbol() || hare.is_nil() {
            return hare;
        }
        hare = LispSymbolRef::from(hare).get_function();
        tortoise = LispSymbolRef::from(tortoise).get_function();
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
        "C".into()
    } else if subr.lang == Lisp_Subr_Lang::Lisp_Subr_Lang_Rust {
        "Rust".into()
    } else {
        unreachable!()
    }
}

/// Return the element of ARRAY at index IDX.
/// ARRAY may be a vector, a string, a char-table, a bool-vector, a record,
/// or a byte-code object.  IDX starts at 0.
#[lisp_fn]
pub fn aref(array: LispObject, idx: EmacsInt) -> LispObject {
    if idx < 0 {
        args_out_of_range!(array, idx);
    }

    let idx_u = idx as usize;

    if let Some(s) = array.as_string() {
        match s.char_indices().nth(idx_u) {
            None => {
                args_out_of_range!(array, idx);
            }
            Some((_, cp)) => EmacsInt::from(cp).into(),
        }
    } else if let Some(bv) = array.as_bool_vector() {
        if idx_u >= bv.len() {
            args_out_of_range!(array, idx);
        }

        unsafe { bv.get_unchecked(idx_u) }
    } else if let Some(ct) = array.as_char_table() {
        ct.get(idx as isize)
    } else if let Some(v) = array.as_vector() {
        if idx_u >= v.len() {
            args_out_of_range!(array, idx);
        }
        unsafe { v.get_unchecked(idx_u) }
    } else if array.is_byte_code_function() || array.is_record() {
        let vl = array.as_vectorlike().unwrap();
        if idx >= vl.pseudovector_size() {
            args_out_of_range!(array, idx);
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
            args_out_of_range!(array, idx);
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
pub fn defalias(
    symbol: LispSymbolRef,
    mut definition: LispObject,
    docstring: LispObject,
) -> LispObject {
    let sym = LispObject::from(symbol);

    unsafe {
        if globals.Vpurify_flag.is_not_nil()
            // If `definition' is a keymap, immutable (and copying) is wrong.
            && get_keymap(definition, false, false).is_nil()
        {
            definition = Fpurecopy(definition);
        }
    }

    let autoload = is_autoload(definition);
    if unsafe { globals.Vpurify_flag.is_nil() } || !autoload {
        // Only add autoload entries after dumping, because the ones before are
        // not useful and else we get loads of them from the loaddefs.el.

        if is_autoload(symbol.get_function()) {
            // Remember that the function was already an autoload.
            loadhist_attach((true, sym).into());
        }
        loadhist_attach((if autoload { Qautoload } else { Qdefun }, sym).into());
    }

    // Handle automatic advice activation.
    let hook = get(symbol, Qdefalias_fset_function);
    if hook.is_not_nil() {
        call!(hook, sym, definition);
    } else {
        fset(symbol, definition);
    }

    if docstring.is_not_nil() {
        put(symbol, Qfunction_documentation, docstring);
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
pub fn subr_arity(subr: LispSubrRef) -> (EmacsInt, LispObject) {
    let minargs = subr.min_args();
    let maxargs = if subr.is_many() {
        Qmany
    } else if subr.is_unevalled() {
        Qunevalled
    } else {
        EmacsInt::from(subr.max_args()).into()
    };

    (EmacsInt::from(minargs), maxargs)
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

/// Return the default value of SYMBOL, but don't check for voidness.
/// Return Qunbound if it is void.
fn default_value(mut symbol: LispSymbolRef) -> LispObject {
    while symbol.get_redirect() == symbol_redirect::SYMBOL_VARALIAS {
        symbol = symbol.get_indirect_variable();
    }
    match symbol.get_redirect() {
        symbol_redirect::SYMBOL_PLAINVAL => unsafe { symbol.get_value() },
        symbol_redirect::SYMBOL_LOCALIZED => {
            // If var is set up for a buffer that lacks a local value for it,
            // the current value is nominally the default value.
            // But the `realvalue' slot may be more up to date, since
            // ordinary setq stores just that slot.  So use that.
            let blv = unsafe { symbol.get_blv() };
            let fwd = blv.get_fwd();
            if !fwd.is_null() && blv.valcell.eq(blv.defcell) {
                unsafe { do_symval_forwarding(fwd) }
            } else {
                let (_, d) = blv.defcell.into();
                d
            }
        }
        symbol_redirect::SYMBOL_FORWARDED => unsafe {
            let valcontents = symbol.get_fwd();

            // For a built-in buffer-local variable, get the default value
            // rather than letting do_symval_forwarding get the current value.
            if let Some(buffer_objfwd) = as_buffer_objfwd(valcontents) {
                let offset = buffer_objfwd.offset;

                if per_buffer_idx_from_field_offset(offset) != 0 {
                    return per_buffer_default(offset.get_byte_offset() as i32);
                }
            }
            // For other variables, get the current value.
            do_symval_forwarding(valcontents)
        },
        _ => panic!("Symbol type has no default value"),
    }
}

/// Return t if SYMBOL has a non-void default value.
/// This is the value that is seen in buffers that do not have their own values
/// for this variable.
#[lisp_fn]
pub fn default_boundp(symbol: LispSymbolRef) -> bool {
    !default_value(symbol).eq(Qunbound)
}

/// Return SYMBOL's default value.
/// This is the value that is seen in buffers that do not have their own values
/// for this variable.  The default value is meaningful for variables with
/// local bindings in certain buffers.
#[lisp_fn(c_name = "default_value", name = "default-value")]
pub fn default_value_lisp(symbol: LispSymbolRef) -> LispObject {
    let value = default_value(symbol);

    if value.eq(Qunbound) {
        void_variable!(symbol);
    }

    value
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
pub unsafe extern "C" fn do_symval_forwarding(valcontents: *const Lisp_Fwd) -> LispObject {
    match (*valcontents).u_intfwd.ty {
        Lisp_Fwd_Int => LispObject::from(*(*valcontents).u_intfwd.intvar),
        Lisp_Fwd_Bool => LispObject::from(*(*valcontents).u_boolfwd.boolvar),
        Lisp_Fwd_Obj => (*(*valcontents).u_objfwd.objvar),
        Lisp_Fwd_Buffer_Obj => *(*valcontents)
            .u_buffer_objfwd
            .offset
            .apply_ptr(ThreadState::current_buffer_unchecked().as_mut()),
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
            let frame = selected_frame();
            if !frame.is_live() {
                panic!("Selected frame is not live");
            }
            let kboard = (*frame.terminal).kboard;
            *(*valcontents).u_kboard_objfwd.offset.apply_ptr(kboard)
        }
        _ => panic!("Unknown intfwd type"),
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
                let pred_sym: LispSymbolRef = predicate.into();
                let mut prop = get(pred_sym, Qchoice);
                if prop.is_not_nil() {
                    if memq(newval, prop).is_nil() {
                        wrong_choice(prop, newval);
                    }
                } else {
                    prop = get(pred_sym, Qrange);
                    if let Some((min, max)) = prop.into() {
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
                buf = ThreadState::current_buffer_unchecked().as_mut();
            }
            *(*valcontents).u_buffer_objfwd.offset.apply_ptr_mut(buf) = newval;
        }
        Lisp_Fwd_Kboard_Obj => {
            let frame = selected_frame();
            if !frame.is_live() {
                panic!("Selected frame is not live");
            }
            let kboard = (*frame.terminal).kboard;
            *(*valcontents).u_kboard_objfwd.offset.apply_ptr_mut(kboard) = newval;
        }
        _ => panic!("Unknown intfwd type"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn find_symbol_value(sym: LispObject) -> LispObject {
    let symbol: LispSymbolRef = sym.into();
    symbol.find_value()
}

unsafe fn update_buffer_defaults(objvar: *const LispObject, newval: LispObject) {
    // If this variable is a default for something stored
    // in the buffer itself, such as default-fill-column,
    // find the buffers that don't have local values for it
    // and update them.
    let defaults: *mut Lisp_Buffer = &mut buffer_defaults;
    let defaults_as_object_ptr = defaults as *const LispObject;
    if objvar > defaults_as_object_ptr && objvar < (defaults.add(1) as *const LispObject) {
        let offset = objvar.offset_from(defaults_as_object_ptr);
        let idx = per_buffer_idx(offset);

        if idx <= 0 {
            return;
        }

        LiveBufferIter::new().for_each(|mut buf| {
            if !buf.value_p(idx as isize) {
                buf.set_value(offset as usize, newval);
            }
        });
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

/// Set SYMBOL's value to NEWVAL, and return NEWVAL.
#[lisp_fn]
pub fn set(symbol: LispSymbolRef, newval: LispObject) -> LispObject {
    unsafe {
        set_internal(
            symbol.into(),
            newval,
            Qnil,
            Set_Internal_Bind::SET_INTERNAL_SET,
        )
    };
    newval
}

/// Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
/// The default value is seen in buffers that do not have their own
/// values for this variable.
#[lisp_fn]
pub fn set_default(symbol: LispSymbolRef, value: LispObject) -> LispObject {
    unsafe { set_default_internal(symbol.into(), value, Set_Internal_Bind::SET_INTERNAL_SET) };
    value
}

extern "C" fn harmonize_variable_watchers(alias: LispObject, base_variable: LispObject) {
    let alias_sym: LispSymbolRef = alias.into();
    let base_variable_sym: LispSymbolRef = base_variable.into();

    if !base_variable.eq(alias) && base_variable.eq(alias_sym.get_indirect_variable()) {
        alias_sym.set_trapped_write(base_variable_sym.get_trapped_write());
    }
}

/// Cause WATCH-FUNCTION to be called when SYMBOL is set.
///
/// It will be called with 4 arguments: (SYMBOL NEWVAL OPERATION WHERE).
/// SYMBOL is the variable being changed.
/// NEWVAL is the value it will be changed to.
/// OPERATION is a symbol representing the kind of change, one of: `set',
/// `let', `unlet', `makunbound', and `defvaralias'.
/// WHERE is a buffer if the buffer-local value of the variable is being
/// changed, nil otherwise.
///
/// All writes to aliases of SYMBOL will call WATCH-FUNCTION too.
#[lisp_fn]
pub fn add_variable_watcher(symbol: LispSymbolRef, watch_function: LispObject) {
    let symbol = symbol.get_indirect_variable();

    symbol.set_trapped_write(symbol_trapped_write::SYMBOL_TRAPPED_WRITE);

    map_obarray(
        unsafe { globals.Vobarray },
        harmonize_variable_watchers,
        symbol.into(),
    );

    let watchers = get(symbol, Qwatchers);
    let mem = member(watch_function, watchers);

    if mem.is_nil() {
        put(symbol, Qwatchers, (watch_function, watchers).into());
    }
}

/// Undo the effect of `add-variable-watcher'.
/// Remove WATCH-FUNCTION from the list of functions to be called when
/// SYMBOL (or its aliases) are set.
#[lisp_fn]
pub fn remove_variable_watcher(symbol: LispSymbolRef, watch_function: LispObject) {
    let symbol = symbol.get_indirect_variable();

    let watchers = get(symbol, Qwatchers);
    let watchers = unsafe { Fdelete(watch_function, watchers) };

    if watchers.is_nil() {
        symbol.set_trapped_write(symbol_trapped_write::SYMBOL_UNTRAPPED_WRITE);

        map_obarray(
            unsafe { globals.Vobarray },
            harmonize_variable_watchers,
            symbol.into(),
        );
    }

    put(symbol, Qwatchers, watchers);
}

/// Return a list of SYMBOL's active watchers.
#[lisp_fn]
pub fn get_variable_watchers(symbol: LispSymbolRef) -> LispObject {
    match symbol.get_trapped_write() {
        symbol_trapped_write::SYMBOL_TRAPPED_WRITE => {
            get(symbol.get_indirect_variable(), Qwatchers)
        }
        _ => Qnil,
    }
}

/// Return population count of VALUE.
/// This is the number of one bits in the two's complement representation
/// of VALUE.  If VALUE is negative, return the number of zero bits in the
/// representation.
#[lisp_fn]
pub fn logcount(value: EmacsInt) -> i32 {
    let value = if value < 0 { -1 - value } else { value };
    unsafe { rust_count_one_bits(value as usize) }
}

/// Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
#[lisp_fn]
pub fn fset(mut symbol: LispSymbolRef, definition: LispObject) -> LispObject {
    let sym_obj = LispObject::from(symbol);
    if sym_obj.is_nil() {
        // Perhaps not quite the right error signal, but seems good enough.
        setting_constant!(sym_obj);
    }

    let function = symbol.get_function();

    unsafe {
        if Vautoload_queue.is_not_nil() && function.is_not_nil() {
            Vautoload_queue = ((sym_obj, function), Vautoload_queue).into();
        }
    }

    if is_autoload(function) {
        let (_, d) = function.into();
        put(symbol, Qautoload, d);
    }

    // Convert to eassert or remove after GC bug is found.  In the
    // meantime, check unconditionally, at a slight perf hit.
    unsafe {
        if valid_lisp_object_p(definition) == 0 {
            panic!("Invalid Lisp object: {:?}", definition);
        }
    }

    symbol.set_function(definition);

    definition
}

/// Parse STRING as a decimal number and return the number.
/// Ignore leading spaces and tabs, and all trailing chars.  Return 0 if
/// STRING cannot be parsed as an integer or floating point number.
///
/// If BASE, interpret STRING as a number in that base.  If BASE isn't
/// present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
/// If the base used is not 10, STRING is always parsed as an integer.
#[lisp_fn(min = "1", name = "string-to-number", c_name = "string_to_number")]
pub fn string_to_number_lisp(mut string: LispStringRef, base: Option<EmacsInt>) -> LispObject {
    let b = match base {
        None => 10,
        Some(n) => {
            if n < 2 || n > 16 {
                args_out_of_range!(base, 2, 16)
            }
            n
        }
    };

    let mut p = string.sdata_ptr();
    unsafe {
        while *p == ' ' as c_char || *p == '\t' as c_char {
            p = p.offset(1);
        }
    }

    match unsafe { string_to_number(p, b as i32, true) } {
        Qnil => LispObject::from(0),
        n => n,
    }
}

include!(concat!(env!("OUT_DIR"), "/data_exports.rs"));
