//! symbols support

use std::fmt;
use std::fmt::{Debug, Formatter};

use remacs_macros::lisp_fn;

use crate::{
    buffers::per_buffer_idx_from_field_offset,
    buffers::{LispBufferLocalValueRef, LispBufferOrCurrent, LispBufferRef},
    data::Lisp_Fwd,
    data::{as_buffer_objfwd, do_symval_forwarding, indirect_function, set},
    hashtable::LispHashTableRef,
    lisp::{ExternalPtr, LispObject, LispStructuralEqual},
    multibyte::LispStringRef,
    remacs_sys::{equal_kind, lispsym, EmacsInt, Lisp_Symbol, Lisp_Type, USE_LSB_TAG},
    remacs_sys::{
        get_symbol_declared_special, get_symbol_redirect, make_lisp_symbol,
        set_symbol_declared_special, set_symbol_redirect, swap_in_symval_forwarding,
        symbol_interned, symbol_redirect, symbol_trapped_write,
    },
    remacs_sys::{Qcyclic_variable_indirection, Qnil, Qsymbolp, Qunbound},
};

pub type LispSymbolRef = ExternalPtr<Lisp_Symbol>;

impl LispSymbolRef {
    pub fn symbol_name(self) -> LispObject {
        let s = unsafe { self.u.s.as_ref() };
        s.name
    }

    pub fn get_function(self) -> LispObject {
        let s = unsafe { self.u.s.as_ref() };
        s.function
    }

    pub fn get_plist(self) -> LispObject {
        let s = unsafe { self.u.s.as_ref() };
        s.plist
    }

    pub fn set_plist(&mut self, plist: LispObject) {
        let s = unsafe { self.u.s.as_mut() };
        s.plist = plist;
    }

    pub fn set_function(&mut self, function: LispObject) {
        let s = unsafe { self.u.s.as_mut() };
        s.function = function;
    }

    pub fn is_interned_in_initial_obarray(self) -> bool {
        let s = unsafe { self.u.s.as_ref() };
        s.interned() == symbol_interned::SYMBOL_INTERNED_IN_INITIAL_OBARRAY as u32
    }

    pub fn is_alias(self) -> bool {
        let s = unsafe { self.u.s.as_ref() };
        s.redirect() == symbol_redirect::SYMBOL_VARALIAS
    }

    pub fn get_trapped_write(self) -> symbol_trapped_write::Type {
        let s = unsafe { self.u.s.as_ref() };
        s.trapped_write()
    }

    pub fn set_trapped_write(mut self, trap: symbol_trapped_write::Type) {
        let s = unsafe { self.u.s.as_mut() };
        s.set_trapped_write(trap);
    }

    pub fn is_constant(self) -> bool {
        self.get_trapped_write() == symbol_trapped_write::SYMBOL_NOWRITE
    }

    pub unsafe fn get_alias(self) -> Self {
        debug_assert!(self.is_alias());
        let s = self.u.s.as_ref();
        LispSymbolRef::new(s.val.alias)
    }

    pub fn get_declared_special(self) -> bool {
        unsafe { get_symbol_declared_special(self.as_ptr()) }
    }

    pub fn set_declared_special(mut self, value: bool) {
        unsafe { set_symbol_declared_special(self.as_mut(), value) };
    }

    /// Return the symbol holding SYMBOL's value.  Signal
    /// `cyclic-variable-indirection' if SYMBOL's chain of variable
    /// indirections contains a loop.
    pub fn get_indirect_variable(self) -> Self {
        let mut tortoise = self;
        let mut hare = self;

        while hare.is_alias() {
            hare = unsafe { hare.get_alias() };

            if !hare.is_alias() {
                break;
            }
            hare = unsafe { hare.get_alias() };
            tortoise = unsafe { tortoise.get_alias() };

            if hare == tortoise {
                xsignal!(Qcyclic_variable_indirection, hare)
            }
        }

        hare
    }

    pub fn get_indirect_function(self) -> LispObject {
        let obj = self.get_function();

        match obj.as_symbol() {
            None => obj,
            Some(_) => indirect_function(obj),
        }
    }

    pub fn get_redirect(self) -> symbol_redirect {
        unsafe { get_symbol_redirect(self.as_ptr()) }
    }

    pub fn set_redirect(mut self, v: symbol_redirect) {
        unsafe { set_symbol_redirect(self.as_mut(), v) }
    }

    pub unsafe fn get_value(self) -> LispObject {
        let s = self.u.s.as_ref();
        s.val.value
    }

    // Find the value of a symbol, returning Qunbound if it's not bound.
    // This is helpful for code which just wants to get a variable's value
    // if it has one, without signaling an error.
    // Note that it must not be possible to quit
    // within this function.  Great care is required for this.
    pub unsafe fn find_value(self) -> LispObject {
        let mut symbol = self.get_indirect_variable();

        match symbol.get_redirect() {
            symbol_redirect::SYMBOL_PLAINVAL => symbol.get_value(),
            symbol_redirect::SYMBOL_LOCALIZED => {
                let mut blv = symbol.get_blv();
                swap_in_symval_forwarding(symbol.as_mut(), blv.as_mut());

                let fwd = blv.get_fwd();
                if fwd.is_null() {
                    blv.get_value()
                } else {
                    do_symval_forwarding(fwd)
                }
            }
            symbol_redirect::SYMBOL_FORWARDED => do_symval_forwarding(symbol.get_fwd()),
            _ => unreachable!(),
        }
    }

    pub unsafe fn get_blv(self) -> LispBufferLocalValueRef {
        let s = self.u.s.as_ref();
        LispBufferLocalValueRef::new(s.val.blv)
    }

    pub unsafe fn get_fwd(self) -> *mut Lisp_Fwd {
        let s = self.u.s.as_ref();
        s.val.fwd
    }

    pub fn set_fwd(mut self, fwd: *mut Lisp_Fwd) {
        assert!(self.get_redirect() == symbol_redirect::SYMBOL_FORWARDED && !fwd.is_null());
        let s = unsafe { self.u.s.as_mut() };
        s.val.fwd = fwd;
    }

    pub fn iter(self) -> LispSymbolIter {
        LispSymbolIter { current: self }
    }
}

impl LispStructuralEqual for LispSymbolRef {
    fn equal(
        &self,
        other: Self,
        _equal_kind: equal_kind::Type,
        _depth: i32,
        _ht: &mut LispHashTableRef,
    ) -> bool {
        LispObject::from(*self).eq(LispObject::from(other))
    }
}

impl From<LispObject> for LispSymbolRef {
    fn from(o: LispObject) -> Self {
        if let Some(sym) = o.as_symbol() {
            sym
        } else {
            wrong_type!(Qsymbolp, o)
        }
    }
}

impl From<LispSymbolRef> for LispObject {
    fn from(mut s: LispSymbolRef) -> Self {
        unsafe { make_lisp_symbol(s.as_mut()) }
    }
}

impl From<LispObject> for Option<LispSymbolRef> {
    fn from(o: LispObject) -> Self {
        if o.is_symbol() {
            Some(LispSymbolRef::new(o.symbol_ptr_value() as *mut Lisp_Symbol))
        } else {
            None
        }
    }
}

// Symbol support (LispType == Lisp_Symbol == 0)
impl LispObject {
    pub fn is_symbol(self) -> bool {
        self.get_type() == Lisp_Type::Lisp_Symbol
    }

    pub fn force_symbol(self) -> LispSymbolRef {
        LispSymbolRef::new(self.symbol_ptr_value() as *mut Lisp_Symbol)
    }

    pub fn as_symbol(self) -> Option<LispSymbolRef> {
        self.into()
    }

    fn symbol_ptr_value(self) -> EmacsInt {
        let ptr_value = if USE_LSB_TAG {
            self.to_C()
        } else {
            self.get_untaggedptr() as EmacsInt
        };

        let lispsym_offset = unsafe { &lispsym as *const _ as EmacsInt };
        ptr_value + lispsym_offset
    }
}

impl Debug for LispSymbolRef {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "'{:?}", self.symbol_name())
    }
}

pub struct LispSymbolIter {
    current: LispSymbolRef,
}

impl Iterator for LispSymbolIter {
    type Item = LispSymbolRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.is_null() {
            None
        } else {
            let sym = self.current;
            let s = unsafe { sym.u.s.as_ref() };
            self.current = LispSymbolRef::new(s.next);
            Some(sym)
        }
    }
}

// Wrapper around LispSymbolRef::get_indirect_variable()
// could be removed when all C references are ported
#[no_mangle]
pub unsafe extern "C" fn indirect_variable(symbol: *mut Lisp_Symbol) -> *mut Lisp_Symbol {
    LispSymbolRef::new(symbol).get_indirect_variable().as_mut()
}

/// Return t if OBJECT is a symbol.
#[lisp_fn]
pub fn symbolp(object: LispObject) -> bool {
    object.is_symbol()
}

/// Return SYMBOL's name, a string.
#[lisp_fn]
pub fn symbol_name(symbol: LispSymbolRef) -> LispObject {
    symbol.symbol_name()
}

/// Return t if SYMBOL's value is not void.
/// Note that if `lexical-binding' is in effect, this refers to the
/// global value outside of any lexical scope.
#[lisp_fn]
pub fn boundp(mut symbol: LispSymbolRef) -> bool {
    symbol = symbol.get_indirect_variable();

    let valcontents = match symbol.get_redirect() {
        symbol_redirect::SYMBOL_PLAINVAL => unsafe { symbol.get_value() },
        symbol_redirect::SYMBOL_LOCALIZED => {
            let mut blv = unsafe { symbol.get_blv() };
            if blv.get_fwd().is_null() {
                unsafe {
                    swap_in_symval_forwarding(symbol.as_mut(), blv.as_mut());
                }
                blv.get_value()
            } else {
                // In set_internal, we un-forward vars when their value is
                // set to Qunbound.
                return true;
            }
        }
        symbol_redirect::SYMBOL_FORWARDED => {
            // In set_internal, we un-forward vars when their value is
            // set to Qunbound.
            return true;
        }
        _ => unreachable!(),
    };

    !valcontents.eq(Qunbound)
}

/* It has been previously suggested to make this function an alias for
symbol-function, but upon discussion at Bug#23957, there is a risk
breaking backward compatibility, as some users of fboundp may
expect `t' in particular, rather than any true value.  */

/// Return t if SYMBOL's function definition is not void.
#[lisp_fn]
pub fn fboundp(symbol: LispSymbolRef) -> bool {
    symbol.get_function().is_not_nil()
}

/// Return SYMBOL's function definition, or nil if that is void.
#[lisp_fn]
pub fn symbol_function(symbol: LispSymbolRef) -> LispObject {
    symbol.get_function()
}

/// Return SYMBOL's property list.
#[lisp_fn]
pub fn symbol_plist(symbol: LispSymbolRef) -> LispObject {
    symbol.get_plist()
}

/// Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.
#[lisp_fn]
pub fn setplist(mut symbol: LispSymbolRef, newplist: LispObject) -> LispObject {
    symbol.set_plist(newplist);
    newplist
}

/// Make SYMBOL's function definition be nil.
/// Return SYMBOL.
#[lisp_fn]
pub fn fmakunbound(symbol: LispObject) -> LispSymbolRef {
    let mut sym: LispSymbolRef = symbol.into();
    if symbol.is_nil() || symbol.is_t() {
        setting_constant!(symbol);
    }
    sym.set_function(Qnil);
    sym
}

// Define this in Rust to avoid unnecessarily consing up the symbol
// name.

/// Return t if OBJECT is a keyword.
/// This means that it is a symbol with a print name beginning with `:'
/// interned in the initial obarray.
#[lisp_fn]
pub fn keywordp(object: LispObject) -> bool {
    if let Some(sym) = object.as_symbol() {
        let name: LispStringRef = sym.symbol_name().into();
        name.byte_at(0) == b':' && sym.is_interned_in_initial_obarray()
    } else {
        false
    }
}

/// Return the variable at the end of OBJECT's variable chain.
/// If OBJECT is a symbol, follow its variable indirections (if any), and
/// return the variable at the end of the chain of aliases.  See Info node
/// `(elisp)Variable Aliases'.
///
/// If OBJECT is not a symbol, just return it.  If there is a loop in the
/// chain of aliases, signal a `cyclic-variable-indirection' error.
#[lisp_fn(name = "indirect-variable", c_name = "indirect_variable")]
pub fn indirect_variable_lisp(object: LispObject) -> LispObject {
    if let Some(symbol) = object.as_symbol() {
        let val = symbol.get_indirect_variable();
        val.into()
    } else {
        object
    }
}

/// Make SYMBOL's value be void.
/// Return SYMBOL.
#[lisp_fn]
pub fn makunbound(symbol: LispSymbolRef) -> LispSymbolRef {
    if symbol.is_constant() {
        setting_constant!(symbol);
    }
    set(symbol, Qunbound);
    symbol
}

/// Return SYMBOL's value.  Error if that is void.  Note that if
/// `lexical-binding' is in effect, this returns the global value
/// outside of any lexical scope.
#[lisp_fn]
pub fn symbol_value(symbol: LispSymbolRef) -> LispObject {
    let val = unsafe { symbol.find_value() };
    if val == Qunbound {
        void_variable!(symbol);
    }
    val
}
/// Non-nil if VARIABLE has a local binding in buffer BUFFER.
/// BUFFER defaults to the current buffer.
#[lisp_fn(min = "1")]
pub fn local_variable_p(mut symbol: LispSymbolRef, buffer: LispBufferOrCurrent) -> bool {
    let buf: LispBufferRef = buffer.into();

    symbol = symbol.get_indirect_variable();

    match symbol.get_redirect() {
        symbol_redirect::SYMBOL_PLAINVAL => false,
        symbol_redirect::SYMBOL_LOCALIZED => {
            let blv = unsafe { symbol.get_blv() };
            if blv.where_.eq(buf) {
                blv.found()
            } else {
                let variable: LispObject = symbol.into();
                buf.local_vars_iter().any(|local_var| {
                    let (car, _) = local_var.into();
                    variable.eq(car)
                })
            }
        }
        symbol_redirect::SYMBOL_FORWARDED => unsafe {
            let contents = symbol.get_fwd();
            match as_buffer_objfwd(contents) {
                Some(buffer_objfwd) => {
                    let idx = per_buffer_idx_from_field_offset(buffer_objfwd.offset);
                    idx == -1 || buf.value_p(idx as isize)
                }
                None => false,
            }
        },
        _ => unreachable!(),
    }
}

include!(concat!(env!("OUT_DIR"), "/symbols_exports.rs"));
