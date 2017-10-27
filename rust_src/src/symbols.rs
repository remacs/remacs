//! symbols support

use std::mem;

use remacs_macros::lisp_fn;
use remacs_sys::{Fset, Lisp_Symbol};
use remacs_sys::{Qcyclic_variable_indirection, Qsetting_constant, Qunbound, Qvoid_variable};
use remacs_sys::{Symbol_Interned, Symbol_Redirect, Symbol_Trapped_Write};
use remacs_sys::{find_symbol_value, make_lisp_symbol};

use lisp::{ExternalPtr, LispObject};

pub type LispSymbolRef = ExternalPtr<Lisp_Symbol>;

const FLAG_REDIRECT: u32 = 0b1110; // bits 2, 3 and 4
const FLAG_TRAPPED_WRITE: u32 = 0b11 << 4; // bits 5 and 6
const FLAG_INTERNED: u32 = 0b11 << 6; // 7 and 8 bits

impl LispSymbolRef {
    pub fn symbol_name(&self) -> LispObject {
        LispObject::from(self.name)
    }

    pub fn get_function(&self) -> LispObject {
        LispObject::from(self.function)
    }

    pub fn get_plist(&self) -> LispObject {
        LispObject::from(self.plist)
    }

    pub fn set_plist(&mut self, plist: LispObject) {
        self.plist = plist.to_raw();
    }

    pub fn set_function(&mut self, function: LispObject) {
        self.function = function.to_raw();
    }

    pub fn is_interned_in_initial_obarray(&self) -> bool {
        self.symbol_bitfield & FLAG_INTERNED
            == (Symbol_Interned::InternedInInitialObarray as u32) << 6
    }

    pub fn is_alias(&self) -> bool {
        self.symbol_bitfield & FLAG_REDIRECT == (Symbol_Redirect::VarAlias as u32) << 1
    }

    pub fn is_constant(&self) -> bool {
        self.symbol_bitfield & FLAG_TRAPPED_WRITE == (Symbol_Trapped_Write::NoWrite as u32) << 4
    }

    pub fn get_alias(&self) -> LispSymbolRef {
        LispSymbolRef::new(unsafe { mem::transmute(self.val.alias) })
    }

    pub fn as_lisp_obj(mut self) -> LispObject {
        LispObject::from(unsafe { make_lisp_symbol(self.as_mut()) })
    }

    /// Return the symbol holding SYMBOL's value.  Signal
    /// `cyclic-variable-indirection' if SYMBOL's chain of variable
    /// indirections contains a loop.
    pub fn get_indirect_variable(self) -> Self {
        let mut tortoise = self;
        let mut hare = self;

        while hare.is_alias() {
            hare = hare.get_alias();

            if !hare.is_alias() {
                break;
            }
            hare = hare.get_alias();
            tortoise = tortoise.get_alias();

            if hare == tortoise {
                xsignal!(Qcyclic_variable_indirection, hare.as_lisp_obj())
            }
        }

        hare
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
fn symbolp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_symbol())
}

/// Return SYMBOL's name, a string.
#[lisp_fn]
pub fn symbol_name(symbol: LispObject) -> LispObject {
    symbol.as_symbol_or_error().symbol_name()
}


/* It has been previously suggested to make this function an alias for
   symbol-function, but upon discussion at Bug#23957, there is a risk
   breaking backward compatibility, as some users of fboundp may
   expect `t' in particular, rather than any true value.  */

/// Return t if SYMBOL's function definition is not void.
#[lisp_fn]
pub fn fboundp(symbol: LispObject) -> LispObject {
    let symbol = symbol.as_symbol_or_error();
    LispObject::from_bool(symbol.get_function().is_not_nil())
}

/// Return SYMBOL's function definition, or nil if that is void.
#[lisp_fn]
fn symbol_function(symbol: LispObject) -> LispObject {
    symbol.as_symbol_or_error().get_function()
}

/// Return SYMBOL's property list.
#[lisp_fn]
fn symbol_plist(symbol: LispObject) -> LispObject {
    symbol.as_symbol_or_error().get_plist()
}

/// Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.
#[lisp_fn]
fn setplist(symbol: LispObject, newplist: LispObject) -> LispObject {
    symbol.as_symbol_or_error().set_plist(newplist);
    newplist
}

/// Make SYMBOL's function definition be nil.
/// Return SYMBOL.
#[lisp_fn]
fn fmakunbound(symbol: LispObject) -> LispObject {
    let mut sym = symbol.as_symbol_or_error();
    if symbol.is_nil() || symbol.is_t() {
        xsignal!(Qsetting_constant, symbol);
    }
    sym.set_function(LispObject::constant_nil());
    symbol
}


// Define this in Rust to avoid unnecessarily consing up the symbol
// name.

/// Return t if OBJECT is a keyword.
/// This means that it is a symbol with a print name beginning with `:'
/// interned in the initial obarray.
#[lisp_fn]
fn keywordp(object: LispObject) -> LispObject {
    if let Some(sym) = object.as_symbol() {
        let name = sym.symbol_name().as_string_or_error();
        LispObject::from_bool(name.byte_at(0) == b':' && sym.is_interned_in_initial_obarray())
    } else {
        LispObject::constant_nil()
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
fn indirect_variable_lisp(object: LispObject) -> LispObject {
    if let Some(symbol) = object.as_symbol() {
        let val = symbol.get_indirect_variable();
        val.as_lisp_obj()
    } else {
        object
    }
}

/// Make SYMBOL's value be void.
/// Return SYMBOL.
#[lisp_fn]
fn makunbound(symbol: LispObject) -> LispObject {
    let sym = symbol.as_symbol_or_error();
    if sym.is_constant() {
        xsignal!(Qsetting_constant, symbol);
    }
    unsafe {
        Fset(symbol.to_raw(), Qunbound);
    }
    symbol
}

/// Return SYMBOL's value.  Error if that is void.  Note that if
/// `lexical-binding' is in effect, this returns the global value
/// outside of any lexical scope.
#[lisp_fn]
pub fn symbol_value(symbol: LispObject) -> LispObject {
    let raw_symbol = symbol.to_raw();
    let val = unsafe { find_symbol_value(raw_symbol) };
    if val == LispObject::constant_unbound().to_raw() {
        xsignal!(Qvoid_variable, symbol);
    }
    LispObject::from(val)
}
