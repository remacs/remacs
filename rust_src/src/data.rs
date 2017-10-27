//! data helpers

use remacs_macros::lisp_fn;
use remacs_sys::Qcyclic_function_indirection;

use lisp::LispObject;

/// Find the function at the end of a chain of symbol function indirections.

/// If OBJECT is a symbol, find the end of its function chain and
/// return the value found there.  If OBJECT is not a symbol, just
/// return it.  If there is a cycle in the function chain, signal a
/// cyclic-function-indirection error.
///
/// This is like Findirect_function, except that it doesn't signal an
/// error if the chain ends up unbound.
#[no_mangle]
pub extern "C" fn indirect_function(object: LispObject) -> LispObject {
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
