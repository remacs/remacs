//! Generic Lisp eval functions

use remacs_macros::lisp_fn;
use remacs_sys::{Qnil, Qt};
use remacs_sys::eval_sub;

use lisp::LispObject;
use lisp::defsubr;

/// Eval args until one of them yields non-nil, then return that value.
/// The remaining args are not evalled at all.
/// If all args return nil, return nil.
/// usage: (or CONDITIONS...)
#[lisp_fn(min = "0", unevalled = "true")]
pub fn or(args: LispObject) -> LispObject {
    let mut val = Qnil;

    for elt in args.iter_cars_safe() {
        val = unsafe { eval_sub(elt.to_raw()) };
        if val != Qnil {
            break;
        }
    }

    LispObject::from_raw(val)
}

/// Eval args until one of them yields nil, then return nil.
/// The remaining args are not evalled at all.
/// If no arg yields nil, return the last arg's value.
/// usage: (and CONDITIONS...)
#[lisp_fn(min = "0", unevalled = "true")]
pub fn and(args: LispObject) -> LispObject {
    let mut val = Qt;

    for elt in args.iter_cars_safe() {
        val = unsafe { eval_sub(elt.to_raw()) };
        if val == Qnil {
            break;
        }
    }

    LispObject::from_raw(val)
}

/// If COND yields non-nil, do THEN, else do ELSE...
/// Returns the value of THEN or the value of the last of the ELSE's.
/// THEN must be one expression, but ELSE... can be zero or more expressions.
/// If COND yields nil, and there are no ELSE's, the value is nil.
/// usage: (if COND THEN ELSE...)
#[lisp_fn(name = "if", c_name = "if", min = "2", unevalled = "true")]
pub fn lisp_if(args: LispObject) -> LispObject {
    let cell = args.as_cons_or_error();
    let cond = unsafe { eval_sub(cell.car().to_raw()) };

    if cond != Qnil {
        LispObject::from_raw(unsafe { eval_sub(cell.cdr().as_cons_or_error().car().to_raw()) })
    } else {
        progn(cell.cdr().as_cons_or_error().cdr())
    }
}

/// Try each clause until one succeeds.
/// Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
/// and, if the value is non-nil, this clause succeeds:
/// then the expressions in BODY are evaluated and the last one's
/// value is the value of the cond-form.
/// If a clause has one element, as in (CONDITION), then the cond-form
/// returns CONDITION's value, if that is non-nil.
/// If no clause succeeds, cond returns nil.
/// usage: (cond CLAUSES...)
#[lisp_fn(min = "0", unevalled = "true")]
pub fn cond(args: LispObject) -> LispObject {
    let mut val = Qnil;

    for clause in args.iter_cars_safe() {
        let cell = clause.as_cons_or_error();
        val = unsafe { eval_sub(cell.car().to_raw()) };
        if val != Qnil {
            if cell.cdr().is_not_nil() {
                val = progn(cell.cdr()).to_raw();
            }
            break;
        }
    }

    LispObject::from_raw(val)
}

/// Eval BODY forms sequentially and return value of last one.
/// usage: (progn BODY...)
#[lisp_fn(min = "0", unevalled = "true")]
pub fn progn(body: LispObject) -> LispObject {
    body.iter_cars_safe()
        .map(|form| unsafe { eval_sub(form.to_raw()) })
        .last()
        .map_or_else(|| LispObject::constant_nil(), LispObject::from_raw)
}

include!(concat!(env!("OUT_DIR"), "/eval_exports.rs"));
