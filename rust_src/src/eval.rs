//! Generic Lisp eval functions

use remacs_macros::lisp_fn;
use remacs_sys::{Fcons, Fset};
use remacs_sys::{QCdocumentation, Qclosure, Qfunction, Qlambda, Qnil, Qsetq, Qt,
                 Qwrong_number_of_arguments};
use remacs_sys::{eval_sub, globals};
use remacs_sys::Lisp_Object;

use lisp::LispObject;
use lisp::defsubr;
use lists::{assq, car};
use vectors::length;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   NOTE!!! Every function that can call EVAL must protect its args   *
 *   and temporaries from garbage collection while it needs them.      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

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
            let tail = cell.cdr();
            if tail.is_not_nil() {
                val = progn(tail).to_raw();
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

/// Evaluate BODY sequentially, discarding its value.
#[no_mangle]
pub extern "C" fn prog_ignore(body: Lisp_Object) {
    progn(LispObject::from_raw(body));
}

/// Eval FIRST and BODY sequentially; return value from FIRST.
/// The value of FIRST is saved during the evaluation of the remaining args,
/// whose values are discarded.
/// usage: (prog1 FIRST BODY...)
#[lisp_fn(min = "1", unevalled = "true")]
pub fn prog1(args: LispObject) -> LispObject {
    let (first, body) = args.as_cons_or_error().as_tuple();

    let val = unsafe { eval_sub(first.to_raw()) };
    progn(body);
    LispObject::from_raw(val)
}

/// Eval FORM1, FORM2 and BODY sequentially; return value from FORM2.
/// The value of FORM2 is saved during the evaluation of the
/// remaining args, whose values are discarded.
/// usage: (prog2 FORM1 FORM2 BODY...)
#[lisp_fn(min = "2", unevalled = "true")]
pub fn prog2(args: LispObject) -> LispObject {
    let (form1, tail) = args.as_cons_or_error().as_tuple();

    unsafe { eval_sub(form1.to_raw()) };
    prog1(tail)
}

/// Set each SYM to the value of its VAL.
/// The symbols SYM are variables; they are literal (not evaluated).
/// The values VAL are expressions; they are evaluated.
/// Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
/// The second VAL is not computed until after the first SYM is set, and so on;
/// each VAL can use the new value of variables set earlier in the `setq'.
/// The return value of the `setq' form is the value of the last VAL.
/// usage: (setq [SYM VAL]...)
#[lisp_fn(min = "0", unevalled = "true")]
pub fn setq(args: LispObject) -> LispObject {
    let mut val = args;

    let mut it = args.iter_cars().enumerate();
    while let Some((nargs, sym)) = it.next() {
        let (_, arg) = it.next().unwrap_or_else(|| {
            xsignal!(
                Qwrong_number_of_arguments,
                LispObject::from_raw(Qsetq),
                LispObject::from(nargs + 1)
            );
        });

        val = LispObject::from_raw(unsafe { eval_sub(arg.to_raw()) });

        let mut lexical = false;

        // Like for eval_sub, we do not check declared_special here since
        // it's been done when let-binding.
        // N.B. the check against nil is a mere optimization!
        if unsafe { globals.f_Vinternal_interpreter_environment != Qnil } && sym.is_symbol() {
            let binding = assq(
                sym,
                LispObject::from_raw(unsafe { globals.f_Vinternal_interpreter_environment }),
            );
            if let Some(binding) = binding.as_cons() {
                lexical = true;
                binding.set_cdr(val); /* SYM is lexically bound. */
            }
        }

        if !lexical {
            unsafe { Fset(sym.to_raw(), val.to_raw()) }; /* SYM is dynamically bound. */
        }
    }

    val
}
def_lisp_sym!(Qsetq, "setq");

/// Like `quote', but preferred for objects which are functions.
/// In byte compilation, `function' causes its argument to be compiled.
/// `quote' cannot do that.
/// usage: (function ARG)
#[lisp_fn(min = "1", unevalled = "true")]
pub fn function(args: LispObject) -> LispObject {
    let cell = args.as_cons_or_error();
    let (quoted, tail) = cell.as_tuple();

    if tail.is_not_nil() {
        xsignal!(
            Qwrong_number_of_arguments,
            LispObject::from_raw(Qfunction),
            length(args)
        );
    }

    if unsafe { globals.f_Vinternal_interpreter_environment != Qnil } {
        if let Some(cell) = quoted.as_cons() {
            let (first, mut cdr) = cell.as_tuple();
            if first.eq_raw(Qlambda) {
                let tmp = cdr.as_cons()
                    .and_then(|c| c.cdr().as_cons())
                    .and_then(|c| c.car().as_cons());
                if let Some(cell) = tmp {
                    let (typ, tail) = cell.as_tuple();
                    if typ.eq_raw(QCdocumentation) {
                        let docstring =
                            LispObject::from_raw(unsafe { eval_sub(car(tail).to_raw()) });
                        docstring.as_string_or_error();
                        let (a, b) = cdr.as_cons().unwrap().as_tuple();
                        cdr = LispObject::from_raw(unsafe {
                            Fcons(
                                a.to_raw(),
                                Fcons(docstring.to_raw(), b.as_cons().unwrap().cdr().to_raw()),
                            )
                        });
                    }
                }

                return LispObject::from_raw(unsafe {
                    Fcons(
                        Qclosure,
                        Fcons(globals.f_Vinternal_interpreter_environment, cdr.to_raw()),
                    )
                });
            }
        }
    }

    quoted
}
def_lisp_sym!(Qfunction, "function");

include!(concat!(env!("OUT_DIR"), "/eval_exports.rs"));
