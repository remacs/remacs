//! Operations on lists.

use remacs_macros::lisp_fn;
use remacs_sys::{EmacsInt, Qcircular_list, Qplistp};
use remacs_sys::globals;

use lisp::LispObject;
use lisp::defsubr;
use symbols::LispSymbolRef;

/// Return t if OBJECT is not a cons cell.  This includes nil.
#[lisp_fn]
pub fn atom(object: LispObject) -> LispObject {
    LispObject::from_bool(!object.is_cons())
}

/// Return t if OBJECT is a cons cell.
#[lisp_fn]
pub fn consp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_cons())
}

/// Return t if OBJECT is a list, that is, a cons cell or nil.
/// Otherwise, return nil.
#[lisp_fn]
pub fn listp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_cons() || object.is_nil())
}

/// Return t if OBJECT is not a list.  Lists include nil.
#[lisp_fn]
pub fn nlistp(object: LispObject) -> LispObject {
    LispObject::from_bool(!(object.is_cons() || object.is_nil()))
}

/// Set the car of CELL to be NEWCAR. Returns NEWCAR.
#[lisp_fn]
pub fn setcar(cell: LispObject, newcar: LispObject) -> LispObject {
    let cell = cell.as_cons_or_error();
    cell.check_impure();
    cell.set_car(newcar);
    newcar
}

/// Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.
#[lisp_fn]
pub fn setcdr(cell: LispObject, newcdr: LispObject) -> LispObject {
    let cell = cell.as_cons_or_error();
    cell.check_impure();
    cell.set_cdr(newcdr);
    newcdr
}

/// Return the car of LIST.  If arg is nil, return nil.
/// Error if arg is not nil and not a cons cell.  See also `car-safe`.
///
/// See Info node `(elisp)Cons Cells' for a discussion of related basic
/// Lisp concepts such as car, cdr, cons cell and list.
#[lisp_fn]
pub fn car(list: LispObject) -> LispObject {
    if list.is_nil() {
        list
    } else {
        list.as_cons_or_error().car()
    }
}

/// Return the cdr of LIST.  If arg is nil, return nil.
/// Error if arg is not nil and not a cons cell.  See also `cdr-safe'.
///
/// See Info node `(elisp)Cons Cells' for a discussion of related basic
/// Lisp concepts such as cdr, car, cons cell and list.
#[lisp_fn]
pub fn cdr(list: LispObject) -> LispObject {
    if list.is_nil() {
        list
    } else {
        list.as_cons_or_error().cdr()
    }
}

/// Return the car of OBJECT if it is a cons cell, or else nil.
#[lisp_fn]
pub fn car_safe(object: LispObject) -> LispObject {
    object
        .as_cons()
        .map_or(LispObject::constant_nil(), |cons| {
            cons.car()
        })
}

/// Return the cdr of OBJECT if it is a cons cell, or else nil.
#[lisp_fn]
pub fn cdr_safe(object: LispObject) -> LispObject {
    object
        .as_cons()
        .map_or(LispObject::constant_nil(), |cons| {
            cons.cdr()
        })
}

/// Take cdr N times on LIST, return the result.
#[lisp_fn]
pub fn nthcdr(n: EmacsInt, list: LispObject) -> LispObject {
    let mut it = list.iter_tails_safe();

    match it.nth(n as usize) {
        Some(value) => value.as_obj(),
        None => it.rest(),
    }
}

/// Return the Nth element of LIST.
/// N counts from zero.  If LIST is not that long, nil is returned.
#[lisp_fn]
pub fn nth(n: EmacsInt, list: LispObject) -> LispObject {
    list.iter_cars()
        .nth(n as usize)
        .map_or(LispObject::constant_nil(), |c| c)
}

/// Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
/// The value is actually the tail of LIST whose car is ELT.
#[lisp_fn]
pub fn memq(elt: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        if elt.eq(tail.car()) {
            return tail.as_obj();
        }
    }
    LispObject::constant_nil()
}

/// Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
/// The value is actually the tail of LIST whose car is ELT.
#[lisp_fn]
pub fn memql(elt: LispObject, list: LispObject) -> LispObject {
    if !elt.is_float() {
        return memq(elt, list);
    }
    for tail in list.iter_tails() {
        if elt.eql(tail.car()) {
            return tail.as_obj();
        }
    }
    LispObject::constant_nil()
}

/// Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
/// The value is actually the tail of LIST whose car is ELT.
#[lisp_fn]
pub fn member(elt: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        if elt.equal(tail.car()) {
            return tail.as_obj();
        }
    }
    LispObject::constant_nil()
}

/// Return non-nil if KEY is `eq' to the car of an element of LIST.
/// The value is actually the first element of LIST whose car is KEY.
/// Elements of LIST that are not conses are ignored.
#[lisp_fn]
pub fn assq(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.car()) {
                return item;
            }
        }
    }
    LispObject::constant_nil()
}

/// Return non-nil if KEY is equal to the car of an element of LIST.
/// The value is actually the first element of LIST whose car equals KEY.
///
/// Equality is defined by TESTFN is non-nil or by `equal' if nil.
#[lisp_fn(min = "2")]
pub fn assoc(key: LispObject, list: LispObject, testfn: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            let is_equal = if testfn.is_nil() {
                key.eq(item_cons.car()) || key.equal(item_cons.car())
            } else {
                call!(testfn, key, item_cons.car()).is_not_nil()
            };
            if is_equal {
                return item;
            }
        }
    }
    LispObject::constant_nil()
}

/// Return non-nil if KEY is `eq' to the cdr of an element of LIST.
/// The value is actually the first element of LIST whose cdr is KEY.
#[lisp_fn]
pub fn rassq(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.cdr()) {
                return item;
            }
        }
    }
    LispObject::constant_nil()
}

/// Return non-nil if KEY is `equal' to the cdr of an element of LIST.
/// The value is actually the first element of LIST whose cdr equals KEY.
/// (fn KEY LIST)
#[lisp_fn]
pub fn rassoc(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.cdr()) || key.equal(item_cons.cdr()) {
                return item;
            }
        }
    }
    LispObject::constant_nil()
}

/// Delete members of LIST which are `eq' to ELT, and return the result.
/// More precisely, this function skips any members `eq' to ELT at the
/// front of LIST, then removes members `eq' to ELT from the remaining
/// sublist by modifying its list structure, then returns the resulting
/// list.
///
/// Write `(setq foo (delq element foo))' to be sure of correctly changing
/// the value of a list `foo'.  See also `remq', which does not modify the
/// argument.
#[lisp_fn]
pub fn delq(elt: LispObject, mut list: LispObject) -> LispObject {
    let mut prev = LispObject::constant_nil();
    for tail in list.iter_tails() {
        let item = tail.car();
        if elt.eq(item) {
            let rest = tail.cdr();
            if prev.is_nil() {
                list = rest;
            } else {
                setcdr(prev, rest);
            }
        } else {
            prev = tail.as_obj();
        }
    }
    list
}

/// Extract a value from a property list.
/// PLIST is a property list, which is a list of the form
/// (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
/// corresponding to the given PROP, or nil if PROP is not one of the
/// properties on the list.  This function never signals an error.
#[lisp_fn]
pub fn plist_get(plist: LispObject, prop: LispObject) -> LispObject {
    let mut prop_item = true;
    for tail in plist.iter_tails_safe() {
        if prop_item {
            match tail.cdr().as_cons() {
                None => break,
                Some(tail_cdr_cons) => if tail.car().eq(prop) {
                    return tail_cdr_cons.car();
                },
            }
        }
        prop_item = !prop_item;
    }
    LispObject::constant_nil()
}

/// Extract a value from a property list, comparing with `equal'.
/// PLIST is a property list, which is a list of the form
/// (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
/// corresponding to the given PROP, or nil if PROP is not
/// one of the properties on the list.
#[lisp_fn]
pub fn lax_plist_get(plist: LispObject, prop: LispObject) -> LispObject {
    let mut prop_item = true;
    for tail in plist.iter_tails_plist() {
        if prop_item {
            match tail.cdr().as_cons() {
                None => {
                    // need an extra check here to catch odd-length lists
                    if tail.as_obj().is_not_nil() {
                        wrong_type!(Qplistp, plist)
                    }
                    break;
                }
                Some(tail_cdr_cons) => if tail.car().equal(prop) {
                    return tail_cdr_cons.car();
                },
            }
        }
        prop_item = !prop_item;
    }
    LispObject::constant_nil()
}

/// Return non-nil if PLIST has the property PROP.
/// PLIST is a property list, which is a list of the form
/// (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
/// Unlike `plist-get', this allows you to distinguish between a missing
/// property and a property with the value nil.
/// The value is actually the tail of PLIST whose car is PROP.
#[lisp_fn]
pub fn plist_member(plist: LispObject, prop: LispObject) -> LispObject {
    let mut prop_item = true;
    for tail in plist.iter_tails_plist() {
        if prop_item && prop.eq(tail.car()) {
            return tail.as_obj();
        }
        prop_item = !prop_item;
    }
    LispObject::constant_nil()
}

fn internal_plist_put<F>(plist: LispObject, prop: LispObject, val: LispObject, cmp: F) -> LispObject
where
    F: Fn(LispObject, LispObject) -> bool,
{
    let mut prop_item = true;
    let mut last_cons = None;
    for tail in plist.iter_tails_plist() {
        if prop_item {
            match tail.cdr().as_cons() {
                None => {
                    // need an extra check here to catch odd-length lists
                    if tail.as_obj().is_not_nil() {
                        wrong_type!(Qplistp, plist)
                    }
                    break;
                }
                Some(tail_cdr_cons) => {
                    if cmp(tail.car(), prop) {
                        tail_cdr_cons.set_car(val);
                        return plist;
                    }
                    last_cons = Some(tail);
                }
            }
        }
        prop_item = !prop_item;
    }
    match last_cons {
        None => LispObject::cons(prop, LispObject::cons(val, LispObject::constant_nil())),
        Some(last_cons) => {
            let last_cons_cdr = last_cons.cdr().as_cons_or_error();
            let newcell = LispObject::cons(prop, LispObject::cons(val, last_cons_cdr.cdr()));
            last_cons_cdr.set_cdr(newcell);
            plist
        }
    }
}

/// Change value in PLIST of PROP to VAL.
/// PLIST is a property list, which is a list of the form
/// (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
/// If PROP is already a property on the list, its value is set to VAL,
/// otherwise the new PROP VAL pair is added.  The new plist is returned;
/// use `(setq x (plist-put x prop val))' to be sure to use the new value.
/// The PLIST is modified by side effects.
#[lisp_fn]
pub fn plist_put(plist: LispObject, prop: LispObject, val: LispObject) -> LispObject {
    internal_plist_put(plist, prop, val, LispObject::eq)
}

/// Change value in PLIST of PROP to VAL, comparing with `equal'.
/// PLIST is a property list, which is a list of the form
/// (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP and VAL are any objects.
/// If PROP is already a property on the list, its value is set to VAL,
/// otherwise the new PROP VAL pair is added.  The new plist is returned;
/// use `(setq x (lax-plist-put x prop val))' to be sure to use the new value.
/// The PLIST is modified by side effects.
#[lisp_fn]
pub fn lax_plist_put(plist: LispObject, prop: LispObject, val: LispObject) -> LispObject {
    internal_plist_put(plist, prop, val, LispObject::equal)
}

/// Return the value of SYMBOL's PROPNAME property.
/// This is the last value stored with `(put SYMBOL PROPNAME VALUE)'.
#[lisp_fn]
pub fn get(symbol: LispSymbolRef, propname: LispObject) -> LispObject {
    let plist_env = LispObject::from(unsafe { globals.f_Voverriding_plist_environment });
    let propval = plist_get(cdr(assq(symbol.as_lisp_obj(), plist_env)), propname);
    if propval.is_not_nil() {
        propval
    } else {
        plist_get(symbol.get_plist(), propname)
    }
}

/// Store SYMBOL's PROPNAME property with value VALUE.
/// It can be retrieved with `(get SYMBOL PROPNAME)'.
#[lisp_fn]
pub fn put(symbol: LispObject, propname: LispObject, value: LispObject) -> LispObject {
    let mut sym = symbol.as_symbol_or_error();
    let new_plist = plist_put(sym.get_plist(), propname, value);
    sym.set_plist(new_plist);
    value
}

/// Return a newly created list with specified arguments as elements.
/// Any number of arguments, even zero arguments, are allowed.
/// usage: (fn &rest OBJECTS)
#[lisp_fn]
pub fn list(args: &mut [LispObject]) -> LispObject {
    args.iter()
        .rev()
        .fold(LispObject::constant_nil(), |list, &arg| {
            LispObject::cons(arg, list)
        })
}

/// Return a newly created list of length LENGTH, with each element being INIT.
#[lisp_fn]
pub fn make_list(length: LispObject, init: LispObject) -> LispObject {
    let length = length.as_natnum_or_error();
    (0..length).fold(LispObject::constant_nil(), |list, _| {
        LispObject::cons(init, list)
    })
}

/// Return the length of a list, but avoid error or infinite loop.
/// This function never gets an error.  If LIST is not really a list,
/// it returns 0.  If LIST is circular, it returns a finite value
/// which is at least the number of distinct elements.
#[lisp_fn]
pub fn safe_length(list: LispObject) -> LispObject {
    LispObject::int_or_float_from_fixnum(list.iter_tails_safe().count() as EmacsInt)
}

// Used by sort() in vectors.rs.

pub fn sort_list(list: LispObject, pred: LispObject) -> LispObject {
    let length = list.iter_tails().count();
    if length < 2 {
        return list;
    }

    let item = nthcdr((length / 2 - 1) as EmacsInt, list);
    let back = cdr(item);
    setcdr(item, LispObject::constant_nil());

    let front = sort_list(list, pred);
    let back = sort_list(back, pred);
    merge(front, back, pred)
}

// also needed by vectors.rs
pub fn inorder(pred: LispObject, a: LispObject, b: LispObject) -> bool {
    call!(pred, b, a).is_nil()
}

/// Merge step of linked-list sorting.
pub fn merge(mut l1: LispObject, mut l2: LispObject, pred: LispObject) -> LispObject {
    let mut tail = LispObject::constant_nil();
    let mut value = LispObject::constant_nil();

    loop {
        if l1.is_nil() {
            if tail.is_nil() {
                return l2;
            }
            setcdr(tail, l2);
            return value;
        }
        if l2.is_nil() {
            if tail.is_nil() {
                return l1;
            }
            setcdr(tail, l1);
            return value;
        }

        let item;
        if inorder(pred, car(l1), car(l2)) {
            item = l1;
            l1 = cdr(l1);
        } else {
            item = l2;
            l2 = cdr(l2);
        }
        if tail.is_nil() {
            value = item;
        } else {
            setcdr(tail, item);
        }
        tail = item;
    }
}

pub fn circular_list(obj: LispObject) -> ! {
    xsignal!(Qcircular_list, obj);
}

include!(concat!(env!("OUT_DIR"), "/lists_exports.rs"));
