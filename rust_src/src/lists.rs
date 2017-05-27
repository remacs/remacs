use std::ptr;

use lisp::{CHECK_NUMBER, CHECK_LIST_END, LispObject, LispCons, Qnil};


fn atom(object: LispObject) -> LispObject {
    LispObject::from_bool(!object.is_cons())
}

defun!("atom",
       Fatom(object),
       Satom,
       atom,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is not a cons cell.  This includes nil.");

fn consp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_cons())
}

defun!("consp",
       Fconsp(object),
       Sconsp,
       consp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a cons cell.

(fn OBJECT)");

fn listp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_cons() || object.is_nil())
}

defun!("listp",
       Flistp(object),
       Slistp,
       listp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a list, that is, a cons cell or nil.
Otherwise, return nil.

(fn OBJECT)");

fn nlistp(object: LispObject) -> LispObject {
    LispObject::from_bool(!(object.is_cons() || object.is_nil()))
}

defun!("nlistp",
       Fnlistp(object),
       Snlistp,
       nlistp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is not a list.  Lists include nil.

(fn OBJECT)");

pub fn setcar(cell: LispObject, newcar: LispObject) -> LispObject {
    let cell = cell.as_cons_or_error();
    cell.check_impure();
    cell.set_car(newcar);
    newcar
}

defun!("setcar",
       Fsetcar(cell, newcar),
       Ssetcar,
       setcar,
       2,
       2,
       ptr::null(),
       "Set the car of CELL to be NEWCAR. Returns NEWCAR.

(fn CELL NEWCAR)");

fn setcdr(cell: LispObject, newcdr: LispObject) -> LispObject {
    let cell = cell.as_cons_or_error();
    cell.check_impure();
    cell.set_cdr(newcdr);
    newcdr
}

defun!("setcdr",
       Fsetcdr(cell, newcdr),
       Ssetcdr,
       setcdr,
       2,
       2,
       ptr::null(),
       "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.

(fn CELL NEWCDR)");

/// Take the car/cdr of a cons cell, or signal an error if it's a
/// different type.
///
/// # Porting Notes
///
/// This is equivalent to `CAR`/`CDR` in C code.
fn car(object: LispObject) -> LispObject {
    if object.is_nil() {
        Qnil
    } else {
        object.as_cons_or_error().car()
    }
}

defun!("car",
       Fcar(list),
       Scar,
       car,
       1,
       1,
       ptr::null(),
       "Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a \
        cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of \
        related basic
Lisp concepts such as car, cdr, cons cell and list.

(fn LIST)");

fn cdr(object: LispObject) -> LispObject {
    if object.is_nil() {
        Qnil
    } else {
        object.as_cons_or_error().cdr()
    }
}

defun!("cdr",
       Fcdr(list),
       Scdr,
       cdr,
       1,
       1,
       ptr::null(),
       "Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a \
        cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of \
        related basic
Lisp concepts such as cdr, car, cons cell and list.

(fn LIST)");

fn car_safe(object: LispObject) -> LispObject {
    object.as_cons().map_or(Qnil, |cons| cons.car())
}

defun!("car-safe",
       Fcar_safe(list),
       Scar_safe,
       car_safe,
       1,
       1,
       ptr::null(),
       "Return the car of OBJECT if it is a cons cell, or else nil.

(fn OBJECT)");

fn cdr_safe(object: LispObject) -> LispObject {
    object.as_cons().map_or(Qnil, |cons| cons.cdr())
}

defun!("cdr-safe",
       Fcdr_safe(list),
       Scdr_safe,
       cdr_safe,
       1,
       1,
       ptr::null(),
       "Return the cdr of OBJECT if it is a cons cell, or else nil.

(fn OBJECT)");

fn nthcdr(n: LispObject, list: LispObject) -> LispObject {
    CHECK_NUMBER(n.to_raw());
    let mut tail = list;
    let num = n.to_fixnum().unwrap();
    for _ in 0..num {
        match tail.as_cons() {
            None => {
                CHECK_LIST_END(tail.to_raw(), list.to_raw());
                return Qnil;
            }
            Some(tail_cons) => tail = tail_cons.cdr(),
        }
    }
    tail
}

defun!("nthcdr",
       Fnthcdr(n, list),
       Snthcdr,
       nthcdr,
       2,
       2,
       ptr::null(),
       "Take cdr N times on LIST, return the result.

(fn N LIST)");

fn nth(n: LispObject, list: LispObject) -> LispObject {
    car(nthcdr(n, list))
}

defun!("nth",
       Fnth(n, list),
       Snth,
       nth,
       2,
       2,
       ptr::null(),
       "Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned.

(fn N LIST)");

fn memq(elt: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        if elt.eq(tail.car()) {
            return tail.as_obj();
        }
    }
    Qnil
}

defun!("memq",
       Fmemq(elt, list),
       Smemq,
       memq,
       2,
       2,
       ptr::null(),
       "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT.

(fn ELT LIST)");

fn memql(elt: LispObject, list: LispObject) -> LispObject {
    if !elt.is_float() {
        return memq(elt, list);
    }
    for tail in list.iter_tails() {
        if elt.eql(tail.car()) {
            return tail.as_obj();
        }
    }
    Qnil
}

defun!("memql",
       Fmemql(elt, list),
       Smemql,
       memql,
       2,
       2,
       ptr::null(),
       "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
The value is actually the tail of LIST whose car is ELT.

(fn ELT LIST)");

fn member(elt: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        if elt.equal(tail.car()) {
            return tail.as_obj();
        }
    }
    Qnil
}

defun!("member",
       Fmember(elt, list),
       Smember,
       member,
       2,
       2,
       ptr::null(),
       "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT.

(fn ELT LIST)");

fn assq(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.car()) {
                return item;
            }
        }
    }
    Qnil
}

defun!("assq",
       Fassq(key, list),
       Sassq,
       assq,
       2,
       2,
       ptr::null(),
       "Return non-nil if KEY is `eq' to the car of an element of LIST.
The value is actually the first element of LIST whose car is KEY.
Elements of LIST that are not conses are ignored.

(fn KEY LIST)");

fn assoc(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.car()) || key.equal(item_cons.car()) {
                return item;
            }
        }
    }
    Qnil
}

defun!("assoc",
       Fassoc(key, list),
       Sassoc,
       assoc,
       2,
       2,
       ptr::null(),
       "Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the first element of LIST whose car equals KEY.

(fn KEY LIST)");

fn rassq(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.cdr()) {
                return item;
            }
        }
    }
    Qnil
}

defun!("rassq",
       Frassq(key, list),
       Srassq,
       rassq,
       2,
       2,
       ptr::null(),
       "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the first element of LIST whose cdr is KEY.

(fn KEY LIST)");

fn rassoc(key: LispObject, list: LispObject) -> LispObject {
    for tail in list.iter_tails() {
        let item = tail.car();
        if let Some(item_cons) = item.as_cons() {
            if key.eq(item_cons.cdr()) || key.equal(item_cons.cdr()) {
                return item;
            }
        }
    }
    Qnil
}

defun!("rassoc",
       Frassoc(key, list),
       Srassoc,
       rassoc,
       2,
       2,
       ptr::null(),
       "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the first element of LIST whose cdr equals KEY.

(fn KEY LIST)");

fn delq(elt: LispObject, mut list: LispObject) -> LispObject {
    let mut prev = Qnil;
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

defun!("delq",
       Fdelq(elt, list),
       Sdelq,
       delq,
       2,
       2,
       ptr::null(),
       "Delete members of LIST which are `eq' to ELT, and return the result.
More precisely, this function skips any members `eq' to ELT at the
front of LIST, then removes members `eq' to ELT from the remaining
sublist by modifying its list structure, then returns the resulting
list.

Write `(setq foo (delq element foo))' to be sure of correctly changing
the value of a list `foo'.  See also `remq', which does not modify the
argument.

(fn ELT LIST)");


fn internal_plist_get<F, I>(mut iter: I, prop: LispObject, cmp: F) -> LispObject
    where I: Iterator<Item = LispCons>,
          F: Fn(LispObject, LispObject) -> bool
{
    let mut prop_item = true;
    for tail in &mut iter {
        match tail.cdr().as_cons() {
            None => break,
            Some(tail_cdr_cons) => {
                if prop_item && cmp(tail.car(), prop) {
                    return tail_cdr_cons.car();
                }
            }
        }
        prop_item = !prop_item;
    }
    // exhaust the iterator to get the list-end check if necessary
    iter.count();
    Qnil
}

fn plist_get(plist: LispObject, prop: LispObject) -> LispObject {
    internal_plist_get(plist.iter_tails_safe(), prop, LispObject::eq)
}

defun!("plist-get",
       Fplist_get(plist, prop),
       Splist_get,
       plist_get,
       2,
       2,
       ptr::null(),
       "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not one of the
properties on the list.  This function never signals an error.

(fn PLIST PROP)");

fn lax_plist_get(plist: LispObject, prop: LispObject) -> LispObject {
    internal_plist_get(plist.iter_tails(), prop, LispObject::equal)
}

defun!("lax-plist-get",
       Flax_plist_get(plist, prop),
       Slax_plist_get,
       lax_plist_get,
       2,
       2,
       ptr::null(),
       "Extract a value from a property list, comparing with `equal'.
PLIST is a property list, which is a list of the form
\\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list.

(fn PLIST PROP)");

fn plist_member(plist: LispObject, prop: LispObject) -> LispObject {
    let mut prop_item = true;
    for tail in plist.iter_tails() {
        if prop_item && prop.eq(tail.car()) {
            return tail.as_obj();
        }
        prop_item = !prop_item;
    }
    Qnil
}

defun!("plist-member",
       Fplist_member(plist, prop),
       Splist_member,
       plist_member,
       2,
       2,
       ptr::null(),
       "Return non-nil if PLIST has the property PROP.
PLIST is a property list, which is a list of the form
\\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
Unlike `plist-get', this allows you to distinguish between a missing
property and a property with the value nil.
The value is actually the tail of PLIST whose car is PROP.

(fn PLIST PROP)");

fn internal_plist_put<F>(plist: LispObject, prop: LispObject, val: LispObject, cmp: F) -> LispObject
    where F: Fn(LispObject, LispObject) -> bool
{
    let mut prop_item = true;
    let mut last_cons = None;
    for tail in plist.iter_tails() {
        if prop_item {
            match tail.cdr().as_cons() {
                None => {
                    // need an extra call to CHECK_LIST_END here to catch odd-length lists
                    // (like Emacs we signal the somewhat confusing `wrong-type-argument')
                    CHECK_LIST_END(tail.as_obj().to_raw(), plist.to_raw());
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
        None => LispObject::cons(prop, LispObject::cons(val, Qnil)),
        Some(last_cons) => {
            let last_cons_cdr = last_cons.cdr().as_cons_or_error();
            let newcell = LispObject::cons(prop, LispObject::cons(val, last_cons_cdr.cdr()));
            last_cons_cdr.set_cdr(newcell);
            plist
        }
    }
}

fn plist_put(plist: LispObject, prop: LispObject, val: LispObject) -> LispObject {
    internal_plist_put(plist, prop, val, LispObject::eq)
}

defun!("plist-put",
       Fplist_put(plist, prop, val),
       Splist_put,
       plist_put,
       3,
       3,
       ptr::null(),
       "Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects.

(fn PLIST PROP VAL)");

fn lax_plist_put(plist: LispObject, prop: LispObject, val: LispObject) -> LispObject {
    internal_plist_put(plist, prop, val, LispObject::equal)
}

defun!("lax-plist-put",
       Flax_plist_put(plist, prop, val),
       Slax_plist_put,
       lax_plist_put,
       3,
       3,
       ptr::null(),
       "Change value in PLIST of PROP to VAL, comparing with `equal'.
PLIST is a property list, which is a list of the form
\\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP and VAL are any objects.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (lax-plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects.

(fn PLIST PROP VAL)");
