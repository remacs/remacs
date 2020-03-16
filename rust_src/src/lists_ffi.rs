use crate::{lisp::LispObject, lists};

#[no_mangle]
pub extern "C" fn circular_list(obj: LispObject) -> ! {
    lists::circular_list(obj)
}

#[no_mangle]
pub extern "C" fn merge(l1: LispObject, l2: LispObject, pred: LispObject) -> LispObject {
    lists::merge(l1, l2, pred)
}
