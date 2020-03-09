use crate::{lisp::LispObject, math};

#[no_mangle]
pub extern "C" fn arithcompare(
    obj1: LispObject,
    obj2: LispObject,
    comparison: math::ArithComparison,
) -> LispObject {
    math::arithcompare(obj1, obj2, comparison).into()
}
