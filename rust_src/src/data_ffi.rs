use crate::{data, lisp::LispObject};

#[no_mangle]
pub extern "C" fn indirect_function(object: LispObject) -> LispObject {
    data::indirect_function(object)
}
