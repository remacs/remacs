use crate::{keyboard, lisp::LispObject};

#[no_mangle]
pub extern "C" fn lucid_event_type_list_p(event: LispObject) -> bool {
    keyboard::lucid_event_type_list_p(event.into())
}
