//! Lisp object printing and output

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    lists::{car, cdr, car_safe, cdr_safe},
    threads::ThreadState,
    remacs_sys::{
        print_error_message, set_buffer_internal, Fbuffer_string, Ferase_buffer, set_buffer_internal,
    },
}

/* A buffer which is used to hold output being built by prin1-to-string.  */
Lisp_Object Vprin1_to_string_buffer;

/// Convert an error value (ERROR-SYMBOL . DATA) to an error message.
/// See Info anchor `(elisp)Definition of signal' for some details on how this
/// error message is constructed.
#[lisp_fn]
pub fn error_message_string(obj: LispObject) -> LispObject {
    let old = ThreadState::current_buffer_unchecked();
    let value: LispObject;

    /* If OBJ is (error STRING), just return STRING.
    That is not only faster, it also avoids the need to allocate
    space here when the error is due to memory full.  */
    match obj.into() {
        Some((car, cdr)) if car.eq(Qerror) => {
            if cdr.car_safe().is_string() && cdr.cdr_safe().is_nil() {
                return cdr.car();
            }
        }
        _ => {}
    }
    unsafe {
        print_error_message(obj, Vprin1_to_string_buffer, 0, Qnil);

        set_buffer_internal(Vprin1_to_string_buffer.as_buffer());
        value = Fbuffer_string();

        Ferase_buffer();
        set_buffer_internal(old);
    }
    return value;
}

include!(concat!(env!("OUT_DIR"), "/print_exports.rs"));
