//! Lisp object printing and output

use remacs_macros::lisp_fn;

use crate::{
    buffers::LispBufferRef,
    lisp::LispObject,
    lists::LispCons,
    remacs_sys::Vprin1_to_string_buffer,
    remacs_sys::{print_error_message, set_buffer_internal, Fbuffer_string, Ferase_buffer},
    remacs_sys::{Qerror, Qnil},
    threads::ThreadState,
};

/// Convert an error value (ERROR-SYMBOL . DATA) to an error message.
/// See Info anchor `(elisp)Definition of signal' for some details on how this
/// error message is constructed.
#[lisp_fn]
pub fn error_message_string(obj: LispCons) -> LispObject {
    let mut old = ThreadState::current_buffer_unchecked();
    let value: LispObject;

    // If OBJ is (error STRING), just return STRING.
    // That is not only faster, it also avoids the need to allocate
    // space here when the error is due to memory full.
    match obj.into() {
        (error, data) if error.eq(Qerror) => {
            if let Some((string, nil)) = data.into() {
                if string.is_string() && nil.is_nil() {
                    return string;
                }
            }
        }
        _ => {}
    }

    unsafe {
        print_error_message(obj.into(), Vprin1_to_string_buffer, std::ptr::null(), Qnil);

        set_buffer_internal(LispBufferRef::from(Vprin1_to_string_buffer).as_mut());
        value = Fbuffer_string();

        Ferase_buffer();
        set_buffer_internal(old.as_mut());
    }
    value
}

include!(concat!(env!("OUT_DIR"), "/print_exports.rs"));
