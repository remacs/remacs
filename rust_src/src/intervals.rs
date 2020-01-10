//! Definitions and global variables for intervals.
use crate::{lisp::LispObject, remacs_sys::EmacsInt, threads::ThreadState, xdisp::invisible_prop};

// originally the macro TEXT_PROP_MEANS_INVISIBLE from intervals.h
#[no_mangle]
pub extern "C" fn text_prop_means_invisible(prop: LispObject) -> EmacsInt {
    let cur_buf = ThreadState::current_buffer_unchecked();
    if cur_buf.invisibility_spec_.is_t() {
        if prop.is_nil() {
            0
        } else {
            1
        }
    } else {
        cur_buf
            .invisibility_spec_
            .as_cons()
            .map_or(0, |cons| invisible_prop(prop, cons))
    }
}
