//! Lock files for editing.

use remacs_macros::lisp_fn;

use crate::{
    lisp::LispObject,
    remacs_sys::{lock_file, unlock_file},
    remacs_sys::Qstringp,
    threads::ThreadState,
};

/// Lock FILE, if current buffer is modified.
/// FILE defaults to current buffer's visited file,
/// or else nothing is done if current buffer isn't visiting a file.
///
/// If the option `create-lockfiles' is nil, this does nothing.
#[lisp_fn(min = "0", name = "lock-buffer")]
pub fn lock_buffer_lisp(file: LispObject) {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let file = if file.is_nil() {
        cur_buf.truename()
    } else if file.is_string() {
        file
    } else {
        wrong_type!(Qstringp, file)
    };

    if cur_buf.modified_since_save() && !file.is_nil() {
        unsafe { lock_file(file) }
    }
}

/// Unlock the file visited in the current buffer.
/// If the buffer is not modified, this does nothing because the file
/// should not be locked in that case.
#[lisp_fn(name = "unlock-buffer")]
pub fn unlock_buffer_lisp() {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let truename = cur_buf.truename();

    if cur_buf.modified_since_save() && truename.is_string() {
        unsafe { unlock_file(truename) }
    }
}

include!(concat!(env!("OUT_DIR"), "/filelock_exports.rs"));
