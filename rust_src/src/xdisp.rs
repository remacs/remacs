use lisp::LispObject;
use lisp::defsubr;
use remacs_macros::lisp_fn;
use remacs_sys::bset_update_mode_line;
use threads::ThreadState;

/// Mark the current buffer for redisplay.
/// This function may be passed to `add-variable-watcher`.
#[lisp_fn]
fn set_buffer_redisplay(
    _symbol: LispObject,
    _newval: LispObject,
    _op: LispObject,
    _where: LispObject,
) -> LispObject {
    unsafe { bset_update_mode_line(ThreadState::current_buffer().as_mut()) };
    ThreadState::current_buffer().prevent_redisplay_optimizations_p = true;
    LispObject::constant_nil()
}

include!(concat!(env!("OUT_DIR"), "/xdisp_exports.rs"));
