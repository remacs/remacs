use crate::{
    frame::LispFrameRef,
    lisp::LispObject,
    remacs_sys::{
        make_frame, make_frame_without_minibuffer, make_minibuffer_frame, output_method, wr_output,
        Qnil, Qnone, Qonly,
    },
};

use super::{display_info::DisplayInfoRef, output::Output, term::KboardRef};

pub fn create_frame(
    display: LispObject,
    dpyinfo: DisplayInfoRef,
    tem: LispObject,
    mut kb: KboardRef,
) -> LispFrameRef {
    let frame = if tem.eq(Qnone) || tem.is_nil() {
        unsafe { make_frame_without_minibuffer(Qnil, kb.as_mut(), display) }
    } else if tem.eq(Qonly) {
        unsafe { make_minibuffer_frame() }
    } else if tem.is_window() {
        unsafe { make_frame_without_minibuffer(tem, kb.as_mut(), display) }
    } else {
        unsafe { make_frame(true) }
    };

    let mut frame = LispFrameRef::new(frame);

    frame.terminal = dpyinfo.get_inner().terminal.as_mut();
    frame.set_output_method(output_method::output_wr);

    let mut output = Box::new(Output::new());
    output.set_display_info(dpyinfo);

    // Remeber to destory the Output object when frame destoried.
    let output = Box::into_raw(output);
    frame.output_data.wr = output as *mut wr_output;

    dpyinfo.get_inner().output = unsafe { frame.output_data.wr.into() };

    frame
}
