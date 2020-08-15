use crate::lisp::LispObject;
use crate::remacs_sys::{event_kind, input_event, scroll_bar_part, Qnil};

pub fn create_emacs_event(kind: event_kind::Type, top_frame: LispObject) -> input_event {
    input_event {
        _bitfield_1: input_event::new_bitfield_1(kind, scroll_bar_part::scroll_bar_nowhere),
        code: 0,
        modifiers: 0,
        x: 0.into(),
        y: 0.into(),
        timestamp: 0,
        frame_or_window: top_frame,
        arg: Qnil,
    }
}
