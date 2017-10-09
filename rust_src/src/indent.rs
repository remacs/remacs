use remacs_macros::lisp_fn;
use lisp::LispObject;
use remacs_sys;
use remacs_sys::MOST_POSITIVE_FIXNUM;
use remacs_sys::EmacsInt;

/// Return the horizontal position of point.  Beginning of line is
/// column 0.  This is calculated by adding together the widths of all
/// the displayed representations of the character between the start
/// of the previous line and point (e.g., control characters will have
/// a width of 2 or 4, tabs will have a variable width).
/// Ignores finite width of frame, which means that this function may
/// return values greater than (frame-width).
/// Whether the line is visible (if `selective-display' is t) has no
/// effect; however, ^M is treated as end of line when
/// `selective-display' is t.
/// Text that has an invisible property is considered as having width
/// 0, unless `buffer-invisibility-spec' specifies that it is replaced
/// by an ellipsis.
#[lisp_fn]
fn current_column() -> LispObject {
    let col = unsafe { remacs_sys::current_column() };
    assert!(col <= MOST_POSITIVE_FIXNUM as isize);
    LispObject::from_fixnum(col as EmacsInt)
}
