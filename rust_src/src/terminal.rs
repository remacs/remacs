use dispnew::LispGlyphRef;
use frames::LispFrameRef;
use libc::c_int;

#[no_mangle]
pub unsafe extern "C" fn update_begin(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).update_begin_hook {
        hook(f.as_mut())
    }
}

#[no_mangle]
pub unsafe extern "C" fn update_end(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).update_end_hook {
        hook(f.as_mut())
    }
}

// Erase operations.

/// Clear from cursor to end of frame.
#[no_mangle]
pub unsafe extern "C" fn clear_to_end(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).clear_to_end_hook {
        hook(f.as_mut())
    }
}

/// Clear entire frame.
#[no_mangle]
pub unsafe extern "C" fn clear_frame(mut f: LispFrameRef) {
    if let Some(hook) = (*f.terminal).clear_frame_hook {
        hook(f.as_mut())
    }
}

/// Clear from cursor to end of line.
/// Assume that the line is already clear starting at column first_unused_hpos.
///
/// Note that the cursor may be moved, on terminals lacking a `ce' string.
#[no_mangle]
pub unsafe extern "C" fn clear_end_of_line(mut f: LispFrameRef, first_unused_hops: c_int) {
    if let Some(hook) = (*f.terminal).clear_end_of_line_hook {
        hook(f.as_mut(), first_unused_hops)
    }
}

/// Output LEN glyphs starting at STRING at the nominal cursor position.
/// Advance the nominal cursor over the text.
#[no_mangle]
pub unsafe extern "C" fn write_glyphs(mut f: LispFrameRef, mut string: LispGlyphRef, len: c_int) {
    if let Some(hook) = (*f.terminal).write_glyphs_hook {
        hook(f.as_mut(), string.as_mut(), len)
    }
}

/// Insert LEN glyphs from START at the nominal cursor position.
///
/// If start is zero, insert blanks instead of a string at start
#[no_mangle]
pub unsafe extern "C" fn insert_glyphs(mut f: LispFrameRef, mut start: LispGlyphRef, len: c_int) {
    if len <= 0 {
        return;
    }
    if let Some(hook) = (*f.terminal).insert_glyphs_hook {
        hook(f.as_mut(), start.as_mut(), len)
    }
}

/// Delete N glyphs at the nominal cursor position.
#[no_mangle]
pub unsafe extern "C" fn delete_glyphs(mut f: LispFrameRef, n: c_int) {
    if let Some(hook) = (*f.terminal).delete_glyphs_hook {
        hook(f.as_mut(), n)
    }
}

/// Insert N lines at vpos VPOS.  If N is negative, delete -N lines.
#[no_mangle]
pub unsafe extern "C" fn ins_del_lines(mut f: LispFrameRef, vpos: c_int, n: c_int) {
    if let Some(hook) = (*f.terminal).ins_del_lines_hook {
        hook(f.as_mut(), vpos, n)
    }
}
