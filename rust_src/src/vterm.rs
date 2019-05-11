//! libvterm facilities

use remacs_macros::lisp_fn;

use libc::{c_char, c_uchar, c_void, size_t};

use std::{cmp, mem};

use crate::{
    cmds::forward_char,
    data::Fset,
    editfns::line_end_position,
    lisp::ExternalPtr,
    lisp::LispObject,
    multibyte::LispStringRef,
    obarray::intern,
    remacs_sys::{
        allocate_vterm, color_to_rgb_string, get_col_offset, refresh_lines, rgb_string_to_color,
        row_to_linenr, search_command, set_point, utf8_to_codepoint, vterm_output_read,
        vterm_screen_callbacks, vterm_screen_set_callbacks, VtermScrollbackLine,
    },

    remacs_sys::{
        buf_charpos_to_bytepos, code_convert_string_norecord, del_range, make_string, pvec_type,
        send_process, vterminal, EmacsInt, EmacsUint, Fput_text_property, Lisp_Type, Qbold,
        Qcursor_type, Qface, Qitalic, Qnil, Qnormal, Qt, Qutf_8, Qvtermp, STRING_BYTES,
    },

    // libvterm
    remacs_sys::{
        vterm_get_size, vterm_input_write, vterm_keyboard_key, vterm_keyboard_unichar, vterm_new,
        vterm_obtain_screen, vterm_obtain_state, vterm_output_get_buffer_current,
        vterm_screen_enable_altscreen, vterm_screen_flush_damage, vterm_screen_reset,
        vterm_screen_set_damage_merge, vterm_set_size, vterm_set_utf8, vterm_state_get_cursorpos,
        vterm_state_set_default_colors, vterm_state_set_palette_color, VTermColor, VTermDamageSize,
        VTermKey, VTermModifier, VTermPos, VTermProp, VTermRect, VTermScreenCell, VTermState,
        VTermValue,
    },
    symbols::LispSymbolRef,
    threads::ThreadState,
    vectors::length,
};

pub type LispVterminalRef = ExternalPtr<vterminal>;

impl LispVterminalRef {
    pub fn set_size(self, rows: i32, cols: i32) {
        unsafe {
            vterm_set_size((*self).vt, rows, cols);
        }
    }
}

impl LispObject {
    pub fn is_vterminal(self) -> bool {
        self.as_vectorlike()
            .map_or(false, |v| v.is_pseudovector(pvec_type::PVEC_VTERMINAL))
    }

    pub fn as_vterminal(self) -> Option<LispVterminalRef> {
        self.as_vectorlike().and_then(|v| v.as_vterminal())
    }

    pub fn as_vterminal_or_error(self) -> LispVterminalRef {
        self.as_vterminal()
            .unwrap_or_else(|| wrong_type!(Qvtermp, self))
    }
}

impl From<LispObject> for LispVterminalRef {
    fn from(o: LispObject) -> Self {
        o.as_vterminal_or_error()
    }
}

impl From<LispVterminalRef> for LispObject {
    fn from(v: LispVterminalRef) -> Self {
        Self::tag_ptr(v, Lisp_Type::Lisp_Vectorlike)
    }
}

impl From<LispObject> for Option<LispVterminalRef> {
    fn from(o: LispObject) -> Self {
        o.as_vterminal()
    }
}

/// Start a libvterm terminal-emulator in a new buffer.
/// The maximum scrollback is set by the argument SCROLLBACK.
/// You can customize the value with `vterm-max-scrollback`.
///
/// libvterm requires several callback functions that are stored
/// in VTermScreenCallbacks.
#[lisp_fn(name = "vterm-new")]
pub fn vterminal_new_lisp(
    rows: EmacsInt,
    cols: EmacsInt,
    process: LispObject,
    scrollback: EmacsUint,
) -> LispVterminalRef {
    unsafe {
        let mut term = LispVterminalRef::from_ptr(allocate_vterm() as *mut c_void).unwrap();

        (*term).vt = vterm_new(rows as i32, cols as i32);
        vterm_set_utf8((*term).vt, 1);
        vterminal_setup_colors(term);
        (*term).vts = vterm_obtain_screen((*term).vt);
        vterm_screen_reset((*term).vts, 1);

        vterm_screen_set_callbacks(
            (*term).vts,
            &vterm_screen_callbacks,
            term.as_mut() as *mut libc::c_void,
        );

        vterm_screen_set_damage_merge((*term).vts, VTermDamageSize::VTERM_DAMAGE_SCROLL);

        vterm_screen_enable_altscreen((*term).vts, 1);

        (*term).sb_size = scrollback as usize;
        (*term).sb_current = 0;
        (*term).sb_pending = 0;

        let s = mem::size_of::<VtermScrollbackLine>() * scrollback as usize;
        (*term).sb_buffer = libc::malloc(s as libc::size_t) as *mut *mut VtermScrollbackLine;

        (*term).invalid_start = 0;
        (*term).invalid_end = rows as i32;

        (*term).cursor.visible = true;

        (*term).width = cols as i32;
        (*term).height = rows as i32;

        (*term).buffer = LispObject::from(ThreadState::current_buffer_unchecked());
        (*term).process = process;

        term
    }
}

/// Refresh the screen (visible part of the buffer when the terminal is focused)
/// of a invalidated terminal
unsafe fn vterminal_refresh_screen(mut term: LispVterminalRef) {
    // Term height may have decreased before `invalid_end` reflects it.
    (*term).invalid_end = cmp::min((*term).invalid_end, (*term).height);

    if (*term).invalid_end >= (*term).invalid_start {
        let line_start = row_to_linenr(term.as_mut() as *mut vterminal, (*term).invalid_start);

        vterminal_goto_line(line_start as EmacsUint);

        vterminal_delete_lines(line_start, (*term).invalid_end - (*term).invalid_start);

        refresh_lines(
            term.as_mut() as *mut vterminal,
            (*term).invalid_start,
            (*term).invalid_end,
            (*term).width,
        );
    }
    (*term).invalid_start = std::i32::MAX;
    (*term).invalid_end = -1;
}

unsafe fn vterminal_adjust_topline(mut term: LispVterminalRef) {
    let buffer_lnum = vterminal_count_lines();

    let state: *mut VTermState = vterm_obtain_state((*term).vt);
    let mut pos: VTermPos = std::mem::zeroed();
    vterm_state_get_cursorpos(state, &mut pos);

    let cursor_lnum = row_to_linenr(term.as_mut() as *mut vterminal, pos.row);

    vterminal_goto_line(cmp::min(cursor_lnum, buffer_lnum as i32) as EmacsUint);

    let offset = get_col_offset(term.as_mut() as *mut vterminal, pos.row, pos.col);
    forward_char(LispObject::from((pos.col - offset as i32) as EmacsInt));
}

/// Refresh the scrollback of an invalidated terminal.
unsafe fn vterminal_refresh_scrollback(mut term: LispVterminalRef) {
    let mut buffer_lnum: u32;

    if (*term).sb_pending > 0 {
        buffer_lnum = vterminal_count_lines();

        let del_cnt =
            buffer_lnum as i32 - (*term).height - (*term).sb_size as i32 + (*term).sb_pending;

        if del_cnt > 0 {
            vterminal_delete_lines(1, del_cnt);
            buffer_lnum = vterminal_count_lines();
        }

        let buf_index = buffer_lnum as i32 - (*term).height + 1;
        vterminal_goto_line(buf_index as EmacsUint);

        refresh_lines(
            term.as_mut() as *mut vterminal,
            -(*term).sb_pending,
            0,
            (*term).width,
        );
        (*term).sb_pending = 0;
    }

    let max_line_count = (*term).sb_current as i32 + (*term).height;
    buffer_lnum = vterminal_count_lines();

    // Remove extra lines at the bottom
    if buffer_lnum as i32 > max_line_count {
        vterminal_delete_lines(max_line_count + 1, buffer_lnum as i32 - max_line_count + 1);
    }
}

/// Flush output and redraw terminal VTERM.
/// If the function is called with STRING, convert it to utf8 and send it to
/// the terminal before updating.
#[lisp_fn(min = "1", name = "vterm-update")]
pub fn vterminal_update(
    vterm: LispVterminalRef,
    string: LispObject,
    shift: bool,
    meta: bool,
    ctrl: bool,
) {
    unsafe {
        if string.is_not_nil() {
            let mut utf8 = code_convert_string_norecord(string, Qutf_8, true)
                .as_string()
                .unwrap();
            let len = STRING_BYTES(utf8.as_mut()) as usize;

            let mut v: Vec<c_uchar> = Vec::with_capacity(len as usize);

            let key = libc::memcpy(
                v.as_mut_ptr() as *mut c_void,
                utf8.data_ptr() as *mut c_void,
                len as libc::size_t,
            ) as *const c_char;

            let mut modifier = VTermModifier::VTERM_MOD_NONE;
            if shift {
                modifier |= VTermModifier::VTERM_MOD_SHIFT;
            }

            if meta {
                modifier |= VTermModifier::VTERM_MOD_ALT;
            }

            if ctrl {
                modifier |= VTermModifier::VTERM_MOD_CTRL;
            }

            let is_key = |key: *const c_char, val: *const c_char, len: usize| {
                libc::memcmp(key as *mut c_void, val as *mut c_void, len as size_t) == 0
            };

            if len > 1 && *(key.offset(0)) == 60 {
                if is_key(key, "<return>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_ENTER, modifier);
                } else if is_key(key, "<up>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_UP, modifier);
                } else if is_key(key, "<down>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_DOWN, modifier);
                } else if is_key(key, "<left>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_LEFT, modifier);
                } else if is_key(key, "<right>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_RIGHT, modifier);
                } else if is_key(key, "<tab>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_TAB, modifier);
                } else if is_key(key, "<backspace>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_BACKSPACE, modifier);
                } else if is_key(key, "<escape>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_ESCAPE, modifier);
                } else if is_key(key, "<insert>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_INS, modifier);
                } else if is_key(key, "<delete>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_DEL, modifier);
                } else if is_key(key, "<home>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_HOME, modifier);
                } else if is_key(key, "<end>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_END, modifier);
                } else if is_key(key, "<prior>".as_ptr() as *const c_char, len) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_PAGEUP, modifier);
                } else if *(key.offset(1)) == 102 {
                    let n = if len == 4 {
                        *(key.offset(2))
                    } else {
                        10 + *(key.offset(3))
                    } as u32;

                    vterm_keyboard_key(
                        (*vterm).vt,
                        VTermKey::VTERM_KEY_FUNCTION_0 + n - 48,
                        modifier,
                    );
                }
            } else if is_key(key, "SPC".as_ptr() as *const c_char, len) {
                vterm_keyboard_unichar((*vterm).vt, ' ' as u32, modifier);
            } else if len <= 4 {
                let mut codepoint: libc::uint32_t = std::mem::zeroed();
                if utf8_to_codepoint(key as *const c_uchar, len, &mut codepoint) {
                    vterm_keyboard_unichar((*vterm).vt, codepoint, modifier);
                }
            }
        }

        vterminal_flush_output(vterm);
        vterminal_redraw(vterm);
    }
}

unsafe fn vterminal_setup_colors(vterm: LispVterminalRef) {
    let state: *mut VTermState = vterm_obtain_state((*vterm).vt);

    let define_color = |color: LispSymbolRef, prop: LispSymbolRef| {
        rgb_string_to_color(call!(
            LispObject::from(intern("vterm--face-color-hex")),
            LispObject::from(color),
            LispObject::from(prop)
        ))
    };

    let mut fg: VTermColor = define_color(intern("vterm"), intern(":foreground"));
    let mut bg: VTermColor = define_color(intern("vterm"), intern(":background"));
    vterm_state_set_default_colors(state, &fg, &bg);

    fg = define_color(intern("vterm-color-black"), intern(":foreground"));
    vterm_state_set_palette_color(state, 0, &fg);
    bg = define_color(intern("vterm-color-black"), intern(":background"));
    vterm_state_set_palette_color(state, 8, &bg);

    fg = define_color(intern("vterm-color-red"), intern(":foreground"));
    vterm_state_set_palette_color(state, 1, &fg);
    bg = define_color(intern("vterm-color-red"), intern(":background"));
    vterm_state_set_palette_color(state, 9, &bg);

    fg = define_color(intern("vterm-color-green"), intern(":foreground"));
    vterm_state_set_palette_color(state, 2, &fg);
    bg = define_color(intern("vterm-color-green"), intern(":background"));
    vterm_state_set_palette_color(state, 10, &bg);

    fg = define_color(intern("vterm-color-yellow"), intern(":foreground"));
    vterm_state_set_palette_color(state, 3, &fg);
    bg = define_color(intern("vterm-color-yellow"), intern(":background"));
    vterm_state_set_palette_color(state, 11, &bg);

    fg = define_color(intern("vterm-color-blue"), intern(":foreground"));
    vterm_state_set_palette_color(state, 4, &fg);
    bg = define_color(intern("vterm-color-blue"), intern(":background"));
    vterm_state_set_palette_color(state, 12, &bg);

    fg = define_color(intern("vterm-color-magenta"), intern(":foreground"));
    vterm_state_set_palette_color(state, 5, &fg);
    bg = define_color(intern("vterm-color-magenta"), intern(":background"));
    vterm_state_set_palette_color(state, 13, &bg);

    fg = define_color(intern("vterm-color-cyan"), intern(":foreground"));
    vterm_state_set_palette_color(state, 6, &fg);
    bg = define_color(intern("vterm-color-cyan"), intern(":background"));
    vterm_state_set_palette_color(state, 14, &bg);

    fg = define_color(intern("vterm-color-white"), intern(":foreground"));
    vterm_state_set_palette_color(state, 7, &fg);
    bg = define_color(intern("vterm-color-white"), intern(":background"));
    vterm_state_set_palette_color(state, 15, &bg);
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_render_text(
    buffer: *mut c_char,
    len: i32,
    cell: *mut VTermScreenCell,
) -> LispObject {
    let text = if len == 0 {
        make_string("".as_ptr() as *mut c_char, len as isize)
    } else {
        make_string(buffer, len as isize)
    };

    let start = LispObject::from(0);
    let end = LispObject::from(length(text) as EmacsInt);
    let properties = list!(
        LispObject::from(intern(":foreground")),
        color_to_rgb_string((*cell).fg),
        LispObject::from(intern(":background")),
        color_to_rgb_string((*cell).bg),
        LispObject::from(intern(":weight")),
        if (*cell).attrs.bold() > 0 {
            Qbold
        } else {
            Qnormal
        },
        LispObject::from(intern(":underline")),
        if (*cell).attrs.underline() > 0 {
            Qt
        } else {
            Qnil
        },
        LispObject::from(intern(":slant")),
        if (*cell).attrs.italic() > 0 {
            Qitalic
        } else {
            Qnormal
        },
        LispObject::from(intern(":inverse-video")),
        if (*cell).attrs.reverse() > 0 {
            Qt
        } else {
            Qnil
        },
        LispObject::from(intern(":strike-through")),
        if (*cell).attrs.strike() > 0 { Qt } else { Qnil }
    );

    Fput_text_property(start, end, Qface, properties, text);

    text
}

unsafe fn vterminal_flush_output(term: LispVterminalRef) {
    let len = vterm_output_get_buffer_current((*term).vt);
    if len > 0 {
        let mut buffer: Vec<c_char> = Vec::with_capacity(len);
        let len = vterm_output_read((*term).vt, buffer.as_mut_ptr() as *mut c_char, len);

        let lisp_string = make_string(buffer.as_mut_ptr() as *mut c_char, len as isize);

        send_process(
            (*term).process,
            buffer.as_mut_ptr() as *mut c_char,
            len as isize,
            lisp_string,
        );
    }
}

/// Send INPUT to terminal VTERM.
#[lisp_fn(name = "vterm-write-input")]
pub fn vterminal_write_input(vterm: LispVterminalRef, mut input: LispStringRef) {
    unsafe {
        vterm_input_write(
            (*vterm).vt,
            input.sdata_ptr(),
            STRING_BYTES(input.as_mut()) as usize + 1,
        );
        vterm_screen_flush_damage((*vterm).vts);
    }
}

/// Change size of VTERM according to ROWS and COLS.
#[lisp_fn(name = "vterm-set-size")]
pub fn vterminal_set_size_lisp(vterm: LispVterminalRef, rows: EmacsInt, cols: EmacsInt) {
    unsafe {
        if cols as i32 != (*vterm).width || rows as i32 != (*vterm).height {
            vterm.set_size(rows as i32, cols as i32);
            vterm_screen_flush_damage((*vterm).vts);
            vterminal_redraw(vterm);
        }
    }
}

/// Refresh cursor, scrollback and screen.
/// Also adjust the top line.
unsafe fn vterminal_redraw(mut vterm: LispVterminalRef) {
    if vterm.is_invalidated {
        if (*vterm).cursor.visible {
            Fset(Qcursor_type, Qt);
        } else {
            Fset(Qcursor_type, Qnil);
        }

        let bufline_before = vterminal_count_lines();

        vterminal_refresh_scrollback(vterm);
        vterminal_refresh_screen(vterm);

        let line_added = vterminal_count_lines() - bufline_before;

        vterminal_adjust_topline(vterm);
    }
    vterm.is_invalidated = false;
}

fn vterminal_delete_lines(linenum: i32, count: i32) {
    let mut cur_buf = ThreadState::current_buffer_unchecked();

    vterminal_goto_line(linenum as EmacsInt);

    unsafe {
        del_range(
            cur_buf.pt,
            line_end_position(Some(count as EmacsInt)) as isize,
        );
    }

    let pos = cur_buf.pt;
    let pos_byte = unsafe { buf_charpos_to_bytepos(cur_buf.as_mut(), pos) };

    if cur_buf.fetch_char(pos_byte) == '\n' as i32 {
        unsafe {
            del_range(pos, pos + 1);
        }
    }
}

/// Count lines in current buffer
#[lisp_fn]
pub fn vterminal_count_lines() -> u32 {
    let cur_buf = ThreadState::current_buffer_unchecked();
    let orig_pt = cur_buf.pt;

    unsafe { set_point(cur_buf.begv) };

    let mut count: u32 = 1;
    let regexp = unsafe { make_string("\n".as_ptr() as *mut c_char, 1) };
    while unsafe { !search_command(regexp, Qnil, Qt, LispObject::from(1), 1, 1, false).is_nil() } {
        count += 1;
    }

    unsafe { set_point(orig_pt) };
    count
}

/// Unlike regular `goto-line` this function's arg LINE is an unsigned integer
#[lisp_fn]
pub fn vterminal_goto_line(line: EmacsUint) {
    unsafe { set_point(1) };

    let regexp = unsafe { make_string("\n".as_ptr() as *mut c_char, 1) };
    if line > 1 {
        unsafe { search_command(regexp, Qnil, Qt, LispObject::from(line - 1), 1, 1, false) };
    }
}

// vterm_screen_callbacks

#[no_mangle]
pub unsafe extern "C" fn vterminal_settermprop(
    prop: VTermProp,
    val: *mut VTermValue,
    user_data: *mut c_void,
) -> i32 {
    let term = user_data as *mut vterminal;

    match prop {
        VTermProp::VTERM_PROP_ALTSCREEN => vterminal_invalidate_terminal(term, 0, (*term).height),
        VTermProp::VTERM_PROP_CURSORVISIBLE => {
            vterminal_invalidate_terminal(term, (*term).cursor.row, (*term).cursor.row + 1);
            (*term).cursor.visible = (*val).boolean != 0;
        }
        _ => return 0,
    }

    1
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_invalidate_terminal(
    term: *mut vterminal,
    start_row: i32,
    end_row: i32,
) {
    if !(start_row == -1 || end_row == -1) {
        (*term).invalid_start = cmp::min((*term).invalid_start, start_row);
        (*term).invalid_end = cmp::max((*term).invalid_end, end_row);
    }
    (*term).is_invalidated = true;
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_damage(rect: VTermRect, data: *mut c_void) -> i32 {
    vterminal_invalidate_terminal(data as *mut vterminal, rect.start_row, rect.end_row);
    1
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_moverect(
    dest: VTermRect,
    src: VTermRect,
    data: *mut c_void,
) -> i32 {
    vterminal_invalidate_terminal(
        data as *mut vterminal,
        cmp::min(dest.start_row, src.start_row),
        cmp::max(dest.end_row, src.end_row),
    );

    1
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_movecursor(
    new: VTermPos,
    old: VTermPos,
    _visible: i32,
    data: *mut libc::c_void,
) -> i32 {
    let term: *mut vterminal = data as *mut vterminal;
    (*term).cursor.row = new.row;
    (*term).cursor.col = new.col;
    vterminal_invalidate_terminal(term, old.row, old.row + 1);
    vterminal_invalidate_terminal(term, new.row, new.row + 1);

    1
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_resize(
    rows: i32,
    cols: i32,
    user_data: *mut libc::c_void,
) -> i32 {
    let term: *mut vterminal = user_data as *mut vterminal;
    (*term).invalid_start = 0;
    (*term).invalid_end = rows;
    (*term).width = cols;
    (*term).height = rows;
    vterminal_invalidate_terminal(term, -1, -1);
    1
}

#[no_mangle]
pub extern "C" fn rust_syms_of_vterm() {
    def_lisp_sym!(Qvtermp, "vtermp");
}

include!(concat!(env!("OUT_DIR"), "/vterm_exports.rs"));
