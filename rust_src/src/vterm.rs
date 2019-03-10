//! libvterm facilities

use remacs_macros::lisp_fn;

use libc::{c_char, c_uchar, c_void, size_t};

use std::{cmp, mem};

use crate::{
    cmds::{forward_char, forward_line},
    data::Fset,
    editfns::{goto_pos, line_beginning_position, line_end_position},
    lisp::ExternalPtr,
    lisp::{defsubr, LispObject},
    multibyte::LispStringRef,
    obarray::intern,
    remacs_sys::{
        allocate_vterm, color_to_rgb_string, get_col_offset, refresh_lines, rgb_string_to_color,
        row_to_linenr, utf8_to_codepoint, vterm_output_read, vterm_screen_callbacks,
        vterm_screen_set_callbacks, VtermScrollbackLine,
    },

    remacs_sys::{
        buf_charpos_to_bytepos, code_convert_string_norecord, del_range, make_string, pvec_type,
        send_process, vterminal, EmacsInt, Fput_text_property, Lisp_Type, Qbold, Qcursor_type,
        Qface, Qitalic, Qnil, Qnormal, Qt, Qutf_8, Qvtermp, STRING_BYTES,
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

pub const MaximumScrollback: usize = 1000;

pub type LispVterminalRef = ExternalPtr<vterminal>;

impl LispVterminalRef {
    pub fn get_size(self) -> (i32, i32) {
        let mut height: i32 = 0;
        let mut width: i32 = 0;
        unsafe { vterm_get_size((*self).vt, &mut height, &mut width) };
        (height, width)
    }

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

#[lisp_fn(name = "vterm-new")]
pub fn vterminal_new_lisp(rows: EmacsInt, cols: EmacsInt, process: LispObject) -> LispVterminalRef {
    unsafe {
        let mut term = LispVterminalRef::from_ptr(allocate_vterm() as *mut c_void).unwrap();

        (*term).vt = vterm_new(rows as i32, cols as i32);
        vterm_set_utf8((*term).vt, 1);
        term_setup_colors(term);
        (*term).vts = vterm_obtain_screen((*term).vt);
        vterm_screen_reset((*term).vts, 1);

        vterm_screen_set_callbacks(
            (*term).vts,
            &vterm_screen_callbacks,
            term.as_mut() as *mut libc::c_void,
        );

        vterm_screen_set_damage_merge((*term).vts, VTermDamageSize::VTERM_DAMAGE_SCROLL);

        vterm_screen_enable_altscreen((*term).vts, 1);

        (*term).sb_size = MaximumScrollback;
        (*term).sb_current = 0;
        (*term).sb_pending = 0;

        let s = mem::size_of::<VtermScrollbackLine>() * MaximumScrollback;
        (*term).sb_buffer = libc::malloc(s as libc::size_t) as *mut *mut VtermScrollbackLine;

        (*term).invalid_start = 0;
        (*term).invalid_end = rows as i32;

        (*term).cursor.visible = true;
        (*term).pending_resize = false;

        (*term).process = process;

        term
    }
}

unsafe fn vterminal_refresh_screen(mut term: LispVterminalRef) {
    if (*term).invalid_end >= (*term).invalid_start {
        let (_height, width) = term.get_size();

        // vterminal height may have decreased before `invalid_end` reflects it.
        let line_start = row_to_linenr(term.as_mut() as *mut vterminal, (*term).invalid_start);

        goto_pos(ThreadState::current_buffer_unchecked().begv as EmacsInt);
        forward_line(Some(line_start as EmacsInt));

        vterminal_delete_lines(line_start, (*term).invalid_end - (*term).invalid_start);
        refresh_lines(
            term.as_mut() as *mut vterminal,
            (*term).invalid_start,
            (*term).invalid_end,
            width,
        );
    }
    (*term).invalid_start = std::i32::MAX;
    (*term).invalid_end = -1;
}

unsafe fn vterminal_adjust_topline(mut term: LispVterminalRef) {
    let buffer_lnum = line_beginning_position(None) as i32;

    let state: *mut VTermState = vterm_obtain_state((*term).vt);
    let mut pos: VTermPos = std::mem::zeroed();
    vterm_state_get_cursorpos(state, &mut pos);

    let cursor_lnum = row_to_linenr(term.as_mut() as *mut vterminal, pos.row);

    goto_pos(ThreadState::current_buffer_unchecked().begv as EmacsInt);
    forward_line(Some(cmp::min(cursor_lnum, buffer_lnum) as EmacsInt - 1));

    let offset = get_col_offset(term.as_mut() as *mut vterminal, pos.row, pos.col);
    forward_char(LispObject::from((pos.col - offset as i32) as EmacsInt));
}

/// Refresh the scrollback of an invalidated terminal.
unsafe fn vterminal_refresh_scrollback(mut term: LispVterminalRef) {
    let (height, width) = term.get_size();

    let mut buffer_lnum: i32;

    if (*term).sb_pending > 0 {
        buffer_lnum = ThreadState::current_buffer_unchecked().zv as i32;
        let del_cnt = buffer_lnum - height - (*term).sb_size as i32 + (*term).sb_pending;

        if del_cnt > 0 {
            buffer_lnum = ThreadState::current_buffer_unchecked().zv as i32;
        }

        let buf_index = buffer_lnum - height + 1;
        goto_pos(ThreadState::current_buffer_unchecked().begv as EmacsInt);
        forward_line(Some(buf_index as EmacsInt));

        refresh_lines(
            term.as_mut() as *mut vterminal,
            -(*term).sb_pending,
            0,
            width,
        );
        (*term).sb_pending = 0;
    }

    let max_line_count = (*term).sb_current as i32 + height;
    let buffer_lnum = line_beginning_position(None) as i32;

    // Remove extra lines at the bottom
    if buffer_lnum > max_line_count {
        vterminal_delete_lines(max_line_count + 1, buffer_lnum - max_line_count + 1);
    }
}

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

            let is_key =
                |key: *const c_char, val: *const c_char, key_len: size_t, val_len: size_t| {
                    key_len == val_len
                        && libc::memcmp(key as *mut c_void, val as *mut c_void, len) == 0
                };

            if len > 1 && *(key.offset(0)) == 60 {
                if is_key(key, "<return>".as_ptr() as *const c_char, len, 8) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_ENTER, modifier);
                } else if is_key(key, "<tab>".as_ptr() as *const c_char, len, 5) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_TAB, modifier);
                } else if is_key(key, "<backspace>".as_ptr() as *const c_char, len, 11) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_BACKSPACE, modifier);
                } else if is_key(key, "<escape>".as_ptr() as *const c_char, len, 8) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_ESCAPE, modifier);
                } else if is_key(key, "<up>".as_ptr() as *const c_char, len, 4) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_UP, modifier);
                } else if is_key(key, "<down>".as_ptr() as *const c_char, len, 6) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_DOWN, modifier);
                } else if is_key(key, "<left>".as_ptr() as *const c_char, len, 6) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_LEFT, modifier);
                } else if is_key(key, "<right>".as_ptr() as *const c_char, len, 7) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_RIGHT, modifier);
                } else if is_key(key, "<insert>".as_ptr() as *const c_char, len, 8) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_INS, modifier);
                } else if is_key(key, "<delete>".as_ptr() as *const c_char, len, 8) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_DEL, modifier);
                } else if is_key(key, "<home>".as_ptr() as *const c_char, len, 6) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_HOME, modifier);
                } else if is_key(key, "<end>".as_ptr() as *const c_char, len, 5) {
                    vterm_keyboard_key((*vterm).vt, VTermKey::VTERM_KEY_END, modifier);
                } else if is_key(key, "<prior>".as_ptr() as *const c_char, len, 7) {
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
            } else if is_key(key, "SPC".as_ptr() as *const c_char, len, 3) {
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

unsafe fn term_setup_colors(vterm: LispVterminalRef) {
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

#[lisp_fn(name = "vterm-set-size")]
pub fn vterminal_set_size_lisp(mut vterm: LispVterminalRef, rows: EmacsInt, cols: EmacsInt) {
    unsafe {
        let (height, width) = vterm.get_size();

        if !(*vterm).pending_resize {
            if cols as i32 != width || rows as i32 != height {
                vterm.set_size(rows as i32, cols as i32);
                vterm_screen_flush_damage((*vterm).vts);
                (*vterm).pending_resize = true;
                vterminal_refresh_size(vterm);
                vterminal_redraw(vterm);
            }
        }
    }
}

fn vterminal_refresh_size(mut vterm: LispVterminalRef) {
    let (height, _width) = vterm.get_size();

    (*vterm).invalid_start = 0;
    (*vterm).invalid_end = height;
    (*vterm).pending_resize = false;
}

unsafe fn vterminal_redraw(mut vterm: LispVterminalRef) {
    if vterm.is_invalidated {
        if (*vterm).cursor.visible {
            Fset(Qcursor_type, Qt);
        } else {
            Fset(Qcursor_type, Qnil);
        }

        vterminal_refresh_scrollback(vterm);
        vterminal_refresh_screen(vterm);

        vterminal_adjust_topline(vterm);
    }
    vterm.is_invalidated = false;
}

fn vterminal_delete_lines(linenum: i32, count: i32) {
    let mut cur_buf = ThreadState::current_buffer_unchecked();

    goto_pos(cur_buf.begv as EmacsInt);
    forward_line(Some(linenum as EmacsInt - 1));

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

// vterm_screen_callbacks

#[no_mangle]
pub unsafe extern "C" fn vterminal_settermprop(
    prop: VTermProp,
    val: *mut VTermValue,
    user_data: *mut c_void,
) -> i32 {
    let term = user_data as *mut vterminal;

    match prop {
        VTermProp::VTERM_PROP_ALTSCREEN => vterminal_invalidate_terminal(term, -1, -1),
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
pub extern "C" fn rust_syms_of_vterm() {
    def_lisp_sym!(Qvtermp, "vtermp");
}

include!(concat!(env!("OUT_DIR"), "/vterm_exports.rs"));
