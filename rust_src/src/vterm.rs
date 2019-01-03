//! libvterm facilities

use remacs_macros::lisp_fn;

use libc::{c_char, c_int, c_uchar, c_void};

use std::{cmp, mem};

use crate::{
    cmds::{forward_char, forward_line},
    editfns::{goto_pos, line_beginning_position, line_end_position},
    lisp::ExternalPtr,
    lisp::{defsubr, LispObject},
    multibyte::LispStringRef,
    obarray::intern,

    remacs_sys::{
        allocate_vterm, color_to_rgb_string, get_col_offset, mysave_value, refresh_lines,
        rgb_string_to_color, row_to_linenr, term_process_key, vterm_output_read,
        vterm_sb_buffer_size, vterm_screen_callbacks, vterm_screen_set_callbacks,
    },

    remacs_sys::{
        buf_charpos_to_bytepos, code_convert_string_norecord, del_range, make_string, send_process,
        vterminal, EmacsInt, Fput_text_property, Lisp_Misc_Type, Qbold, Qface, Qitalic, Qnil,
        Qnormal, Qt, Qutf_8, Qvtermp, STRING_BYTES,
    },

    // vterm
    remacs_sys::{
        vterm_get_size, vterm_input_write, vterm_new, vterm_obtain_screen, vterm_obtain_state,
        vterm_output_get_buffer_current, vterm_screen_enable_altscreen, vterm_screen_flush_damage,
        vterm_screen_reset, vterm_screen_set_damage_merge, vterm_set_size, vterm_set_utf8,
        vterm_state_get_cursorpos, vterm_state_set_default_colors, vterm_state_set_palette_color,
        VTermColor, VTermDamageSize, VTermModifier, VTermPos, VTermRect, VTermScreenCell,
        VTermState,
    },
    threads::ThreadState,
    vectors::length,
};

pub const MaximumScrollback: usize = 1000;

pub type LispVterminalRef = ExternalPtr<vterminal>;

impl LispVterminalRef {
    pub fn as_lisp_obj(self) -> LispObject {
        unsafe { mem::transmute(self.as_ptr()) }
    }

    pub fn get_size(self) -> (c_int, c_int) {
        let mut height: i32 = 0;
        let mut width: i32 = 0;
        unsafe { vterm_get_size((*self).vt, &mut height, &mut width) };
        (height, width)
    }

    pub fn set_size(self, rows: EmacsInt, cols: EmacsInt) {
        unsafe {
            vterm_set_size((*self).vt, rows as c_int, cols as c_int);
        }
    }
}

// TODO: update when vterminal is a pseudovector
impl LispObject {
    pub fn is_vterminal(self) -> bool {
        self.as_misc().map_or(false, |m| {
            m.get_type() == Lisp_Misc_Type::Lisp_Misc_Save_Value
        })
    }

    pub fn as_vterminal(self) -> Option<LispVterminalRef> {
        if self.is_vterminal() {
            unsafe { Some(mem::transmute(mysave_value(self))) }
        } else {
            None
        }
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
        v.as_lisp_obj()
    }
}

impl From<LispObject> for Option<LispVterminalRef> {
    fn from(o: LispObject) -> Self {
        o.as_vterminal()
    }
}

#[lisp_fn(name = "vterm-new")]
pub fn vterm_new_lisp(rows: EmacsInt, cols: EmacsInt, process: LispObject) -> LispObject {
    unsafe {
        let val = allocate_vterm();

        let mut term = val.as_vterminal().unwrap();

        (*term).vt = vterm_new(rows as c_int, cols as c_int);
        vterm_set_utf8((*term).vt, 1);
        term_setup_colors(term);
        (*term).vts = vterm_obtain_screen((*term).vt);
        vterm_screen_reset((*term).vts, 1);

        vterm_screen_set_callbacks(
            (*term).vts,
            &mut vterm_screen_callbacks,
            term.as_mut() as *mut libc::c_void,
        );

        vterm_screen_set_damage_merge((*term).vts, VTermDamageSize::VTERM_DAMAGE_SCROLL);

        vterm_screen_enable_altscreen((*term).vts, 1);

        (*term).sb_size = MaximumScrollback;
        (*term).sb_current = 0;
        (*term).sb_pending = 0;

        vterm_sb_buffer_size(term.as_mut() as *mut vterminal);

        (*term).invalid_start = 0;
        (*term).invalid_end = rows as c_int;

        (*term).process = process;

        val
    }
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_refresh_screen(mut term: LispVterminalRef) {
    if (*term).invalid_end >= (*term).invalid_start {
        let (height, width) = term.get_size();

        // vterminal height may have decreased before `invalid_end` reflects it.
        let line_start = row_to_linenr(term.as_mut() as *mut vterminal, (*term).invalid_start);

        goto_pos(ThreadState::current_buffer().begv as EmacsInt);
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

pub unsafe extern "C" fn vterminal_adjust_topline(mut term: LispVterminalRef, added: i64) {
    let (height, width) = term.get_size();

    let buffer_lnum = line_beginning_position(None) as c_int;

    let state: *mut VTermState = vterm_obtain_state((*term).vt);
    let mut pos: VTermPos = std::mem::zeroed();
    vterm_state_get_cursorpos(state, &mut pos);

    let cursor_lnum = row_to_linenr(term.as_mut() as *mut vterminal, pos.row);

    goto_pos(ThreadState::current_buffer().begv as EmacsInt);
    forward_line(Some(cmp::min(cursor_lnum, buffer_lnum) as EmacsInt - 1));

    let offset = get_col_offset(term.as_mut() as *mut vterminal, pos.row, pos.col);
    forward_char(LispObject::from((pos.col - offset as c_int) as EmacsInt));
}

/// Refresh the scrollback of an invalidated terminal.
#[no_mangle]
pub unsafe extern "C" fn vterminal_refresh_scrollback(mut term: LispVterminalRef) {
    let (height, width) = term.get_size();

    let mut buffer_lnum: c_int;

    if (*term).sb_pending > 0 {
        buffer_lnum = ThreadState::current_buffer().zv as c_int;
        let del_cnt = buffer_lnum - height - (*term).sb_size as c_int + (*term).sb_pending;

        if del_cnt > 0 {
            buffer_lnum = ThreadState::current_buffer().zv as c_int;
        }

        let buf_index = buffer_lnum - height + 1;
        goto_pos(ThreadState::current_buffer().begv as EmacsInt);
        forward_line(Some(buf_index as EmacsInt));

        refresh_lines(
            term.as_mut() as *mut vterminal,
            -(*term).sb_pending,
            0,
            width,
        );
        (*term).sb_pending = 0;
    }

    let max_line_count = (*term).sb_current as c_int + height;
    let buffer_lnum = line_beginning_position(None) as c_int;

    // Remove extra lines at the bottom
    if buffer_lnum > max_line_count {
        vterminal_delete_lines(max_line_count + 1, buffer_lnum - max_line_count + 1);
    }
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_redraw(mut vterm: LispVterminalRef) {
    if vterm.is_invalidated {
        let bufline_before = line_beginning_position(None);
        vterminal_refresh_scrollback(vterm);
        vterminal_refresh_screen(vterm);

        let line_added = line_beginning_position(None) - bufline_before;
        vterminal_adjust_topline(vterm, line_added);
    }
    vterm.is_invalidated = false;
}

#[lisp_fn(min = "1", name = "vterm-update")]
pub fn vterminal_update(
    mut vterm: LispVterminalRef,
    string: LispObject,
    key: LispObject,
    shift: bool,
    meta: bool,
    ctrl: bool,
) -> LispObject {
    unsafe {
        if string.is_not_nil() {
            let mut utf8 = code_convert_string_norecord(string, Qutf_8, true).as_string_or_error();
            let mut len = STRING_BYTES(utf8.as_mut()) as isize + 1;

            let mut v: Vec<c_uchar> = Vec::with_capacity(len as usize);

            let key = libc::memcpy(
                v.as_mut_ptr() as *mut c_void,
                utf8.data_ptr() as *mut c_void,
                len as libc::size_t,
            ) as *mut c_uchar;

            let mut modifier = VTermModifier::VTERM_MOD_NONE;
            if shift {
                modifier = modifier | VTermModifier::VTERM_MOD_SHIFT;
            }

            if meta {
                modifier = modifier | VTermModifier::VTERM_MOD_ALT;
            }

            if ctrl {
                modifier = modifier | VTermModifier::VTERM_MOD_CTRL;
            }

            term_process_key(
                vterm.as_mut() as *mut vterminal,
                key,
                len as usize - 1,
                modifier,
            );
        }

        vterminal_flush_output(vterm);
        vterminal_redraw(vterm);
        LispObject::from(0)
    }
}

#[no_mangle]
pub unsafe extern "C" fn term_setup_colors(vterm: LispVterminalRef) {
    let mut fg: VTermColor = std::mem::zeroed();
    let mut bg: VTermColor = std::mem::zeroed();
    let state: *mut VTermState = vterm_obtain_state((*vterm).vt);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm")),
        LispObject::from(intern(":foreground"))
    ));
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_default_colors(state, &mut fg, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-black")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 0, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-black")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 8, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-red")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 1, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-red")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 9, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-green")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 2, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-green")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 10, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-yellow")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 3, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-yellow")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 11, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-blue")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 4, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-blue")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 12, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-magenta")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 5, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-magenta")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 13, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-cyan")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 6, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-cyan")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 14, &mut bg);

    fg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-white")),
        LispObject::from(intern(":foreground"))
    ));
    vterm_state_set_palette_color(state, 7, &mut fg);
    bg = rgb_string_to_color(call!(
        LispObject::from(intern("vterm--face-color-hex")),
        LispObject::from(intern("vterm-color-white")),
        LispObject::from(intern(":background"))
    ));
    vterm_state_set_palette_color(state, 15, &mut bg);
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_render_text(
    buffer: *mut c_char,
    len: c_int,
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

#[no_mangle]
pub unsafe extern "C" fn vterminal_flush_output(term: LispVterminalRef) {
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
pub fn vterminal_write_input(mut vterm: LispVterminalRef, mut input: LispStringRef) -> EmacsInt {
    unsafe {
        vterm_input_write(
            (*vterm).vt,
            input.sdata_ptr(),
            STRING_BYTES(input.as_mut()) as usize + 1,
        );
        vterm_screen_flush_damage((*vterm).vts);
    }

    0 as EmacsInt
}

#[lisp_fn(name = "vterm-set-size")]
pub fn vterminal_set_size_lisp(
    mut vterm: LispVterminalRef,
    rows: EmacsInt,
    cols: EmacsInt,
) -> LispObject {
    unsafe {
        let (height, width) = vterm.get_size();

        if cols as c_int != width || rows as c_int != height {
            vterm.set_size(rows, cols);
            vterm_screen_flush_damage((*vterm).vts);
            vterminal_redraw(vterm);
        }
    }
    Qnil
}

#[no_mangle]
pub extern "C" fn vterminal_delete_lines(linenum: c_int, count: c_int) {
    let mut cur_buf = ThreadState::current_buffer();

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
pub unsafe extern "C" fn vterminal_invalidate_terminal(
    term: *mut vterminal,
    start_row: c_int,
    end_row: c_int,
) {
    if !(start_row == -1 || end_row == -1) {
        (*term).invalid_start = cmp::min((*term).invalid_start, start_row);
        (*term).invalid_end = cmp::max((*term).invalid_end, end_row);
    }
    (*term).is_invalidated = true;
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_damage(rect: VTermRect, data: *mut c_void) -> c_int {
    vterminal_invalidate_terminal(data as *mut vterminal, rect.start_row, rect.end_row);
    1
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_moverect(
    dest: VTermRect,
    src: VTermRect,
    data: *mut c_void,
) -> c_int {
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
    visible: c_int,
    data: *mut libc::c_void,
) -> c_int {
    let term: *mut vterminal = data as *mut vterminal;
    (*term).cursor.row = new.row;
    (*term).cursor.col = new.col;
    vterminal_invalidate_terminal(term, old.row, old.row + 1);
    vterminal_invalidate_terminal(term, new.row, new.row + 1);

    return 1;
}

#[no_mangle]
pub unsafe extern "C" fn vterminal_resize(
    rows: c_int,
    cols: c_int,
    data: *mut libc::c_void,
) -> c_int {
    // can not use invalidate_terminal here
    // when the window heigh decreased,
    // the value of term->invalid_end can't bigger than window height
    let term = data as *mut vterminal;
    (*term).invalid_start = 0;
    (*term).invalid_end = rows;
    vterminal_invalidate_terminal(term, -1, -1);
    1
}

#[no_mangle]
pub extern "C" fn rust_syms_of_vterm() {
    def_lisp_sym!(Qvtermp, "vtermp");
}

include!(concat!(env!("OUT_DIR"), "/vterm_exports.rs"));
