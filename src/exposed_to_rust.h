#ifndef EXPOSED_TO_RUST_H
#define EXPOSED_TO_RUST_H

#include "lisp.h"

#include <stdio.h>

/* buffer.h */
struct infile
{
  /* The input stream.  */
  FILE *stream;

  /* Lookahead byte count.  */
  signed char lookahead;

  /* Lookahead bytes, in reverse order.  Keep these here because it is
     not portable to ungetc more than one byte at a time.  */
  unsigned char buf[MAX_MULTIBYTE_LENGTH - 1];
};

/* Defined in buffer.c.  */
extern struct infile* infile;

/* casefiddle.c */
enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

Lisp_Object casify_object (enum case_action flag, Lisp_Object obj);
ptrdiff_t casify_region (enum case_action flag, Lisp_Object b, Lisp_Object e);
Lisp_Object casify_pnc_region (enum case_action flag, Lisp_Object beg, Lisp_Object end,
                               Lisp_Object region_noncontiguous_p);

/* charset.h */
typedef struct
{
  /* The current charset for which the following tables are setup.  */
  struct charset *current;

  /* 1 iff the following table is used for encoder.  */
  short for_encoder;

  /* When the following table is used for encoding, minimum and
     maximum character of the current charset.  */
  int min_char, max_char;

  /* A Unicode character corresponding to the code index 0 (i.e. the
     minimum code-point) of the current charset, or -1 if the code
     index 0 is not a Unicode character.  This is checked when
     table.encoder[CHAR] is zero.  */
  int zero_index_char;

  union {
    /* Table mapping code-indices (not code-points) of the current
       charset to Unicode characters.  If decoder[CHAR] is -1, CHAR
       doesn't belong to the current charset.  */
    int decoder[0x10000];
    /* Table mapping Unicode characters to code-indices of the current
       charset.  The first 0x10000 elements are for BMP (0..0xFFFF),
       and the last 0x10000 are for SMP (0x10000..0x1FFFF) or SIP
       (0x20000..0x2FFFF).  Note that there is no charset map that
       uses both SMP and SIP.  */
    unsigned short encoder[0x20000];
  } table;
} TempCharsetWork;

extern TempCharsetWork *temp_charset_work;

/* window.c */
struct save_window_data
  {
    union vectorlike_header header;
    Lisp_Object selected_frame;
    Lisp_Object current_window;
    Lisp_Object f_current_buffer;
    Lisp_Object minibuf_scroll_window;
    Lisp_Object minibuf_selected_window;
    Lisp_Object root_window;
    Lisp_Object focus_frame;
    /* A vector, each of whose elements is a struct saved_window
       for one window.  */
    Lisp_Object saved_windows;

    /* All fields above are traced by the GC.
       From `frame-cols' down, the fields are ignored by the GC.  */
    /* We should be able to do without the following two.  */
    int frame_cols, frame_lines;
    /* These two should get eventually replaced by their pixel
       counterparts.  */
    int frame_menu_bar_lines, frame_tool_bar_lines;
    int frame_text_width, frame_text_height;
    /* These are currently unused.  We need them as soon as we convert
       to pixels.  */
    int frame_menu_bar_height, frame_tool_bar_height;
  };

/* This is saved as a Lisp_Vector.  */
struct saved_window
{
  union vectorlike_header header;

  Lisp_Object window, buffer, start, pointm, old_pointm;
  Lisp_Object pixel_left, pixel_top, pixel_height, pixel_width;
  Lisp_Object pixel_height_before_size_change, pixel_width_before_size_change;
  Lisp_Object left_col, top_line, total_cols, total_lines;
  Lisp_Object normal_cols, normal_lines;
  Lisp_Object hscroll, min_hscroll, hscroll_whole, suspend_auto_hscroll;
  Lisp_Object parent, prev;
  Lisp_Object start_at_line_beg;
  Lisp_Object display_table;
  Lisp_Object left_margin_cols, right_margin_cols;
  Lisp_Object left_fringe_width, right_fringe_width, fringes_outside_margins;
  Lisp_Object scroll_bar_width, vertical_scroll_bar_type, dedicated;
  Lisp_Object scroll_bar_height, horizontal_scroll_bar_type;
  Lisp_Object combination_limit, window_parameters;
};

#endif /* EXPOSED_TO_RUST_H */
