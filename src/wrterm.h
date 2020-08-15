#ifndef __WRTERM_H_
#define __WRTERM_H_

#include "dispextern.h"

struct x_bitmap_record
{
  Pixmap pixmap;
  bool have_mask;
  Pixmap mask;
  char *file;
  int refcount;
  /* Record some info about this pixmap.  */
  int height, width, depth;
};


typedef int Screen;

struct wr_display_info
{
  /* Chain of all w32_display_info structures.  */
  struct w32_display_info *next;

  /* This is a cons cell of the form (NAME . FONT-LIST-CACHE).  */
  Lisp_Object name_list_element;

  /* Dots per inch of the screen.  */
  double resx, resy;

  /* The Screen this connection is connected to.  */
  Screen *screen;

  /* Number of planes on this screen.  */
  int n_planes;

  /* Mask of things that cause the mouse to be grabbed.  */
  int grabbed;

  /* The root window of this screen.  */
  Window root_window;

  /* The cursor to use for vertical scroll bars.  */
  Cursor vertical_scroll_bar_cursor;

  /* Resource data base */
  XrmDatabase xrdb;

  /* Minimum width over all characters in all fonts in font_table.  */
  int smallest_char_width;

  /* Minimum font height over all fonts in font_table.  */
  int smallest_font_height;

  /* Information about the range of text currently shown in
     mouse-face.  */
  Mouse_HLInfo mouse_highlight;

  /* The number of fonts actually stored in w32_font_table.
     font_table[n] is used and valid if 0 <= n < n_fonts. 0 <=
     n_fonts <= font_table_size. and font_table[i].name != 0. */
  int n_fonts;

  /* Pointer to bitmap records.  */
  struct x_bitmap_record *bitmaps;

  /* Allocated size of bitmaps field.  */
  ptrdiff_t bitmaps_size;

  /* Last used bitmap index.  */
  ptrdiff_t bitmaps_last;


  /* The frame which currently has the visual highlight, and should get
     keyboard input (other sorts of input have the frame encoded in the
     event).  It points to the focus frame's selected window's
     frame.  It differs from w32_focus_frame when we're using a global
     minibuffer.  */
  struct frame *x_highlight_frame;

  /* The frame where the mouse was last time we reported a mouse event.  */
  struct frame *last_mouse_frame;

  /* The frame where the mouse was last time we reported a mouse motion.  */
  struct frame *last_mouse_motion_frame;

  /* Position where the mouse was last time we reported a motion.
     This is a position on last_mouse_motion_frame.  */
  int last_mouse_motion_x;
  int last_mouse_motion_y;


  /* Bits and shifts to use to compose pixel values on TrueColor visuals.  */
  int red_bits, blue_bits, green_bits;

  /* This says how to access this display in Xlib.  */
  Display *display;
};


struct wr_output
{

  /* The X window that is the parent of this X window.
     Usually this is a window that was made by the window manager,
     but it can be the root window, and it can be explicitly specified
     (see the explicit_parent field, below).  */
  Window parent_desc;

  /* Descriptor for the cursor in use for this window.  */
  Cursor text_cursor;
  Cursor nontext_cursor;
  Cursor modeline_cursor;
  Cursor hand_cursor;
  Cursor hourglass_cursor;
  Cursor horizontal_drag_cursor;
  Cursor vertical_drag_cursor;
  Cursor left_edge_cursor;
  Cursor top_left_corner_cursor;
  Cursor top_edge_cursor;
  Cursor top_right_corner_cursor;
  Cursor right_edge_cursor;
  Cursor bottom_right_corner_cursor;
  Cursor bottom_edge_cursor;
  Cursor bottom_left_corner_cursor;

  /* True means tried already to make this frame visible.  */
  bool asked_for_visible : 1;

  /* True if this frame was ever previously visible.  */
  bool has_been_visible : 1;

  /* This is the Emacs structure for the X display this frame is on.  */
  struct wr_display_info *display_info;

  /* Default ASCII font of this frame.  */
  struct font *font;

};

extern struct x_display_info *x_display_list;


/* This is the `Display *' which frame F is on.  */
#define FRAME_X_DISPLAY(f) (FRAME_DISPLAY_INFO (f)->display)

/* This gives the x_display_info structure for the display F is on.  */
#define FRAME_DISPLAY_INFO(f) (FRAME_X_OUTPUT (f)->display_info)

/* Return the X output data for frame F.  */
#define FRAME_X_OUTPUT(f) ((f)->output_data.wr)

#define FRAME_FONT(f) (FRAME_X_OUTPUT (f)->font)

/* This is the `Screen *' which frame F is on.  */
#define FRAME_X_SCREEN(f) (FRAME_DISPLAY_INFO (f)->screen)


struct wr_bitmap_record
{
  char *file;
  int refcount;
  int height, width, depth;
};

#endif // __WRTERM_H_
