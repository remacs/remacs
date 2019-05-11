#include <config.h>
#ifdef HAVE_LIBVTERM

#include <inttypes.h>
#include <stdbool.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>

#include "vterm.h"

#include "lisp.h"
#include "coding.h"


void fetch_cell(vterminal *, int , int , VTermScreenCell *);
static bool compare_cells(VTermScreenCell *, VTermScreenCell *);


static int vterminal_sb_push(int cols, const VTermScreenCell *cells, void *data) {
  vterminal *term = (vterminal *)data;

  if (!term->sb_size) {
    return 0;
  }

  // copy vterm cells into sb_buffer
  size_t c = (size_t)cols;
  VtermScrollbackLine *sbrow = NULL;
  if (term->sb_current == term->sb_size) {
    if (term->sb_buffer[term->sb_current - 1]->cols == c) {
      // Recycle old row if it's the right size
      sbrow = term->sb_buffer[term->sb_current - 1];
    } else {
      free(term->sb_buffer[term->sb_current - 1]);
    }

    // Make room at the start by shifting to the right.
    memmove(term->sb_buffer + 1, term->sb_buffer,
            sizeof(term->sb_buffer[0]) * (term->sb_current - 1));

  } else if (term->sb_current > 0) {
    // Make room at the start by shifting to the right.
    memmove(term->sb_buffer + 1, term->sb_buffer,
            sizeof(term->sb_buffer[0]) * term->sb_current);
  }

  if (!sbrow) {
    sbrow = malloc(sizeof(VtermScrollbackLine) + c * sizeof(sbrow->cells[0]));
    sbrow->cols = c;
  }

  // New row is added at the start of the storage buffer.
  term->sb_buffer[0] = sbrow;
  if (term->sb_current < term->sb_size) {
    term->sb_current++;
  }

  if (term->sb_pending < (int)term->sb_size) {
    term->sb_pending++;
  }

  memcpy(sbrow->cells, cells, sizeof(cells[0]) * c);

  return 1;
}

/// Scrollback pop handler (from pangoterm).
///
/// @param cols
/// @param cells  VTerm state to update.
/// @param data   Term
static int vterminal_sb_pop(int cols, VTermScreenCell *cells, void *data) {
  vterminal *term = (vterminal *)data;

  if (!term->sb_current) {
    return 0;
  }

  if (term->sb_pending) {
    term->sb_pending--;
  }

  VtermScrollbackLine *sbrow = term->sb_buffer[0];
  term->sb_current--;
  // Forget the "popped" row by shifting the rest onto it.
  memmove(term->sb_buffer, term->sb_buffer + 1,
          sizeof(term->sb_buffer[0]) * (term->sb_current));

  size_t cols_to_copy = (size_t)cols;
  if (cols_to_copy > sbrow->cols) {
    cols_to_copy = sbrow->cols;
  }

  // copy to vterm state
  memcpy(cells, sbrow->cells, sizeof(cells[0]) * cols_to_copy);
  size_t col;
  for (col = cols_to_copy; col < (size_t)cols; col++) {
    cells[col].chars[0] = 0;
    cells[col].width = 1;
  }

  free(sbrow);

  return 1;
}

static size_t
codepoint_to_utf8(const uint32_t codepoint, unsigned char buffer[4]) {
  if (codepoint <= 0x7F) {
    buffer[0] = codepoint;
    return 1;
  }
  if (codepoint >= 0x80 && codepoint <= 0x07FF) {
    buffer[0] = 0xC0 | (codepoint >> 6);
    buffer[1] = 0x80 | (codepoint & 0x3F);
    return 2;
  }
  if (codepoint >= 0x0800 && codepoint <= 0xFFFF) {
    buffer[0] = 0xE0 | (codepoint >> 12);
    buffer[1] = 0x80 | ((codepoint >> 6) & 0x3F);
    buffer[2] = 0x80 | (codepoint & 0x3F);
    return 3;
  }

  if (codepoint >= 0x10000 && codepoint <= 0x10FFFF) {
    buffer[0] = 0xF0 | (codepoint >> 18);
    buffer[1] = 0x80 | ((codepoint >> 12) & 0x3F);
    buffer[2] = 0x80 | ((codepoint >> 6) & 0x3F);
    buffer[3] = 0x80 | (codepoint & 0x3F);
    return 4;
  }
  return 0;
}

bool
utf8_to_codepoint(const unsigned char buffer[4], const size_t len,
                       uint32_t *codepoint) {
  *codepoint = 0;
  if (len == 1 && buffer[0] <= 0x7F) {
    *codepoint = buffer[0];
    return true;
  }
  if (len == 2 && (buffer[0] >= 0xC0 && buffer[0] <= 0xDF) &&
      (buffer[1] >= 0x80 && buffer[1] <= 0xBF)) {
    *codepoint = buffer[0] & 0x1F;
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[1] & 0x3F);
    return true;
  }
  if (len == 3 && (buffer[0] >= 0xE0 && buffer[0] <= 0xEF) &&
      (buffer[1] >= 0x80 && buffer[1] <= 0xBF) &&
      (buffer[2] >= 0x80 && buffer[2] <= 0xBF)) {
    *codepoint = buffer[0] & 0xF;
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[1] & 0x3F);
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[2] & 0x3F);
    return true;
  }
  if (len == 4 && (buffer[0] >= 0xF0 && buffer[0] <= 0xF7) &&
      (buffer[1] >= 0x80 && buffer[1] <= 0xBF) &&
      (buffer[2] >= 0x80 && buffer[2] <= 0xBF) &&
      (buffer[3] >= 0x80 && buffer[3] <= 0xBF)) {
    *codepoint = buffer[0] & 7;
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[1] & 0x3F);
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[2] & 0x3F);
    *codepoint = *codepoint << 6;
    *codepoint = *codepoint | (buffer[3] & 0x3F);
    return true;
  }

  return false;
}

int row_to_linenr(vterminal *term, int row) {
  return row != INT_MAX ? row + (int)term->sb_current + 1 : INT_MAX;
}

void
fetch_cell(vterminal *term, int row, int col, VTermScreenCell *cell) {
  if (row < 0) {
    VtermScrollbackLine *sbrow = term->sb_buffer[-row - 1];
    if ((size_t)col < sbrow->cols) {
      *cell = sbrow->cells[col];
    } else {
      // fill the pointer with an empty cell
      VTermColor fg, bg;
      VTermState *state = vterm_obtain_state(term->vt);
      vterm_state_get_default_colors(state, &fg, &bg);

      *cell = (VTermScreenCell){.chars = {0}, .width = 1, .bg = bg};
    }
  } else {
    vterm_screen_get_cell(term->vts, (VTermPos){.row = row, .col = col}, cell);
  }
}

bool
is_eol(vterminal *term, int end_col, int row, int col) {
  /* This cell is EOL if this and every cell to the right is black */
  if (row >= 0) {
    VTermPos pos = {.row = row, .col = col};
    return vterm_screen_is_eol(term->vts, pos);
  }

  VtermScrollbackLine *sbrow = term->sb_buffer[-row - 1];
  int c;
  for (c = col; c < end_col && c < sbrow->cols;) {
    if (sbrow->cells[c].chars[0]) {
      return 0;
    }
    c += sbrow->cells[c].width;
  }
  return 1;
}

static bool compare_cells(VTermScreenCell *a, VTermScreenCell *b) {
  bool equal = true;
  equal = equal && (a->fg.red == b->fg.red);
  equal = equal && (a->fg.green == b->fg.green);
  equal = equal && (a->fg.blue == b->fg.blue);
  equal = equal && (a->bg.red == b->bg.red);
  equal = equal && (a->bg.green == b->bg.green);
  equal = equal && (a->bg.blue == b->bg.blue);
  equal = equal && (a->attrs.bold == b->attrs.bold);
  equal = equal && (a->attrs.underline == b->attrs.underline);
  equal = equal && (a->attrs.italic == b->attrs.italic);
  equal = equal && (a->attrs.reverse == b->attrs.reverse);
  equal = equal && (a->attrs.strike == b->attrs.strike);
  return equal;
}

vterminal*
allocate_vterm (void) {
  return ALLOCATE_ZEROED_PSEUDOVECTOR
    (vterminal, vt, PVEC_VTERMINAL);
}

Lisp_Object
refresh_lines (vterminal *term, int start_row, int end_row, int end_col) {

  int i, j;

  char buffer[((end_row - start_row + 1) * end_col) * 4];
  int length = 0;
  VTermScreenCell cell;
  VTermScreenCell lastCell;
  fetch_cell(term, start_row, 0, &lastCell);

  int offset = 0;
  for (i = start_row; i < end_row; i++) {
    for (j = 0; j < end_col; j++) {
      fetch_cell(term, i, j, &cell);

      if (!compare_cells(&cell, &lastCell)) {
        Lisp_Object text = vterminal_render_text(buffer, length, &lastCell);
        call1(intern ("insert"), text);
        length = 0;
      }
      
      lastCell = cell;
      if (cell.chars[0] == 0) {
        if (is_eol(term, end_col, i, j)) {
          /* This cell is EOL if this and every cell to the right is black */
          break;
        }
        buffer[length] = ' ';
        length++;
      } else {
        unsigned char bytes[4];
        size_t count = codepoint_to_utf8(cell.chars[0], bytes);
        for (int k = 0; k < count; k++) {
          buffer[length] = bytes[k];
          length++;
        }
      }

      if (cell.width > 1) {
        int w = cell.width - 1;
        offset += w;
        j = j + w;
      }
    }

    buffer[length] = '\n';
    length++;
  }
  Lisp_Object text = vterminal_render_text(buffer, length, &lastCell);
  call1(intern ("insert"), text);

  return text; 
}

VTermScreenCallbacks vterm_screen_callbacks = {
    .damage = vterminal_damage,
    .moverect = vterminal_moverect,
    .movecursor = vterminal_movecursor,
    .settermprop = vterminal_settermprop,
    .resize = vterminal_resize,
    .sb_pushline = vterminal_sb_push,
    .sb_popline = vterminal_sb_pop,
};

bool
vterm_module_copy_string_contents (Lisp_Object lisp_str, char *buffer, ptrdiff_t *length)
{
  CHECK_STRING (lisp_str);

  Lisp_Object lisp_str_utf8 = ENCODE_UTF_8 (lisp_str);
  ptrdiff_t raw_size = SBYTES (lisp_str_utf8);
  ptrdiff_t required_buf_size = raw_size + 1;

  if (buffer == NULL)
    {
      *length = required_buf_size;
      return true;
    }

  if (*length < required_buf_size)
    {
      *length = required_buf_size;
      xsignal0 (Qargs_out_of_range);
    }

  *length = required_buf_size;
  memcpy (buffer, SDATA (lisp_str_utf8), raw_size + 1);

  return true;
}

void byte_to_hex(uint8_t byte, char *hex) { snprintf(hex, 3, "%.2X", byte); }

Lisp_Object
color_to_rgb_string(VTermColor color) {
  char buffer[8];
  buffer[0] = '#';
  buffer[7] = '\0';
  byte_to_hex(color.red, buffer + 1);
  byte_to_hex(color.green, buffer + 3);
  byte_to_hex(color.blue, buffer + 5);

  return make_string (buffer, 7);
}

uint8_t hex_to_byte(char *hex) { return strtoul(hex, NULL, 16); }

VTermColor
rgb_string_to_color(Lisp_Object string) {
  VTermColor color;
  ptrdiff_t len = 8;
  char buffer[len];
  char hex[3];
  
  vterm_module_copy_string_contents (string, buffer, &len);

  hex[0] = buffer[1];
  hex[1] = buffer[2];
  color.red = hex_to_byte(hex);
  hex[0] = buffer[3];
  hex[1] = buffer[4];
  color.green = hex_to_byte(hex);
  hex[0] = buffer[5];
  hex[1] = buffer[6];
  color.blue = hex_to_byte(hex);

  return color;
};

#endif
