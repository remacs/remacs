/* Definitions for interface to indent.c
   Copyright (C) 1985-1986, 2001-2018 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_INDENT_H
#define EMACS_INDENT_H

#include "lisp.h"

struct position
  {
    ptrdiff_t bufpos;
    ptrdiff_t bytepos;
    EMACS_INT hpos;
    EMACS_INT vpos;
    EMACS_INT prevhpos;
    int contin;
  };

struct position *compute_motion (ptrdiff_t from, ptrdiff_t frombyte,
				 EMACS_INT fromvpos, EMACS_INT fromhpos,
				 bool did_motion, ptrdiff_t to,
				 EMACS_INT tovpos, EMACS_INT tohpos,
                                 EMACS_INT width, ptrdiff_t hscroll,
                                 int tab_offset, struct window *);
struct position *vmotion (ptrdiff_t from, ptrdiff_t from_byte,
			  EMACS_INT vtarget, struct window *);
ptrdiff_t skip_invisible (ptrdiff_t pos, ptrdiff_t *next_boundary_p,
                          ptrdiff_t to, Lisp_Object window);

/* Last value returned by current_column.
   Some things in set last_known_column_point to -1
   to mark the memorized value as invalid.  */
extern ptrdiff_t last_known_column;

/* Value of point when current_column was called */
extern ptrdiff_t last_known_column_point;

/* Value of MODIFF when current_column was called.  */
extern EMACS_INT last_known_column_modified;

/* Functions for dealing with the column cache.  */

/* Return true if the display table DISPTAB specifies the same widths
   for characters as WIDTHTAB.  We use this to decide when to
   invalidate the buffer's column_cache.  */
bool disptab_matches_widthtab (struct Lisp_Char_Table *disptab,
			       struct Lisp_Vector *widthtab);

/* Recompute BUF's width table, using the display table DISPTAB.  */
void recompute_width_table (struct buffer *buf,
                            struct Lisp_Char_Table *disptab);

/* Return the current indentation at a given byte offset */
ptrdiff_t position_indentation (ptrdiff_t pos_byte);

void scan_for_column (ptrdiff_t *endpos, EMACS_INT *goalcol, ptrdiff_t *prevcol);

#endif /* EMACS_INDENT_H */
