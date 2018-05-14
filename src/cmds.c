/* Simple built-in editing commands.

Copyright (C) 1985, 1993-1998, 2001-2017 Free Software Foundation, Inc.

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


#include <config.h>

#include "lisp.h"
#include "commands.h"
#include "character.h"
#include "buffer.h"
#include "syntax.h"
#include "keyboard.h"
#include "keymap.h"
#include "frame.h"

/* module initialization */

extern void rust_syms_of_cmds(void);

void
syms_of_cmds (void)
{
  rust_syms_of_cmds();
  DEFVAR_LISP ("post-self-insert-hook", Vpost_self_insert_hook,
	       doc: /* Hook run at the end of `self-insert-command'.
This is run after inserting the character.  */);
  Vpost_self_insert_hook = Qnil;
}
