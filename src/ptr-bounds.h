/* Pointer bounds checking for GNU Emacs

Copyright 2017 Free Software Foundation, Inc.

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

#ifndef PTR_BOUNDS_H
#define PTR_BOUNDS_H

#include <stddef.h>

/* When not checking pointer bounds, the following macros simply
   return their first argument.  These macros return either void *, or
   the same type as their first argument.  */

/* Return a copy of P, but with the bounds of Q.  */
#ifdef __CHKP__
# define ptr_bounds_copy(p, q) __builtin___bnd_copy_ptr_bounds (p, q)
#else
# define ptr_bounds_copy(p, q) ((void) (void const *) {q}, p)
#endif

/* Return a copy of P, but with infinite bounds.
   This is a loophole in pointer bounds checking.  */
#ifdef __CHKP__
# define ptr_bounds_init(p) __builtin___bnd_init_ptr_bounds (p)
#else
# define ptr_bounds_init(p) (p)
#endif

/* Return a copy of P, but with bounds [P, P + N).
   This is a loophole in pointer bounds checking.  */
#ifdef __CHKP__
# define ptr_bounds_set(p, n) __builtin___bnd_set_ptr_bounds (p, n)
#else
# define ptr_bounds_set(p, n) ((void) (size_t) {n}, p)
#endif

#endif /* PTR_BOUNDS_H */
