/* sig2str.h -- convert between signal names and numbers

   Copyright (C) 2002, 2005, 2009-2018 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

#include <signal.h>

/* Don't override system declarations of SIG2STR_MAX, sig2str, str2sig.  */
#ifndef SIG2STR_MAX

/* Size of a buffer needed to hold a signal name like "HUP".  */
# define SIG2STR_MAX 5

#ifdef __cplusplus
extern "C" {
#endif

int str2sig (char const *, int *);

#ifdef __cplusplus
}
#endif

#endif
