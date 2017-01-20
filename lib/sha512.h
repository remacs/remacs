/* Declarations of functions and data types used for SHA512 and SHA384 sum
   library functions.
   Copyright (C) 2005-2006, 2008-2017 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef SHA512_H
# define SHA512_H 1

# include <stdio.h>

# ifdef __cplusplus
extern "C" {
# endif

enum { SHA384_DIGEST_SIZE = 384/8 };
enum { SHA512_DIGEST_SIZE = 512/8 };

/* Compute SHA512 (SHA384) message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *sha512_buffer (const char *buffer, size_t len, void *resblock);
extern void *sha384_buffer (const char *buffer, size_t len, void *resblock);

# ifdef __cplusplus
}
# endif

#endif
