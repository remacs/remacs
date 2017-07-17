/* Declaration of functions and data types used for MD5 sum computing
   library functions.
   Copyright (C) 1995-1997, 1999-2001, 2004-2006, 2008-2017 Free Software
   Foundation, Inc.
   This file is part of the GNU C Library.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.  */

#ifndef _MD5_H
#define _MD5_H 1

#include <stdio.h>
#include <stdint.h>

#define MD5_DIGEST_SIZE 16
#define MD5_BLOCK_SIZE 64

#ifndef __GNUC_PREREQ
# if defined __GNUC__ && defined __GNUC_MINOR__
#  define __GNUC_PREREQ(maj, min)                                       \
  ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
# else
#  define __GNUC_PREREQ(maj, min) 0
# endif
#endif

#ifndef __THROW
# if defined __cplusplus && __GNUC_PREREQ (2,8)
#  define __THROW       throw ()
# else
#  define __THROW
# endif
#endif

#ifndef _LIBC
# define __md5_buffer md5_buffer
#endif

# ifdef __cplusplus
extern "C" {
# endif

/* Compute MD5 message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *__md5_buffer (const char *buffer, size_t len,
                           void *resblock) __THROW;

# ifdef __cplusplus
}
# endif

#endif /* md5.h */
