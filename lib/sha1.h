/* Declarations of functions and data types used for SHA1 sum
   library functions.
   Copyright (C) 2000-2001, 2003, 2005-2006, 2008-2017 Free Software
   Foundation, Inc.

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

#ifndef SHA1_H
# define SHA1_H 1

# define SHA1_DIGEST_SIZE 20

# include <stdio.h>
# include <stdint.h>

# ifdef __cplusplus
extern "C" {
# endif

/* Structure to save state of computation between the single steps.  */
struct sha1_ctx;

/* Initialize structure containing state of computation. */
extern struct sha1_ctx *sha1_ctx_new (void);


/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of 64.  */
extern void sha1_process_bytes (const void *buffer, size_t len,
                                struct sha1_ctx *ctx);

/* Process the remaining bytes in the buffer and put result from CTX
   in first 20 bytes following RESBUF.  The result is always in little
   endian byte order, so that a byte-wise output yields to the wanted
   ASCII representation of the message digest.  */
extern void *sha1_finish_ctx (struct sha1_ctx *ctx, void *resbuf);

/* Compute SHA1 message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *sha1_buffer (const char *buffer, size_t len, void *resblock);

# ifdef __cplusplus
}
# endif

#endif
