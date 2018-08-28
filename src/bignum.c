/* Big numbers for Emacs.

Copyright 2018 Free Software Foundation, Inc.

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

#include "bignum.h"

#include "lisp.h"

/* Return the value of the Lisp bignum N, as a double.  */
double
bignum_to_double (Lisp_Object n)
{
  return mpz_get_d (XBIGNUM (n)->value);
}

/* Return D, converted to a bignum.  Discard any fraction.  */
Lisp_Object
double_to_bignum (double d)
{
  mpz_t z;
  mpz_init_set_d (z, d);
  Lisp_Object result = make_integer (z);
  mpz_clear (z);
  return result;
}

/* Return a Lisp integer equal to OP, which has BITS bits and which
   must not be in fixnum range.  */
static Lisp_Object
make_bignum_bits (mpz_t const op, size_t bits)
{
  /* The documentation says integer-width should be nonnegative, so
     a single comparison suffices even though 'bits' is unsigned.  */
  if (integer_width < bits)
    range_error ();

  struct Lisp_Bignum *b = ALLOCATE_PSEUDOVECTOR (struct Lisp_Bignum, value,
						 PVEC_BIGNUM);
  /* We could mpz_init + mpz_swap here, to avoid a copy, but the
     resulting API seemed possibly confusing.  */
  mpz_init_set (b->value, op);

  return make_lisp_ptr (b, Lisp_Vectorlike);
}

/* Return a Lisp integer equal to OP, which must not be in fixnum range.  */
static Lisp_Object
make_bignum (mpz_t const op)
{
  return make_bignum_bits (op, mpz_sizeinbase (op, 2));
}

/* Return a Lisp integer equal to N, which must not be in fixnum range.  */
Lisp_Object
make_bigint (intmax_t n)
{
  eassert (FIXNUM_OVERFLOW_P (n));
  mpz_t z;
  mpz_init (z);
  mpz_set_intmax (z, n);
  Lisp_Object result = make_bignum (z);
  mpz_clear (z);
  return result;
}

/* Return a Lisp integer with value taken from OP.  */
Lisp_Object
make_integer (mpz_t const op)
{
  size_t bits = mpz_sizeinbase (op, 2);

  if (bits <= FIXNUM_BITS)
    {
      EMACS_INT v = 0;
      int i = 0, shift = 0;

      do
	{
	  EMACS_INT limb = mpz_getlimbn (op, i++);
	  v += limb << shift;
	  shift += GMP_NUMB_BITS;
	}
      while (shift < bits);

      if (mpz_sgn (op) < 0)
	v = -v;

      if (!FIXNUM_OVERFLOW_P (v))
	return make_fixnum (v);
    }

  return make_bignum_bits (op, bits);
}

void
mpz_set_intmax_slow (mpz_t result, intmax_t v)
{
  bool complement = v < 0;
  if (complement)
    v = -1 - v;

  enum { nails = sizeof v * CHAR_BIT - INTMAX_WIDTH };
# ifndef HAVE_GMP
  /* mini-gmp requires NAILS to be zero, which is true for all
     likely Emacs platforms.  Sanity-check this.  */
  verify (nails == 0);
# endif

  mpz_import (result, 1, -1, sizeof v, 0, nails, &v);
  if (complement)
    mpz_com (result, result);
}

/* Convert NUM to a base-BASE Lisp string.  */

Lisp_Object
bignum_to_string (Lisp_Object num, int base)
{
  ptrdiff_t n = mpz_sizeinbase (XBIGNUM (num)->value, base) - 1;
  USE_SAFE_ALLOCA;
  char *str = SAFE_ALLOCA (n + 3);
  mpz_get_str (str, base, XBIGNUM (num)->value);
  while (str[n])
    n++;
  Lisp_Object result = make_unibyte_string (str, n);
  SAFE_FREE ();
  return result;
}

/* Create a bignum by scanning NUM, with digits in BASE.
   NUM must consist of an optional '-', a nonempty sequence
   of base-BASE digits, and a terminating null byte, and
   the represented number must not be in fixnum range.  */

Lisp_Object
make_bignum_str (char const *num, int base)
{
  struct Lisp_Bignum *b = ALLOCATE_PSEUDOVECTOR (struct Lisp_Bignum, value,
						 PVEC_BIGNUM);
  mpz_init (b->value);
  int check = mpz_set_str (b->value, num, base);
  eassert (check == 0);
  return make_lisp_ptr (b, Lisp_Vectorlike);
}
