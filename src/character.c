/* Basic character support.

Copyright (C) 2001-2017 Free Software Foundation, Inc.
Copyright (C) 1995, 1997, 1998, 2001 Electrotechnical Laboratory, JAPAN.
  Licensed to the Free Software Foundation.
Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H13PRO009

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* At first, see the document in `character.h' to understand the code
   in this file.  */

#include <config.h>

#include <stdio.h>

#include <sys/types.h>
#include <intprops.h>
#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "composite.h"
#include "disptab.h"

/* Char-table of information about which character to unify to which
   Unicode character.  Mainly used by the macro MAYBE_UNIFY_CHAR.  */
Lisp_Object Vchar_unify_table;



/* Translate character C by translation table TABLE.  If no translation is
   found in TABLE, return the untranslated character.  If TABLE is a list,
   elements are char tables.  In that case, recursively translate C by all the
   tables in the list.  */

int
translate_char (Lisp_Object table, int c)
{
  if (CHAR_TABLE_P (table))
    {
      Lisp_Object ch;

      ch = CHAR_TABLE_REF (table, c);
      if (CHARACTERP (ch))
	c = XINT (ch);
    }
  else
    {
      for (; CONSP (table); table = XCDR (table))
	c = translate_char (XCAR (table), c);
    }
  return c;
}


DEFUN ("unibyte-char-to-multibyte", Funibyte_char_to_multibyte,
       Sunibyte_char_to_multibyte, 1, 1, 0,
       doc: /* Convert the byte CH to multibyte character.  */)
  (Lisp_Object ch)
{
  int c;

  CHECK_CHARACTER (ch);
  c = XFASTINT (ch);
  if (c >= 0x100)
    error ("Not a unibyte character: %d", c);
  MAKE_CHAR_MULTIBYTE (c);
  return make_number (c);
}

DEFUN ("multibyte-char-to-unibyte", Fmultibyte_char_to_unibyte,
       Smultibyte_char_to_unibyte, 1, 1, 0,
       doc: /* Convert the multibyte character CH to a byte.
If the multibyte character does not represent a byte, return -1.  */)
  (Lisp_Object ch)
{
  int cm;

  CHECK_CHARACTER (ch);
  cm = XFASTINT (ch);
  if (cm < 256)
    /* Can't distinguish a byte read from a unibyte buffer from
       a latin1 char, so let's let it slide.  */
    return ch;
  else
    {
      int cu = CHAR_TO_BYTE_SAFE (cm);
      return make_number (cu);
    }
}


/* Return width (columns) of C considering the buffer display table DP. */

static ptrdiff_t
char_width (int c, struct Lisp_Char_Table *dp)
{
  ptrdiff_t width = CHARACTER_WIDTH (c);

  if (dp)
    {
      Lisp_Object disp = DISP_CHAR_VECTOR (dp, c), ch;
      int i;

      if (VECTORP (disp))
	for (i = 0, width = 0; i < ASIZE (disp); i++)
	  {
	    ch = AREF (disp, i);
	    if (CHARACTERP (ch))
	      {
		int w = CHARACTER_WIDTH (XFASTINT (ch));
		if (INT_ADD_WRAPV (width, w, &width))
		  string_overflow ();
	      }
	  }
    }
  return width;
}


DEFUN ("char-width", Fchar_width, Schar_width, 1, 1, 0,
       doc: /* Return width of CHAR when displayed in the current buffer.
The width is measured by how many columns it occupies on the screen.
Tab is taken to occupy `tab-width' columns.
usage: (char-width CHAR)  */)
  (Lisp_Object ch)
{
  int c;
  ptrdiff_t width;

  CHECK_CHARACTER (ch);
  c = XINT (ch);
  width = char_width (c, buffer_display_table ());
  return make_number (width);
}

/* Return width of string STR of length LEN when displayed in the
   current buffer.  The width is measured by how many columns it
   occupies on the screen.  If PRECISION > 0, return the width of
   longest substring that doesn't exceed PRECISION, and set number of
   characters and bytes of the substring in *NCHARS and *NBYTES
   respectively.  */

ptrdiff_t
c_string_width (const unsigned char *str, ptrdiff_t len, int precision,
		ptrdiff_t *nchars, ptrdiff_t *nbytes)
{
  ptrdiff_t i = 0, i_byte = 0;
  ptrdiff_t width = 0;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (i_byte < len)
    {
      int bytes;
      int c = STRING_CHAR_AND_LENGTH (str + i_byte, bytes);
      ptrdiff_t thiswidth = char_width (c, dp);

      if (0 < precision && precision - width < thiswidth)
	{
	  *nchars = i;
	  *nbytes = i_byte;
	  return width;
	}
      if (INT_ADD_WRAPV (thiswidth, width, &width))
	string_overflow ();
      i++;
      i_byte += bytes;
  }

  if (precision > 0)
    {
      *nchars = i;
      *nbytes = i_byte;
    }

  return width;
}

/* Return width of string STR of length LEN when displayed in the
   current buffer.  The width is measured by how many columns it
   occupies on the screen.  */

ptrdiff_t
strwidth (const char *str, ptrdiff_t len)
{
  return c_string_width ((const unsigned char *) str, len, -1, NULL, NULL);
}

/* Return width of Lisp string STRING when displayed in the current
   buffer.  The width is measured by how many columns it occupies on
   the screen while paying attention to compositions.  If PRECISION >
   0, return the width of longest substring that doesn't exceed
   PRECISION, and set number of characters and bytes of the substring
   in *NCHARS and *NBYTES respectively.  */

ptrdiff_t
lisp_string_width (Lisp_Object string, ptrdiff_t precision,
		   ptrdiff_t *nchars, ptrdiff_t *nbytes)
{
  ptrdiff_t len = SCHARS (string);
  /* This set multibyte to 0 even if STRING is multibyte when it
     contains only ascii and eight-bit-graphic, but that's
     intentional.  */
  bool multibyte = len < SBYTES (string);
  unsigned char *str = SDATA (string);
  ptrdiff_t i = 0, i_byte = 0;
  ptrdiff_t width = 0;
  struct Lisp_Char_Table *dp = buffer_display_table ();

  while (i < len)
    {
      ptrdiff_t chars, bytes, thiswidth;
      Lisp_Object val;
      ptrdiff_t cmp_id;
      ptrdiff_t ignore, end;

      if (find_composition (i, -1, &ignore, &end, &val, string)
	  && ((cmp_id = get_composition_id (i, i_byte, end - i, val, string))
	      >= 0))
	{
	  thiswidth = composition_table[cmp_id]->width;
	  chars = end - i;
	  bytes = string_char_to_byte (string, end) - i_byte;
	}
      else
	{
	  int c;

	  if (multibyte)
	    {
	      int cbytes;
	      c = STRING_CHAR_AND_LENGTH (str + i_byte, cbytes);
	      bytes = cbytes;
	    }
	  else
	    c = str[i_byte], bytes = 1;
	  chars = 1;
	  thiswidth = char_width (c, dp);
	}

      if (0 < precision && precision - width < thiswidth)
	{
	  *nchars = i;
	  *nbytes = i_byte;
	  return width;
	}
      if (INT_ADD_WRAPV (thiswidth, width, &width))
	string_overflow ();
      i += chars;
      i_byte += bytes;
    }

  if (precision > 0)
    {
      *nchars = i;
      *nbytes = i_byte;
    }

  return width;
}

DEFUN ("string-width", Fstring_width, Sstring_width, 1, 1, 0,
       doc: /* Return width of STRING when displayed in the current buffer.
Width is measured by how many columns it occupies on the screen.
When calculating width of a multibyte character in STRING,
only the base leading-code is considered; the validity of
the following bytes is not checked.  Tabs in STRING are always
taken to occupy `tab-width' columns.
usage: (string-width STRING)  */)
  (Lisp_Object str)
{
  Lisp_Object val;

  CHECK_STRING (str);
  XSETFASTINT (val, lisp_string_width (str, -1, NULL, NULL));
  return val;
}

/* Return the number of characters in the NBYTES bytes at PTR.
   This works by looking at the contents and checking for multibyte
   sequences while assuming that there's no invalid sequence.
   However, if the current buffer has enable-multibyte-characters =
   nil, we treat each byte as a character.  */

ptrdiff_t
chars_in_text (const unsigned char *ptr, ptrdiff_t nbytes)
{
  /* current_buffer is null at early stages of Emacs initialization.  */
  if (current_buffer == 0
      || NILP (BVAR (current_buffer, enable_multibyte_characters)))
    return nbytes;

  return multibyte_chars_in_text (ptr, nbytes);
}

static ptrdiff_t
string_count_byte8 (Lisp_Object string)
{
  bool multibyte = STRING_MULTIBYTE (string);
  ptrdiff_t nbytes = SBYTES (string);
  unsigned char *p = SDATA (string);
  unsigned char *pend = p + nbytes;
  ptrdiff_t count = 0;
  int c, len;

  if (multibyte)
    while (p < pend)
      {
	c = *p;
	len = BYTES_BY_CHAR_HEAD (c);

	if (CHAR_BYTE8_HEAD_P (c))
	  count++;
	p += len;
      }
  else
    while (p < pend)
      {
	if (*p++ >= 0x80)
	  count++;
      }
  return count;
}


Lisp_Object
string_escape_byte8 (Lisp_Object string)
{
  ptrdiff_t nchars = SCHARS (string);
  ptrdiff_t nbytes = SBYTES (string);
  bool multibyte = STRING_MULTIBYTE (string);
  ptrdiff_t byte8_count;
  ptrdiff_t thrice_byte8_count, uninit_nchars, uninit_nbytes;
  const unsigned char *src, *src_end;
  unsigned char *dst;
  Lisp_Object val;
  int c, len;

  if (multibyte && nchars == nbytes)
    return string;

  byte8_count = string_count_byte8 (string);

  if (byte8_count == 0)
    return string;

  if (INT_MULTIPLY_WRAPV (byte8_count, 3, &thrice_byte8_count))
    string_overflow ();

  if (multibyte)
    {
      /* Convert 2-byte sequence of byte8 chars to 4-byte octal.  */
      if (INT_ADD_WRAPV (nchars, thrice_byte8_count, &uninit_nchars)
	  || INT_ADD_WRAPV (nbytes, 2 * byte8_count, &uninit_nbytes))
	string_overflow ();
      val = make_uninit_multibyte_string (uninit_nchars, uninit_nbytes);
    }
  else
    {
      /* Convert 1-byte sequence of byte8 chars to 4-byte octal.  */
      if (INT_ADD_WRAPV (thrice_byte8_count, nbytes, &uninit_nbytes))
	string_overflow ();
      val = make_uninit_string (uninit_nbytes);
    }

  src = SDATA (string);
  src_end = src + nbytes;
  dst = SDATA (val);
  if (multibyte)
    while (src < src_end)
      {
	c = *src;
	len = BYTES_BY_CHAR_HEAD (c);

	if (CHAR_BYTE8_HEAD_P (c))
	  {
	    c = STRING_CHAR_ADVANCE (src);
	    c = CHAR_TO_BYTE8 (c);
	    dst += sprintf ((char *) dst, "\\%03o", c + 0u);
	  }
	else
	  while (len--) *dst++ = *src++;
      }
  else
    while (src < src_end)
      {
	c = *src++;
	if (c >= 0x80)
	  dst += sprintf ((char *) dst, "\\%03o", c + 0u);
	else
	  *dst++ = c;
      }
  return val;
}


DEFUN ("string", Fstring, Sstring, 0, MANY, 0,
       doc: /*
Concatenate all the argument characters and make the result a string.
usage: (string &rest CHARACTERS)  */)
  (ptrdiff_t n, Lisp_Object *args)
{
  ptrdiff_t i;
  int c;
  unsigned char *buf, *p;
  Lisp_Object str;
  USE_SAFE_ALLOCA;

  SAFE_NALLOCA (buf, MAX_MULTIBYTE_LENGTH, n);
  p = buf;

  for (i = 0; i < n; i++)
    {
      CHECK_CHARACTER (args[i]);
      c = XINT (args[i]);
      p += CHAR_STRING (c, p);
    }

  str = make_string_from_bytes ((char *) buf, n, p - buf);
  SAFE_FREE ();
  return str;
}

DEFUN ("unibyte-string", Funibyte_string, Sunibyte_string, 0, MANY, 0,
       doc: /* Concatenate all the argument bytes and make the result a unibyte string.
usage: (unibyte-string &rest BYTES)  */)
  (ptrdiff_t n, Lisp_Object *args)
{
  ptrdiff_t i;
  Lisp_Object str;
  USE_SAFE_ALLOCA;
  unsigned char *buf = SAFE_ALLOCA (n);
  unsigned char *p = buf;

  for (i = 0; i < n; i++)
    {
      CHECK_RANGED_INTEGER (args[i], 0, 255);
      *p++ = XINT (args[i]);
    }

  str = make_string_from_bytes ((char *) buf, n, p - buf);
  SAFE_FREE ();
  return str;
}

DEFUN ("char-resolve-modifiers", Fchar_resolve_modifiers,
       Schar_resolve_modifiers, 1, 1, 0,
       doc: /* Resolve modifiers in the character CHAR.
The value is a character with modifiers resolved into the character
code.  Unresolved modifiers are kept in the value.
usage: (char-resolve-modifiers CHAR)  */)
  (Lisp_Object character)
{
  EMACS_INT c;

  CHECK_NUMBER (character);
  c = XINT (character);
  return make_number (char_resolve_modifier_mask (c));
}

DEFUN ("get-byte", Fget_byte, Sget_byte, 0, 2, 0,
       doc: /* Return a byte value of a character at point.
Optional 1st arg POSITION, if non-nil, is a position of a character to get
a byte value.
Optional 2nd arg STRING, if non-nil, is a string of which first
character is a target to get a byte value.  In this case, POSITION, if
non-nil, is an index of a target character in the string.

If the current buffer (or STRING) is multibyte, and the target
character is not ASCII nor 8-bit character, an error is signaled.  */)
  (Lisp_Object position, Lisp_Object string)
{
  int c;
  ptrdiff_t pos;
  unsigned char *p;

  if (NILP (string))
    {
      if (NILP (position))
	{
	  p = PT_ADDR;
	}
      else
	{
	  CHECK_NUMBER_COERCE_MARKER (position);
	  if (XINT (position) < BEGV || XINT (position) >= ZV)
	    args_out_of_range_3 (position, make_number (BEGV), make_number (ZV));
	  pos = XFASTINT (position);
	  p = CHAR_POS_ADDR (pos);
	}
      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	return make_number (*p);
    }
  else
    {
      CHECK_STRING (string);
      if (NILP (position))
	{
	  p = SDATA (string);
	}
      else
	{
	  CHECK_NATNUM (position);
	  if (XINT (position) >= SCHARS (string))
	    args_out_of_range (string, position);
	  pos = XFASTINT (position);
	  p = SDATA (string) + string_char_to_byte (string, pos);
	}
      if (! STRING_MULTIBYTE (string))
	return make_number (*p);
    }
  c = STRING_CHAR (p);
  if (CHAR_BYTE8_P (c))
    c = CHAR_TO_BYTE8 (c);
  else if (! ASCII_CHAR_P (c))
    error ("Not an ASCII nor an 8-bit character: %d", c);
  return make_number (c);
}

/* Return true if C is an alphabetic character.  */
bool
alphabeticp (int c)
{
  Lisp_Object category = CHAR_TABLE_REF (Vunicode_category_table, c);
  if (! INTEGERP (category))
    return false;
  EMACS_INT gen_cat = XINT (category);

  /* See UTS #18.  There are additional characters that should be
     here, those designated as Other_uppercase, Other_lowercase,
     and Other_alphabetic; FIXME.  */
  return (gen_cat == UNICODE_CATEGORY_Lu
	  || gen_cat == UNICODE_CATEGORY_Ll
	  || gen_cat == UNICODE_CATEGORY_Lt
	  || gen_cat == UNICODE_CATEGORY_Lm
	  || gen_cat == UNICODE_CATEGORY_Lo
	  || gen_cat == UNICODE_CATEGORY_Mn
	  || gen_cat == UNICODE_CATEGORY_Mc
	  || gen_cat == UNICODE_CATEGORY_Me
	  || gen_cat == UNICODE_CATEGORY_Nl);
}

/* Return true if C is a alphabetic or decimal-number character.  */
bool
alphanumericp (int c)
{
  Lisp_Object category = CHAR_TABLE_REF (Vunicode_category_table, c);
  if (! INTEGERP (category))
    return false;
  EMACS_INT gen_cat = XINT (category);

  /* See UTS #18.  Same comment as for alphabeticp applies.  FIXME. */
  return (gen_cat == UNICODE_CATEGORY_Lu
	  || gen_cat == UNICODE_CATEGORY_Ll
	  || gen_cat == UNICODE_CATEGORY_Lt
	  || gen_cat == UNICODE_CATEGORY_Lm
	  || gen_cat == UNICODE_CATEGORY_Lo
	  || gen_cat == UNICODE_CATEGORY_Mn
	  || gen_cat == UNICODE_CATEGORY_Mc
	  || gen_cat == UNICODE_CATEGORY_Me
	  || gen_cat == UNICODE_CATEGORY_Nl
	  || gen_cat == UNICODE_CATEGORY_Nd);
}

/* Return true if C is a graphic character.  */
bool
graphicp (int c)
{
  Lisp_Object category = CHAR_TABLE_REF (Vunicode_category_table, c);
  if (! INTEGERP (category))
    return false;
  EMACS_INT gen_cat = XINT (category);

  /* See UTS #18.  */
  return (!(gen_cat == UNICODE_CATEGORY_Zs /* space separator */
	    || gen_cat == UNICODE_CATEGORY_Zl /* line separator */
	    || gen_cat == UNICODE_CATEGORY_Zp /* paragraph separator */
	    || gen_cat == UNICODE_CATEGORY_Cc /* control */
	    || gen_cat == UNICODE_CATEGORY_Cs /* surrogate */
	    || gen_cat == UNICODE_CATEGORY_Cn)); /* unassigned */
}

/* Return true if C is a printable character.  */
bool
printablep (int c)
{
  Lisp_Object category = CHAR_TABLE_REF (Vunicode_category_table, c);
  if (! INTEGERP (category))
    return false;
  EMACS_INT gen_cat = XINT (category);

  /* See UTS #18.  */
  return (!(gen_cat == UNICODE_CATEGORY_Cc /* control */
	    || gen_cat == UNICODE_CATEGORY_Cs /* surrogate */
	    || gen_cat == UNICODE_CATEGORY_Cn)); /* unassigned */
}

/* Return true if C is a horizontal whitespace character, as defined
   by http://www.unicode.org/reports/tr18/tr18-19.html#blank.  */
bool
blankp (int c)
{
  Lisp_Object category = CHAR_TABLE_REF (Vunicode_category_table, c);
  if (! INTEGERP (category))
    return false;

  return XINT (category) == UNICODE_CATEGORY_Zs; /* separator, space */
}

void
syms_of_character (void)
{
  DEFSYM (Qcharacterp, "characterp");
  DEFSYM (Qauto_fill_chars, "auto-fill-chars");

  staticpro (&Vchar_unify_table);
  Vchar_unify_table = Qnil;

  defsubr (&Sunibyte_char_to_multibyte);
  defsubr (&Smultibyte_char_to_unibyte);
  defsubr (&Schar_width);
  defsubr (&Sstring_width);
  defsubr (&Sstring);
  defsubr (&Sunibyte_string);
  defsubr (&Schar_resolve_modifiers);
  defsubr (&Sget_byte);

  DEFVAR_LISP ("translation-table-vector",  Vtranslation_table_vector,
	       doc: /*
Vector recording all translation tables ever defined.
Each element is a pair (SYMBOL . TABLE) relating the table to the
symbol naming it.  The ID of a translation table is an index into this vector.  */);
  Vtranslation_table_vector = Fmake_vector (make_number (16), Qnil);

  DEFVAR_LISP ("auto-fill-chars", Vauto_fill_chars,
	       doc: /*
A char-table for characters which invoke auto-filling.
Such characters have value t in this table.  */);
  Vauto_fill_chars = Fmake_char_table (Qauto_fill_chars, Qnil);
  CHAR_TABLE_SET (Vauto_fill_chars, ' ', Qt);
  CHAR_TABLE_SET (Vauto_fill_chars, '\n', Qt);

  DEFVAR_LISP ("char-width-table", Vchar_width_table,
	       doc: /*
A char-table for width (columns) of each character.  */);
  Vchar_width_table = Fmake_char_table (Qnil, make_number (1));
  char_table_set_range (Vchar_width_table, 0x80, 0x9F, make_number (4));
  char_table_set_range (Vchar_width_table, MAX_5_BYTE_CHAR + 1, MAX_CHAR,
			make_number (4));

  DEFVAR_LISP ("printable-chars", Vprintable_chars,
	       doc: /* A char-table for each printable character.  */);
  Vprintable_chars = Fmake_char_table (Qnil, Qnil);
  Fset_char_table_range (Vprintable_chars,
			 Fcons (make_number (32), make_number (126)), Qt);
  Fset_char_table_range (Vprintable_chars,
			 Fcons (make_number (160),
				make_number (MAX_5_BYTE_CHAR)), Qt);

  DEFVAR_LISP ("char-script-table", Vchar_script_table,
	       doc: /* Char table of script symbols.
It has one extra slot whose value is a list of script symbols.  */);

  DEFSYM (Qchar_script_table, "char-script-table");
  Fput (Qchar_script_table, Qchar_table_extra_slots, make_number (1));
  Vchar_script_table = Fmake_char_table (Qchar_script_table, Qnil);

  DEFVAR_LISP ("script-representative-chars", Vscript_representative_chars,
	       doc: /* Alist of scripts vs the representative characters.
Each element is a cons (SCRIPT . CHARS).
SCRIPT is a symbol representing a script or a subgroup of a script.
CHARS is a list or a vector of characters.
If it is a list, all characters in the list are necessary for supporting SCRIPT.
If it is a vector, one of the characters in the vector is necessary.
This variable is used to find a font for a specific script.  */);
  Vscript_representative_chars = Qnil;

  DEFVAR_LISP ("unicode-category-table", Vunicode_category_table,
	       doc: /* Char table of Unicode's "General Category".
All Unicode characters have one of the following values (symbol):
  Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pc, Pd, Ps, Pe, Pi, Pf, Po,
  Sm, Sc, Sk, So, Zs, Zl, Zp, Cc, Cf, Cs, Co, Cn
See The Unicode Standard for the meaning of those values.  */);
  /* The correct char-table is setup in characters.el.  */
  Vunicode_category_table = Qnil;
}
