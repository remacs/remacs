/* GNU Emacs case conversion functions.

Copyright (C) 1985, 1994, 1997-1999, 2001-2017 Free Software Foundation,
Inc.

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


#include <config.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "commands.h"
#include "syntax.h"
#include "composite.h"
#include "keymap.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

/* State for casing individual characters.  */
struct casing_context {
  /* A char-table with title-case character mappings or nil.  Non-nil implies
     flag is CASE_CAPITALIZE or CASE_CAPITALIZE_UP.  */
  Lisp_Object titlecase_char_table;
  /* User-requested action. */
  enum case_action flag;
  /* If true, function operates on a buffer as opposed to a string or character.
     When run on a buffer, syntax_prefix_flag_p is taken into account when
     determined inword flag. */
  bool inbuffer;
  /* Conceptually, this denotes whether we are inside of a word except
     that if flag is CASE_UP it’s always false and if flag is CASE_DOWN
     this is always true. */
  bool inword;
};

/* Initialise CTX structure for casing characters. */
static void
prepare_casing_context (struct casing_context *ctx,
			enum case_action flag, bool inbuffer)
{
  ctx->flag = flag;
  ctx->inbuffer = inbuffer;
  ctx->inword = flag == CASE_DOWN;
  ctx->titlecase_char_table = (int)flag < (int)CASE_CAPITALIZE ? Qnil :
    uniprop_table (intern_c_string ("titlecase"));

  /* If the case table is flagged as modified, rescan it.  */
  if (NILP (XCHAR_TABLE (BVAR (current_buffer, downcase_table))->extras[1]))
    Fset_case_table (BVAR (current_buffer, downcase_table));

  if (inbuffer && (int) flag >= (int) CASE_CAPITALIZE)
    SETUP_BUFFER_SYNTAX_TABLE ();	/* For syntax_prefix_flag_p.  */
}

/* Based on CTX, case character CH accordingly.  Update CTX as necessary.
   Return cased character. */
static int
case_character (struct casing_context *ctx, int ch)
{
  Lisp_Object prop;

  if (ctx->inword)
    ch = ctx->flag == CASE_CAPITALIZE_UP ? ch : downcase (ch);
  else if (!NILP (ctx->titlecase_char_table) &&
	   CHARACTERP (prop = CHAR_TABLE_REF (ctx->titlecase_char_table, ch)))
    ch = XFASTINT (prop);
  else
    ch = upcase(ch);

  if ((int) ctx->flag >= (int) CASE_CAPITALIZE)
    ctx->inword = SYNTAX (ch) == Sword &&
      (!ctx->inbuffer || ctx->inword || !syntax_prefix_flag_p (ch));
  return ch;
}

static Lisp_Object
do_casify_natnum (struct casing_context *ctx, Lisp_Object obj)
{
  int flagbits = (CHAR_ALT | CHAR_SUPER | CHAR_HYPER
		  | CHAR_SHIFT | CHAR_CTL | CHAR_META);
  int flags, ch = XFASTINT (obj), cased;
  bool multibyte;

  /* If the character has higher bits set above the flags, return it unchanged.
     It is not a real character.  */
  if (UNSIGNED_CMP (ch, >, flagbits))
    return obj;

  flags = ch & flagbits;
  ch = ch & ~flagbits;

  /* FIXME: Even if enable-multibyte-characters is nil, we may manipulate
     multibyte chars.  This means we have a bug for latin-1 chars since when we
     receive an int 128-255 we can't tell whether it's an eight-bit byte or
     a latin-1 char.  */
  multibyte = ch >= 256
    || !NILP (BVAR (current_buffer, enable_multibyte_characters));
  if (! multibyte)
    MAKE_CHAR_MULTIBYTE (ch);
  cased = case_character (ctx, ch);
  if (cased == ch)
    return obj;

  if (! multibyte)
    MAKE_CHAR_UNIBYTE (cased);
  XSETFASTINT (obj, cased | flags);
  return obj;
}

static Lisp_Object
do_casify_multibyte_string (struct casing_context *ctx, Lisp_Object obj)
{
  ptrdiff_t i, i_byte, size = SCHARS (obj);
  int len, ch, cased;
  USE_SAFE_ALLOCA;
  ptrdiff_t o_size;
  if (INT_MULTIPLY_WRAPV (size, MAX_MULTIBYTE_LENGTH, &o_size))
    o_size = PTRDIFF_MAX;
  unsigned char *dst = SAFE_ALLOCA (o_size);
  unsigned char *o = dst;

  for (i = i_byte = 0; i < size; i++, i_byte += len)
    {
      if (o_size - MAX_MULTIBYTE_LENGTH < o - dst)
	string_overflow ();
      ch = STRING_CHAR_AND_LENGTH (SDATA (obj) + i_byte, len);
      cased = case_character (ctx, ch);
      o += CHAR_STRING (cased, o);
    }
  eassert (o - dst <= o_size);
  obj = make_multibyte_string ((char *) dst, size, o - dst);
  SAFE_FREE ();
  return obj;
}

static Lisp_Object
do_casify_unibyte_string (struct casing_context *ctx, Lisp_Object obj)
{
  ptrdiff_t i, size = SCHARS (obj);
  int ch, cased;

  obj = Fcopy_sequence (obj);
  for (i = 0; i < size; i++)
    {
      ch = SREF (obj, i);
      MAKE_CHAR_MULTIBYTE (ch);
      cased = case_character (ctx, ch);
      if (ch == cased)
	continue;
      MAKE_CHAR_UNIBYTE (cased);
      /* If the char can't be converted to a valid byte, just don't change it */
      if (cased >= 0 && cased < 256)
	SSET (obj, i, cased);
    }
  return obj;
}

static Lisp_Object
casify_object (enum case_action flag, Lisp_Object obj)
{
  struct casing_context ctx;
  prepare_casing_context (&ctx, flag, false);

  if (NATNUMP (obj))
    return do_casify_natnum (&ctx, obj);
  else if (!STRINGP (obj))
    wrong_type_argument (Qchar_or_string_p, obj);
  else if (!SCHARS (obj))
    return obj;
  else if (STRING_MULTIBYTE (obj))
    return do_casify_multibyte_string (&ctx, obj);
  else
    return do_casify_unibyte_string (&ctx, obj);
}

DEFUN ("upcase", Fupcase, Supcase, 1, 1, 0,
       doc: /* Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
See also `capitalize', `downcase' and `upcase-initials'.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_UP, obj);
}

DEFUN ("downcase", Fdowncase, Sdowncase, 1, 1, 0,
       doc: /* Convert argument to lower case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_DOWN, obj);
}

DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 1, 0,
       doc: /* Convert argument to capitalized form and return that.
This means that each word's first character is converted to either
title case or upper case, and the rest to lower case.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_CAPITALIZE, obj);
}

/* Like Fcapitalize but change only the initials.  */

DEFUN ("upcase-initials", Fupcase_initials, Supcase_initials, 1, 1, 0,
       doc: /* Convert the initial of each word in the argument to upper case.
This means that each word's first character is converted to either
title case or upper case, and the rest are left unchanged.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.  */)
  (Lisp_Object obj)
{
  return casify_object (CASE_CAPITALIZE_UP, obj);
}

/* Based on CTX, case region in a unibyte buffer from POS to *ENDP.  Return
   first position that has changed and save last position in *ENDP.  If no
   characters were changed, return -1 and *ENDP is unspecified. */
static ptrdiff_t
do_casify_unibyte_region (struct casing_context *ctx,
			  ptrdiff_t pos, ptrdiff_t *endp)
{
  ptrdiff_t first = -1, last = -1;  /* Position of first and last changes. */
  ptrdiff_t end = *endp;
  int ch, cased;

  for (; pos < end; ++pos)
    {
      ch = FETCH_BYTE (pos);
      MAKE_CHAR_MULTIBYTE (ch);

      cased = case_character (ctx, ch);
      if (cased == ch)
	continue;

      last = pos;
      if (first < 0)
	first = pos;

      MAKE_CHAR_UNIBYTE (cased);
      FETCH_BYTE (pos) = cased;
    }

  *endp = last + 1;
  return first;
}

/* Based on CTX, case region in a multibyte buffer from POS to *ENDP.  Return
   first position that has changed and save last position in *ENDP.  If no
   characters were changed, return -1 and *ENDP is unspecified. */
static ptrdiff_t
do_casify_multibyte_region (struct casing_context *ctx,
                           ptrdiff_t pos, ptrdiff_t *endp)
{
  ptrdiff_t first = -1, last = -1;  /* Position of first and last changes. */
  ptrdiff_t pos_byte = CHAR_TO_BYTE (pos), end = *endp;
  ptrdiff_t opoint = PT;
  int ch, cased, len;

  while (pos < end)
    {
      ch = STRING_CHAR_AND_LENGTH (BYTE_POS_ADDR (pos_byte), len);
      cased = case_character (ctx, ch);
      if (cased != ch)
	{
	  last = pos;
	  if (first < 0)
	    first = pos;

	  if (ASCII_CHAR_P (cased) && ASCII_CHAR_P (ch))
	    FETCH_BYTE (pos_byte) = cased;
	  else
	    {
	      unsigned char str[MAX_MULTIBYTE_LENGTH];
	      int totlen = CHAR_STRING (cased, str);
	      if (len == totlen)
		memcpy (BYTE_POS_ADDR (pos_byte), str, len);
	      else
		/* Replace one character with the other(s), keeping text
		   properties the same.  */
		replace_range_2 (pos, pos_byte, pos + 1, pos_byte + len,
				 (char *) str, 9, totlen, 0);
	      len = totlen;
	    }
	}
      pos++;
      pos_byte += len;
    }

  if (PT != opoint)
    TEMP_SET_PT_BOTH (opoint, CHAR_TO_BYTE (opoint));

  *endp = last;
  return first;
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   b and e specify range of buffer to operate on. */
static void
casify_region (enum case_action flag, Lisp_Object b, Lisp_Object e)
{
  struct casing_context ctx;
  ptrdiff_t start, end;

  if (EQ (b, e))
    /* Not modifying because nothing marked */
    return;

  validate_region (&b, &e);
  start = XFASTINT (b);
  end = XFASTINT (e);
  modify_text (start, end);
  record_change (start, end - start);
  prepare_casing_context (&ctx, flag, true);

  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    start = do_casify_unibyte_region (&ctx, start, &end);
  else
    start = do_casify_multibyte_region (&ctx, start, &end);

  if (start >= 0)
    {
      signal_after_change (start, end + 1 - start, end + 1 - start);
      update_compositions (start, end + 1, CHECK_ALL);
    }
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 3,
       "(list (region-beginning) (region-end) (region-noncontiguous-p))",
       doc: /* Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
See also `capitalize-region'.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object region_noncontiguous_p)
{
  Lisp_Object bounds = Qnil;

  if (!NILP (region_noncontiguous_p))
    {
      bounds = call1 (Fsymbol_value (intern ("region-extract-function")),
		      intern ("bounds"));

      while (CONSP (bounds))
	{
	  casify_region (CASE_UP, XCAR (XCAR (bounds)), XCDR (XCAR (bounds)));
	  bounds = XCDR (bounds);
	}
    }
  else
    casify_region (CASE_UP, beg, end);

  return Qnil;
}

DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 3,
       "(list (region-beginning) (region-end) (region-noncontiguous-p))",
       doc: /* Convert the region to lower case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.  */)
  (Lisp_Object beg, Lisp_Object end, Lisp_Object region_noncontiguous_p)
{
  Lisp_Object bounds = Qnil;

  if (!NILP (region_noncontiguous_p))
    {
      bounds = call1 (Fsymbol_value (intern ("region-extract-function")),
		      intern ("bounds"));

      while (CONSP (bounds))
	{
	  casify_region (CASE_DOWN, XCAR (XCAR (bounds)), XCDR (XCAR (bounds)));
	  bounds = XCDR (bounds);
	}
    }
  else
    casify_region (CASE_DOWN, beg, end);

  return Qnil;
}

DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 2, "r",
       doc: /* Convert the region to capitalized form.
This means that each word's first character is converted to either
title case or upper case, and the rest to lower case.
In programs, give two arguments, the starting and ending
character positions to operate on.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  casify_region (CASE_CAPITALIZE, beg, end);
  return Qnil;
}

/* Like Fcapitalize_region but change only the initials.  */

DEFUN ("upcase-initials-region", Fupcase_initials_region,
       Supcase_initials_region, 2, 2, "r",
       doc: /* Upcase the initial of each word in the region.
This means that each word's first character is converted to either
title case or upper case, and the rest are left unchanged.
In programs, give two arguments, the starting and ending
character positions to operate on.  */)
  (Lisp_Object beg, Lisp_Object end)
{
  casify_region (CASE_CAPITALIZE_UP, beg, end);
  return Qnil;
}

static Lisp_Object
casify_word (enum case_action flag, Lisp_Object arg)
{
  CHECK_NUMBER (arg);
  ptrdiff_t farend = scan_words (PT, XINT (arg));
  if (!farend)
    farend = XINT (arg) <= 0 ? BEGV : ZV;
  ptrdiff_t newpoint = max (PT, farend);
  casify_region (flag, make_number (PT), make_number (farend));
  SET_PT (newpoint);
  return Qnil;
}

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 1, "p",
       doc: /* Convert to upper case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.
See also `capitalize-word'.  */)
  (Lisp_Object arg)
{
  return casify_word (CASE_UP, arg);
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 1, "p",
       doc: /* Convert to lower case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.  */)
  (Lisp_Object arg)
{
  return casify_word (CASE_DOWN, arg);
}

DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 1, "p",
       doc: /* Capitalize from point to the end of word, moving over.
With numerical argument ARG, capitalize the next ARG-1 words as well.
This gives the word(s) a first character in upper case
and the rest lower case.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, capitalize previous words but do not move.  */)
  (Lisp_Object arg)
{
  return casify_word (CASE_CAPITALIZE, arg);
}

void
syms_of_casefiddle (void)
{
  DEFSYM (Qidentity, "identity");
  defsubr (&Supcase);
  defsubr (&Sdowncase);
  defsubr (&Scapitalize);
  defsubr (&Supcase_initials);
  defsubr (&Supcase_region);
  defsubr (&Sdowncase_region);
  defsubr (&Scapitalize_region);
  defsubr (&Supcase_initials_region);
  defsubr (&Supcase_word);
  defsubr (&Sdowncase_word);
  defsubr (&Scapitalize_word);
}

void
keys_of_casefiddle (void)
{
  initial_define_key (control_x_map, Ctl ('U'), "upcase-region");
  Fput (intern ("upcase-region"), Qdisabled, Qt);
  initial_define_key (control_x_map, Ctl ('L'), "downcase-region");
  Fput (intern ("downcase-region"), Qdisabled, Qt);

  initial_define_key (meta_map, 'u', "upcase-word");
  initial_define_key (meta_map, 'l', "downcase-word");
  initial_define_key (meta_map, 'c', "capitalize-word");
}
