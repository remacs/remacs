/* Markers: examining, setting and deleting.
   Copyright (C) 1985, 1997-1998, 2001-2017 Free Software Foundation,
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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


#include <config.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"

/* Record one cached position found recently by
   buf_charpos_to_bytepos or buf_bytepos_to_charpos.  */

static ptrdiff_t cached_charpos;
static ptrdiff_t cached_bytepos;
static struct buffer *cached_buffer;
static EMACS_INT cached_modiff;

/* Juanma Barranquero <lekktu@gmail.com> reported ~3x increased
   bootstrap time when byte_char_debug_check is enabled; so this
   is never turned on by --enable-checking configure option.  */

#ifdef MARKER_DEBUG

extern int count_markers (struct buffer *) EXTERNALLY_VISIBLE;
extern ptrdiff_t verify_bytepos (ptrdiff_t charpos) EXTERNALLY_VISIBLE;

static void
byte_char_debug_check (struct buffer *b, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  ptrdiff_t nchars;

  if (NILP (BVAR (b, enable_multibyte_characters)))
    return;

  if (bytepos > BUF_GPT_BYTE (b))
    nchars
      = multibyte_chars_in_text (BUF_BEG_ADDR (b),
				 BUF_GPT_BYTE (b) - BUF_BEG_BYTE (b))
      + multibyte_chars_in_text (BUF_GAP_END_ADDR (b),
				 bytepos - BUF_GPT_BYTE (b));
  else
    nchars = multibyte_chars_in_text (BUF_BEG_ADDR (b),
				      bytepos - BUF_BEG_BYTE (b));

  if (charpos - 1 != nchars)
    emacs_abort ();
}

#else /* not MARKER_DEBUG */

#define byte_char_debug_check(b, charpos, bytepos) do { } while (0)

#endif /* MARKER_DEBUG */

void
clear_charpos_cache (struct buffer *b)
{
  if (cached_buffer == b)
    cached_buffer = 0;
}

/* Converting between character positions and byte positions.  */

/* There are several places in the buffer where we know
   the correspondence: BEG, BEGV, PT, GPT, ZV and Z,
   and everywhere there is a marker.  So we find the one of these places
   that is closest to the specified position, and scan from there.  */

/* This macro is a subroutine of buf_charpos_to_bytepos.
   Note that it is desirable that BYTEPOS is not evaluated
   except when we really want its value.  */

#define CONSIDER(CHARPOS, BYTEPOS)					\
{									\
  ptrdiff_t this_charpos = (CHARPOS);					\
  bool changed = 0;							\
									\
  if (this_charpos == charpos)						\
    {									\
      ptrdiff_t value = (BYTEPOS);				       	\
									\
      byte_char_debug_check (b, charpos, value);			\
      return value;							\
    }									\
  else if (this_charpos > charpos)					\
    {									\
      if (this_charpos < best_above)					\
	{								\
	  best_above = this_charpos;					\
	  best_above_byte = (BYTEPOS);					\
	  changed = 1;							\
	}								\
    }									\
  else if (this_charpos > best_below)					\
    {									\
      best_below = this_charpos;					\
      best_below_byte = (BYTEPOS);					\
      changed = 1;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
        {								\
	  ptrdiff_t value = best_below_byte + (charpos - best_below);	\
									\
	  byte_char_debug_check (b, charpos, value);			\
	  return value;							\
	}								\
    }									\
}

static void
CHECK_MARKER (Lisp_Object x)
{
  CHECK_TYPE (MARKERP (x), Qmarkerp, x);
}

/* Return the byte position corresponding to CHARPOS in B.  */

ptrdiff_t
buf_charpos_to_bytepos (struct buffer *b, ptrdiff_t charpos)
{
  struct Lisp_Marker *tail;
  ptrdiff_t best_above, best_above_byte;
  ptrdiff_t best_below, best_below_byte;

  eassert (BUF_BEG (b) <= charpos && charpos <= BUF_Z (b));

  best_above = BUF_Z (b);
  best_above_byte = BUF_Z_BYTE (b);

  /* If this buffer has as many characters as bytes,
     each character must be one byte.
     This takes care of the case where enable-multibyte-characters is nil.  */
  if (best_above == best_above_byte)
    return charpos;

  best_below = BEG;
  best_below_byte = BEG_BYTE;

  /* We find in best_above and best_above_byte
     the closest known point above CHARPOS,
     and in best_below and best_below_byte
     the closest known point below CHARPOS,

     If at any point we can tell that the space between those
     two best approximations is all single-byte,
     we interpolate the result immediately.  */

  CONSIDER (BUF_PT (b), BUF_PT_BYTE (b));
  CONSIDER (BUF_GPT (b), BUF_GPT_BYTE (b));
  CONSIDER (BUF_BEGV (b), BUF_BEGV_BYTE (b));
  CONSIDER (BUF_ZV (b), BUF_ZV_BYTE (b));

  if (b == cached_buffer && BUF_MODIFF (b) == cached_modiff)
    CONSIDER (cached_charpos, cached_bytepos);

  for (tail = BUF_MARKERS (b); tail; tail = tail->next)
    {
      CONSIDER (tail->charpos, tail->bytepos);

      /* If we are down to a range of 50 chars,
	 don't bother checking any other markers;
	 scan the intervening chars directly now.  */
      if (best_above - best_below < 50)
	break;
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (charpos - best_below < best_above - charpos)
    {
      bool record = charpos - best_below > 5000;

      while (best_below != charpos)
	{
	  best_below++;
	  BUF_INC_POS (b, best_below_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.  */
      if (record)
	build_marker (b, best_below, best_below_byte);

      byte_char_debug_check (b, best_below, best_below_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_below;
      cached_bytepos = best_below_byte;

      return best_below_byte;
    }
  else
    {
      bool record = best_above - charpos > 5000;

      while (best_above != charpos)
	{
	  best_above--;
	  BUF_DEC_POS (b, best_above_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.  */
      if (record)
	build_marker (b, best_above, best_above_byte);

      byte_char_debug_check (b, best_above, best_above_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_above;
      cached_bytepos = best_above_byte;

      return best_above_byte;
    }
}

#undef CONSIDER

/* This macro is a subroutine of buf_bytepos_to_charpos.
   It is used when BYTEPOS is actually the byte position.  */

#define CONSIDER(BYTEPOS, CHARPOS)					\
{									\
  ptrdiff_t this_bytepos = (BYTEPOS);					\
  int changed = 0;							\
									\
  if (this_bytepos == bytepos)						\
    {									\
      ptrdiff_t value = (CHARPOS);				       	\
									\
      byte_char_debug_check (b, value, bytepos);			\
      return value;							\
    }									\
  else if (this_bytepos > bytepos)					\
    {									\
      if (this_bytepos < best_above_byte)				\
	{								\
	  best_above = (CHARPOS);					\
	  best_above_byte = this_bytepos;				\
	  changed = 1;							\
	}								\
    }									\
  else if (this_bytepos > best_below_byte)				\
    {									\
      best_below = (CHARPOS);						\
      best_below_byte = this_bytepos;					\
      changed = 1;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
	{								\
	  ptrdiff_t value = best_below + (bytepos - best_below_byte);	\
									\
	  byte_char_debug_check (b, value, bytepos);			\
	  return value;							\
	}								\
    }									\
}

/* Return the character position corresponding to BYTEPOS in B.  */

ptrdiff_t
buf_bytepos_to_charpos (struct buffer *b, ptrdiff_t bytepos)
{
  struct Lisp_Marker *tail;
  ptrdiff_t best_above, best_above_byte;
  ptrdiff_t best_below, best_below_byte;

  eassert (BUF_BEG_BYTE (b) <= bytepos && bytepos <= BUF_Z_BYTE (b));

  best_above = BUF_Z (b);
  best_above_byte = BUF_Z_BYTE (b);

  /* If this buffer has as many characters as bytes,
     each character must be one byte.
     This takes care of the case where enable-multibyte-characters is nil.  */
  if (best_above == best_above_byte)
    return bytepos;

  best_below = BEG;
  best_below_byte = BEG_BYTE;

  CONSIDER (BUF_PT_BYTE (b), BUF_PT (b));
  CONSIDER (BUF_GPT_BYTE (b), BUF_GPT (b));
  CONSIDER (BUF_BEGV_BYTE (b), BUF_BEGV (b));
  CONSIDER (BUF_ZV_BYTE (b), BUF_ZV (b));

  if (b == cached_buffer && BUF_MODIFF (b) == cached_modiff)
    CONSIDER (cached_bytepos, cached_charpos);

  for (tail = BUF_MARKERS (b); tail; tail = tail->next)
    {
      CONSIDER (tail->bytepos, tail->charpos);

      /* If we are down to a range of 50 chars,
	 don't bother checking any other markers;
	 scan the intervening chars directly now.  */
      if (best_above - best_below < 50)
	break;
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (bytepos - best_below_byte < best_above_byte - bytepos)
    {
      bool record = bytepos - best_below_byte > 5000;

      while (best_below_byte < bytepos)
	{
	  best_below++;
	  BUF_INC_POS (b, best_below_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.
	 But don't do it if BUF_MARKERS is nil;
	 that is a signal from Fset_buffer_multibyte.  */
      if (record && BUF_MARKERS (b))
	build_marker (b, best_below, best_below_byte);

      byte_char_debug_check (b, best_below, best_below_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_below;
      cached_bytepos = best_below_byte;

      return best_below;
    }
  else
    {
      bool record = best_above_byte - bytepos > 5000;

      while (best_above_byte > bytepos)
	{
	  best_above--;
	  BUF_DEC_POS (b, best_above_byte);
	}

      /* If this position is quite far from the nearest known position,
	 cache the correspondence by creating a marker here.
	 It will last until the next GC.
	 But don't do it if BUF_MARKERS is nil;
	 that is a signal from Fset_buffer_multibyte.  */
      if (record && BUF_MARKERS (b))
	build_marker (b, best_above, best_above_byte);

      byte_char_debug_check (b, best_above, best_above_byte);

      cached_buffer = b;
      cached_modiff = BUF_MODIFF (b);
      cached_charpos = best_above;
      cached_bytepos = best_above_byte;

      return best_above;
    }
}

#undef CONSIDER

/* Operations on markers. */

/* Internal function to set MARKER in BUFFER at POSITION.  Non-zero
   RESTRICTED means limit the POSITION by the visible part of BUFFER.  */

Lisp_Object
set_marker_internal (Lisp_Object marker, Lisp_Object position,
		     Lisp_Object buffer, bool restricted)
{
  struct Lisp_Marker *m;
  struct buffer *b = live_buffer (buffer);

  CHECK_MARKER (marker);
  m = XMARKER (marker);

  /* Set MARKER to point nowhere if BUFFER is dead, or
     POSITION is nil or a marker points to nowhere.  */
  if (NILP (position)
      || (MARKERP (position) && !XMARKER (position)->buffer)
      || !b)
    unchain_marker (m);

  /* Optimize the special case where we are copying the position of
     an existing marker, and MARKER is already in the same buffer.  */
  else if (MARKERP (position) && b == XMARKER (position)->buffer
	   && b == m->buffer)
    {
      m->bytepos = XMARKER (position)->bytepos;
      m->charpos = XMARKER (position)->charpos;
    }

  else
    {
      register ptrdiff_t charpos, bytepos;

      /* Do not use CHECK_NUMBER_COERCE_MARKER because we
	 don't want to call buf_charpos_to_bytepos if POSITION
	 is a marker and so we know the bytepos already.  */
      if (INTEGERP (position))
	charpos = XINT (position), bytepos = -1;
      else if (MARKERP (position))
	{
	  charpos = XMARKER (position)->charpos;
	  bytepos = XMARKER (position)->bytepos;
	}
      else
	wrong_type_argument (Qinteger_or_marker_p, position);

      charpos = clip_to_bounds
	(restricted ? BUF_BEGV (b) : BUF_BEG (b), charpos,
	 restricted ? BUF_ZV (b) : BUF_Z (b));
      /* Don't believe BYTEPOS if it comes from a different buffer,
	 since that buffer might have a very different correspondence
	 between character and byte positions.  */
      if (bytepos == -1
	  || !(MARKERP (position) && XMARKER (position)->buffer == b))
	bytepos = buf_charpos_to_bytepos (b, charpos);
      else
	bytepos = clip_to_bounds
	  (restricted ? BUF_BEGV_BYTE (b) : BUF_BEG_BYTE (b),
	   bytepos, restricted ? BUF_ZV_BYTE (b) : BUF_Z_BYTE (b));

      attach_marker (m, b, charpos, bytepos);
    }
  return marker;
}

/* Remove MARKER from the chain of whatever buffer it is in,
   leaving it points to nowhere.  This is called during garbage
   collection, so we must be careful to ignore and preserve
   mark bits, including those in chain fields of markers.  */

void
unchain_marker (register struct Lisp_Marker *marker)
{
  register struct buffer *b = marker->buffer;

  if (b)
    {
      register struct Lisp_Marker *tail, **prev;

      /* No dead buffers here.  */
      eassert (BUFFER_LIVE_P (b));

      marker->buffer = NULL;
      prev = &BUF_MARKERS (b);

      for (tail = BUF_MARKERS (b); tail; prev = &tail->next, tail = *prev)
	if (marker == tail)
	  {
	    if (*prev == BUF_MARKERS (b))
	      {
		/* Deleting first marker from the buffer's chain.  Crash
		   if new first marker in chain does not say it belongs
		   to the same buffer, or at least that they have the same
		   base buffer.  */
		if (tail->next && b->text != tail->next->buffer->text)
		  emacs_abort ();
	      }
	    *prev = tail->next;
	    /* We have removed the marker from the chain;
	       no need to scan the rest of the chain.  */
	    break;
	  }

      /* Error if marker was not in it's chain.  */
      eassert (tail != NULL);
    }
}

#ifdef MARKER_DEBUG

/* For debugging -- count the markers in buffer BUF.  */

int
count_markers (struct buffer *buf)
{
  int total = 0;
  struct Lisp_Marker *tail;

  for (tail = BUF_MARKERS (buf); tail; tail = tail->next)
    total++;

  return total;
}

/* For debugging -- recompute the bytepos corresponding
   to CHARPOS in the simplest, most reliable way.  */

ptrdiff_t
verify_bytepos (ptrdiff_t charpos)
{
  ptrdiff_t below = 1;
  ptrdiff_t below_byte = 1;

  while (below != charpos)
    {
      below++;
      BUF_INC_POS (current_buffer, below_byte);
    }

  return below_byte;
}

#endif /* MARKER_DEBUG */


/* Accessors to enable Rust code to get data from the Lisp_Marker struct */

bool_bf
mget_insertion_type(const struct Lisp_Marker *m)
{
  return m->insertion_type;
}

void
mset_insertion_type(struct Lisp_Marker *m, bool_bf val)
{
  m->insertion_type = val;
}

struct Lisp_Marker*
mget_next_marker (struct Lisp_Marker *m) {
  return m->next;
}

void
mset_next_marker (struct Lisp_Marker *m, struct Lisp_Marker *n) {
  m->next = n;
}

struct buffer*
mget_buffer (struct Lisp_Marker *m) {
  return m->buffer;
}

void
mset_buffer(struct Lisp_Marker *m, struct buffer *b)
{
  m->buffer = b;
}

ptrdiff_t
mget_charpos (struct Lisp_Marker *m) {
  return m->charpos;
}

ptrdiff_t
mget_bytepos (struct Lisp_Marker *m) {
  return m->bytepos;
}

