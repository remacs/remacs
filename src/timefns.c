/* Timestamp functions for Emacs

Copyright (C) 1985-1987, 1989, 1993-2018 Free Software Foundation, Inc.

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

#include "systime.h"

#include "blockinput.h"
#include "coding.h"
#include "lisp.h"

#include <strftime.h>

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_TIMEZONE_T
# include <sys/param.h>
# if defined __NetBSD_Version__ && __NetBSD_Version__ < 700000000
#  define HAVE_TZALLOC_BUG true
# endif
#endif
#ifndef HAVE_TZALLOC_BUG
# define HAVE_TZALLOC_BUG false
#endif

#define TM_YEAR_BASE 1900

#ifndef HAVE_TM_GMTOFF
# define HAVE_TM_GMTOFF false
#endif

#ifndef TIME_T_MIN
# define TIME_T_MIN TYPE_MINIMUM (time_t)
#endif
#ifndef TIME_T_MAX
# define TIME_T_MAX TYPE_MAXIMUM (time_t)
#endif

/* Return a struct timeval that is roughly equivalent to T.
   Use the least timeval not less than T.
   Return an extremal value if the result would overflow.  */
struct timeval
make_timeval (struct timespec t)
{
  struct timeval tv;
  tv.tv_sec = t.tv_sec;
  tv.tv_usec = t.tv_nsec / 1000;

  if (t.tv_nsec % 1000 != 0)
    {
      if (tv.tv_usec < 999999)
	tv.tv_usec++;
      else if (tv.tv_sec < TYPE_MAXIMUM (time_t))
	{
	  tv.tv_sec++;
	  tv.tv_usec = 0;
	}
    }

  return tv;
}

/* Yield A's UTC offset, or an unspecified value if unknown.  */
static long int
tm_gmtoff (struct tm *a)
{
#if HAVE_TM_GMTOFF
  return a->tm_gmtoff;
#else
  return 0;
#endif
}

/* Yield A - B, measured in seconds.
   This function is copied from the GNU C Library.  */
static int
tm_diff (struct tm *a, struct tm *b)
{
  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid int overflow in leap day calculations,
     but it's OK to assume that A and B are close to each other.  */
  int a4 = (a->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (a->tm_year & 3);
  int b4 = (b->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (b->tm_year & 3);
  int a100 = a4 / 25 - (a4 % 25 < 0);
  int b100 = b4 / 25 - (b4 % 25 < 0);
  int a400 = a100 >> 2;
  int b400 = b100 >> 2;
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
  int years = a->tm_year - b->tm_year;
  int days = (365 * years + intervening_leap_days
	      + (a->tm_yday - b->tm_yday));
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
		+ (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

enum { tzeqlen = sizeof "TZ=" - 1 };

/* Time zones equivalent to current local time and to UTC, respectively.  */
static timezone_t local_tz;
static timezone_t const utc_tz = 0;

static struct tm *
emacs_localtime_rz (timezone_t tz, time_t const *t, struct tm *tm)
{
  tm = localtime_rz (tz, t, tm);
  if (!tm && errno == ENOMEM)
    memory_full (SIZE_MAX);
  return tm;
}

static time_t
emacs_mktime_z (timezone_t tz, struct tm *tm)
{
  errno = 0;
  time_t t = mktime_z (tz, tm);
  if (t == (time_t) -1 && errno == ENOMEM)
    memory_full (SIZE_MAX);
  return t;
}

static _Noreturn void
invalid_time_zone_specification (Lisp_Object zone)
{
  xsignal2 (Qerror, build_string ("Invalid time zone specification"), zone);
}

/* Free a timezone, except do not free the time zone for local time.
   Freeing utc_tz is also a no-op.  */
static void
xtzfree (timezone_t tz)
{
  if (tz != local_tz)
    tzfree (tz);
}

/* Convert the Lisp time zone rule ZONE to a timezone_t object.
   The returned value either is 0, or is LOCAL_TZ, or is newly allocated.
   If SETTZ, set Emacs local time to the time zone rule; otherwise,
   the caller should eventually pass the returned value to xtzfree.  */
static timezone_t
tzlookup (Lisp_Object zone, bool settz)
{
  static char const tzbuf_format[] = "<%+.*"pI"d>%s%"pI"d:%02d:%02d";
  char const *trailing_tzbuf_format = tzbuf_format + sizeof "<%+.*"pI"d" - 1;
  char tzbuf[sizeof tzbuf_format + 2 * INT_STRLEN_BOUND (EMACS_INT)];
  char const *zone_string;
  timezone_t new_tz;

  if (NILP (zone))
    return local_tz;
  else if (EQ (zone, Qt) || EQ (zone, make_fixnum (0)))
    {
      zone_string = "UTC0";
      new_tz = utc_tz;
    }
  else
    {
      bool plain_integer = FIXNUMP (zone);

      if (EQ (zone, Qwall))
	zone_string = 0;
      else if (STRINGP (zone))
	zone_string = SSDATA (ENCODE_SYSTEM (zone));
      else if (plain_integer || (CONSP (zone) && FIXNUMP (XCAR (zone))
				 && CONSP (XCDR (zone))))
	{
	  Lisp_Object abbr UNINIT;
	  if (!plain_integer)
	    {
	      abbr = XCAR (XCDR (zone));
	      zone = XCAR (zone);
	    }

	  EMACS_INT abszone = eabs (XFIXNUM (zone)), hour = abszone / (60 * 60);
	  int hour_remainder = abszone % (60 * 60);
	  int min = hour_remainder / 60, sec = hour_remainder % 60;

	  if (plain_integer)
	    {
	      int prec = 2;
	      EMACS_INT numzone = hour;
	      if (hour_remainder != 0)
		{
		  prec += 2, numzone = 100 * numzone + min;
		  if (sec != 0)
		    prec += 2, numzone = 100 * numzone + sec;
		}
	      sprintf (tzbuf, tzbuf_format, prec,
		       XFIXNUM (zone) < 0 ? -numzone : numzone,
		       &"-"[XFIXNUM (zone) < 0], hour, min, sec);
	      zone_string = tzbuf;
	    }
	  else
	    {
	      AUTO_STRING (leading, "<");
	      AUTO_STRING_WITH_LEN (trailing, tzbuf,
				    sprintf (tzbuf, trailing_tzbuf_format,
					     &"-"[XFIXNUM (zone) < 0],
					     hour, min, sec));
	      zone_string = SSDATA (concat3 (leading, ENCODE_SYSTEM (abbr),
					     trailing));
	    }
	}
      else
	invalid_time_zone_specification (zone);

      new_tz = tzalloc (zone_string);

      if (HAVE_TZALLOC_BUG && !new_tz && errno != ENOMEM && plain_integer
	  && XFIXNUM (zone) % (60 * 60) == 0)
	{
	  /* tzalloc mishandles POSIX strings; fall back on tzdb if
	     possible (Bug#30738).  */
	  sprintf (tzbuf, "Etc/GMT%+"pI"d", - (XFIXNUM (zone) / (60 * 60)));
	  new_tz = tzalloc (zone_string);
	}

      if (!new_tz)
	{
	  if (errno == ENOMEM)
	    memory_full (SIZE_MAX);
	  invalid_time_zone_specification (zone);
	}
    }

  if (settz)
    {
      block_input ();
      emacs_setenv_TZ (zone_string);
      tzset ();
      timezone_t old_tz = local_tz;
      local_tz = new_tz;
      tzfree (old_tz);
      unblock_input ();
    }

  return new_tz;
}

void
init_timefns (bool dumping)
{
#ifndef CANNOT_DUMP
  /* A valid but unlikely setting for the TZ environment variable.
     It is OK (though a bit slower) if the user chooses this value.  */
  static char dump_tz_string[] = "TZ=UtC0";

  /* When just dumping out, set the time zone to a known unlikely value
     and skip the rest of this function.  */
  if (dumping)
    {
      xputenv (dump_tz_string);
      tzset ();
      return;
    }
#endif

  char *tz = getenv ("TZ");

#if !defined CANNOT_DUMP
  /* If the execution TZ happens to be the same as the dump TZ,
     change it to some other value and then change it back,
     to force the underlying implementation to reload the TZ info.
     This is needed on implementations that load TZ info from files,
     since the TZ file contents may differ between dump and execution.  */
  if (tz && strcmp (tz, &dump_tz_string[tzeqlen]) == 0)
    {
      ++*tz;
      tzset ();
      --*tz;
    }
#endif

  /* Set the time zone rule now, so that the call to putenv is done
     before multiple threads are active.  */
  tzlookup (tz ? build_string (tz) : Qwall, true);
}

/* Report that a time value is out of range for Emacs.  */
void
time_overflow (void)
{
  error ("Specified time is not representable");
}

static _Noreturn void
invalid_time (void)
{
  error ("Invalid time specification");
}

/* Check a return value compatible with that of decode_time_components.  */
static void
check_time_validity (int validity)
{
  if (validity <= 0)
    {
      if (validity < 0)
	time_overflow ();
      else
	invalid_time ();
    }
}

/* Return the upper part of the time T (everything but the bottom 16 bits).  */
static EMACS_INT
hi_time (time_t t)
{
  time_t hi = t >> LO_TIME_BITS;
  if (FIXNUM_OVERFLOW_P (hi))
    time_overflow ();
  return hi;
}

/* Return the bottom bits of the time T.  */
static int
lo_time (time_t t)
{
  return t & ((1 << LO_TIME_BITS) - 1);
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   Set *PHIGH, *PLOW, *PUSEC, *PPSEC to its parts; do not check their values.
   Return 2, 3, or 4 to indicate the effective length of SPECIFIED_TIME
   if successful, 0 if unsuccessful.  */
static int
disassemble_lisp_time (Lisp_Object specified_time, Lisp_Object *phigh,
		       Lisp_Object *plow, Lisp_Object *pusec,
		       Lisp_Object *ppsec)
{
  Lisp_Object high = make_fixnum (0);
  Lisp_Object low = specified_time;
  Lisp_Object usec = make_fixnum (0);
  Lisp_Object psec = make_fixnum (0);
  int len = 4;

  if (CONSP (specified_time))
    {
      high = XCAR (specified_time);
      low = XCDR (specified_time);
      if (CONSP (low))
	{
	  Lisp_Object low_tail = XCDR (low);
	  low = XCAR (low);
	  if (CONSP (low_tail))
	    {
	      usec = XCAR (low_tail);
	      low_tail = XCDR (low_tail);
	      if (CONSP (low_tail))
		psec = XCAR (low_tail);
	      else
		len = 3;
	    }
	  else if (!NILP (low_tail))
	    {
	      usec = low_tail;
	      len = 3;
	    }
	  else
	    len = 2;
	}
      else
	len = 2;

      /* When combining components, require LOW to be an integer,
	 as otherwise it would be a pain to add up times.  */
      if (! INTEGERP (low))
	return 0;
    }
  else if (INTEGERP (specified_time))
    len = 2;

  *phigh = high;
  *plow = low;
  *pusec = usec;
  *ppsec = psec;
  return len;
}

/* Convert T into an Emacs time *RESULT, truncating toward minus infinity.
   Return true if T is in range, false otherwise.  */
static bool
decode_float_time (double t, struct lisp_time *result)
{
  double lo_multiplier = 1 << LO_TIME_BITS;
  double emacs_time_min = MOST_NEGATIVE_FIXNUM * lo_multiplier;
  if (! (emacs_time_min <= t && t < -emacs_time_min))
    return false;

  double small_t = t / lo_multiplier;
  EMACS_INT hi = small_t;
  double t_sans_hi = t - hi * lo_multiplier;
  int lo = t_sans_hi;
  long double fracps = (t_sans_hi - lo) * 1e12L;
#ifdef INT_FAST64_MAX
  int_fast64_t ifracps = fracps;
  int us = ifracps / 1000000;
  int ps = ifracps % 1000000;
#else
  int us = fracps / 1e6L;
  int ps = fracps - us * 1e6L;
#endif
  us -= (ps < 0);
  ps += (ps < 0) * 1000000;
  lo -= (us < 0);
  us += (us < 0) * 1000000;
  hi -= (lo < 0);
  lo += (lo < 0) << LO_TIME_BITS;
  result->hi = hi;
  result->lo = lo;
  result->us = us;
  result->ps = ps;
  return true;
}

/* From the time components HIGH, LOW, USEC and PSEC taken from a Lisp
   list, generate the corresponding time value.
   If LOW is floating point, the other components should be zero.

   If RESULT is not null, store into *RESULT the converted time.
   If *DRESULT is not null, store into *DRESULT the number of
   seconds since the start of the POSIX Epoch.

   Return 1 if successful, 0 if the components are of the
   wrong type, and -1 if the time is out of range.  */
int
decode_time_components (Lisp_Object high, Lisp_Object low, Lisp_Object usec,
			Lisp_Object psec,
			struct lisp_time *result, double *dresult)
{
  EMACS_INT hi, us, ps;
  intmax_t lo;
  if (! (FIXNUMP (high)
	 && FIXNUMP (usec) && FIXNUMP (psec)))
    return 0;
  if (! INTEGERP (low))
    {
      if (FLOATP (low))
	{
	  double t = XFLOAT_DATA (low);
	  if (result && ! decode_float_time (t, result))
	    return -1;
	  if (dresult)
	    *dresult = t;
	  return 1;
	}
      else if (NILP (low))
	{
	  struct timespec now = current_timespec ();
	  if (result)
	    {
	      result->hi = hi_time (now.tv_sec);
	      result->lo = lo_time (now.tv_sec);
	      result->us = now.tv_nsec / 1000;
	      result->ps = now.tv_nsec % 1000 * 1000;
	    }
	  if (dresult)
	    *dresult = now.tv_sec + now.tv_nsec / 1e9;
	  return 1;
	}
      else
	return 0;
    }

  hi = XFIXNUM (high);
  if (! integer_to_intmax (low, &lo))
    return -1;
  us = XFIXNUM (usec);
  ps = XFIXNUM (psec);

  /* Normalize out-of-range lower-order components by carrying
     each overflow into the next higher-order component.  */
  us += ps / 1000000 - (ps % 1000000 < 0);
  lo += us / 1000000 - (us % 1000000 < 0);
  if (INT_ADD_WRAPV (lo >> LO_TIME_BITS, hi, &hi))
    return -1;
  ps = ps % 1000000 + 1000000 * (ps % 1000000 < 0);
  us = us % 1000000 + 1000000 * (us % 1000000 < 0);
  lo &= (1 << LO_TIME_BITS) - 1;

  if (result)
    {
      if (FIXNUM_OVERFLOW_P (hi))
	return -1;
      result->hi = hi;
      result->lo = lo;
      result->us = us;
      result->ps = ps;
    }

  if (dresult)
    {
      double dhi = hi;
      *dresult = (us * 1e6 + ps) / 1e12 + lo + dhi * (1 << LO_TIME_BITS);
    }

  return 1;
}

struct timespec
lisp_to_timespec (struct lisp_time t)
{
  if (! ((TYPE_SIGNED (time_t) ? TIME_T_MIN >> LO_TIME_BITS <= t.hi : 0 <= t.hi)
	 && t.hi <= TIME_T_MAX >> LO_TIME_BITS))
    return invalid_timespec ();
  time_t s = (t.hi << LO_TIME_BITS) + t.lo;
  int ns = t.us * 1000 + t.ps / 1000;
  return make_timespec (s, ns);
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   Store its effective length into *PLEN.
   If SPECIFIED_TIME is nil, use the current time.
   Signal an error if SPECIFIED_TIME does not represent a time.  */
static struct lisp_time
lisp_time_struct (Lisp_Object specified_time, int *plen)
{
  Lisp_Object high, low, usec, psec;
  struct lisp_time t;
  int len = disassemble_lisp_time (specified_time, &high, &low, &usec, &psec);
  if (!len)
    invalid_time ();
  int val = decode_time_components (high, low, usec, psec, &t, 0);
  check_time_validity (val);
  *plen = len;
  return t;
}

/* Like lisp_time_struct, except return a struct timespec.
   Discard any low-order digits.  */
struct timespec
lisp_time_argument (Lisp_Object specified_time)
{
  int len;
  struct lisp_time lt = lisp_time_struct (specified_time, &len);
  struct timespec t = lisp_to_timespec (lt);
  if (! timespec_valid_p (t))
    time_overflow ();
  return t;
}

/* Like lisp_time_argument, except decode only the seconds part,
   and do not check the subseconds part.  */
static time_t
lisp_seconds_argument (Lisp_Object specified_time)
{
  Lisp_Object high, low, usec, psec;
  struct lisp_time t;

  int val = disassemble_lisp_time (specified_time, &high, &low, &usec, &psec);
  if (val != 0)
    {
      val = decode_time_components (high, low, make_fixnum (0),
				    make_fixnum (0), &t, 0);
      if (0 < val
	  && ! ((TYPE_SIGNED (time_t)
		 ? TIME_T_MIN >> LO_TIME_BITS <= t.hi
		 : 0 <= t.hi)
		&& t.hi <= TIME_T_MAX >> LO_TIME_BITS))
	val = -1;
    }
  check_time_validity (val);
  return (t.hi << LO_TIME_BITS) + t.lo;
}

static struct lisp_time
time_add (struct lisp_time ta, struct lisp_time tb)
{
  EMACS_INT hi = ta.hi + tb.hi;
  int lo = ta.lo + tb.lo;
  int us = ta.us + tb.us;
  int ps = ta.ps + tb.ps;
  us += (1000000 <= ps);
  ps -= (1000000 <= ps) * 1000000;
  lo += (1000000 <= us);
  us -= (1000000 <= us) * 1000000;
  hi += (1 << LO_TIME_BITS <= lo);
  lo -= (1 << LO_TIME_BITS <= lo) << LO_TIME_BITS;
  return (struct lisp_time) { hi, lo, us, ps };
}

static struct lisp_time
time_subtract (struct lisp_time ta, struct lisp_time tb)
{
  EMACS_INT hi = ta.hi - tb.hi;
  int lo = ta.lo - tb.lo;
  int us = ta.us - tb.us;
  int ps = ta.ps - tb.ps;
  us -= (ps < 0);
  ps += (ps < 0) * 1000000;
  lo -= (us < 0);
  us += (us < 0) * 1000000;
  hi -= (lo < 0);
  lo += (lo < 0) << LO_TIME_BITS;
  return (struct lisp_time) { hi, lo, us, ps };
}

static Lisp_Object
time_arith (Lisp_Object a, Lisp_Object b, bool subtract)
{
  if (FLOATP (a) && !isfinite (XFLOAT_DATA (a)))
    {
      double da = XFLOAT_DATA (a);
      double db = XFLOAT_DATA (Ffloat_time (b));
      return make_float (subtract ? da - db : da + db);
    }
  if (FLOATP (b) && !isfinite (XFLOAT_DATA (b)))
    return subtract ? make_float (-XFLOAT_DATA (b)) : b;

  int alen, blen;
  struct lisp_time ta = lisp_time_struct (a, &alen);
  struct lisp_time tb = lisp_time_struct (b, &blen);
  struct lisp_time t = (subtract ? time_subtract : time_add) (ta, tb);
  if (FIXNUM_OVERFLOW_P (t.hi))
    time_overflow ();
  Lisp_Object val = Qnil;

  switch (max (alen, blen))
    {
    default:
      val = Fcons (make_fixnum (t.ps), val);
      FALLTHROUGH;
    case 3:
      val = Fcons (make_fixnum (t.us), val);
      FALLTHROUGH;
    case 2:
      val = Fcons (make_fixnum (t.lo), val);
      val = Fcons (make_fixnum (t.hi), val);
      break;
    }

  return val;
}

DEFUN ("time-add", Ftime_add, Stime_add, 2, 2, 0,
       doc: /* Return the sum of two time values A and B, as a time value.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_arith (a, b, false);
}

DEFUN ("time-subtract", Ftime_subtract, Stime_subtract, 2, 2, 0,
       doc: /* Return the difference between two time values A and B, as a time value.
Use `float-time' to convert the difference into elapsed seconds.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_arith (a, b, true);
}

/* Return negative, 0, positive if a < b, a == b, a > b respectively.
   Return positive if either a or b is a NaN; this is good enough
   for the current callers.  */
static int
time_cmp (Lisp_Object a, Lisp_Object b)
{
  if ((FLOATP (a) && !isfinite (XFLOAT_DATA (a)))
      || (FLOATP (b) && !isfinite (XFLOAT_DATA (b))))
    {
      double da = FLOATP (a) ? XFLOAT_DATA (a) : 0;
      double db = FLOATP (b) ? XFLOAT_DATA (b) : 0;
      return da < db ? -1 : da != db;
    }

  int alen, blen;
  struct lisp_time ta = lisp_time_struct (a, &alen);
  struct lisp_time tb = lisp_time_struct (b, &blen);
  return (ta.hi != tb.hi ? (ta.hi < tb.hi ? -1 : 1)
	  : ta.lo != tb.lo ? (ta.lo < tb.lo ? -1 : 1)
	  : ta.us != tb.us ? (ta.us < tb.us ? -1 : 1)
	  : ta.ps < tb.ps ? -1 : ta.ps != tb.ps);
}

DEFUN ("time-less-p", Ftime_less_p, Stime_less_p, 2, 2, 0,
       doc: /* Return non-nil if time value T1 is earlier than time value T2.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object t1, Lisp_Object t2)
{
  return time_cmp (t1, t2) < 0 ? Qt : Qnil;
}

DEFUN ("time-equal-p", Ftime_equal_p, Stime_equal_p, 2, 2, 0,
       doc: /* Return non-nil if T1 and T2 are equal time values.
A nil value for either argument stands for the current time.
See `current-time-string' for the various forms of a time value.  */)
  (Lisp_Object t1, Lisp_Object t2)
{
  return time_cmp (t1, t2) == 0 ? Qt : Qnil;
}


/* Make a Lisp list that represents the Emacs time T.  T may be an
   invalid time, with a slightly negative tv_nsec value such as
   UNKNOWN_MODTIME_NSECS; in that case, the Lisp list contains a
   correspondingly negative picosecond count.  */
Lisp_Object
make_lisp_time (struct timespec t)
{
  time_t s = t.tv_sec;
  int ns = t.tv_nsec;
  return list4i (hi_time (s), lo_time (s), ns / 1000, ns % 1000 * 1000);
}

DEFUN ("float-time", Ffloat_time, Sfloat_time, 0, 1, 0,
       doc: /* Return the current time, as a float number of seconds since the epoch.
If SPECIFIED-TIME is given, it is the time to convert to float
instead of the current time.  The argument should have the form
\(HIGH LOW) or (HIGH LOW USEC) or (HIGH LOW USEC PSEC).  Thus,
you can use times from `current-time' and from `file-attributes'.
SPECIFIED-TIME can also have the form (HIGH . LOW), but this is
considered obsolete.

WARNING: Since the result is floating point, it may not be exact.
If precise time stamps are required, use either `current-time',
or (if you need time as a string) `format-time-string'.  */)
  (Lisp_Object specified_time)
{
  double t;
  Lisp_Object high, low, usec, psec;
  if (! (disassemble_lisp_time (specified_time, &high, &low, &usec, &psec)
	 && decode_time_components (high, low, usec, psec, 0, &t)))
    invalid_time ();
  return make_float (t);
}

/* Write information into buffer S of size MAXSIZE, according to the
   FORMAT of length FORMAT_LEN, using time information taken from *TP.
   Use the time zone specified by TZ.
   Use NS as the number of nanoseconds in the %N directive.
   Return the number of bytes written, not including the terminating
   '\0'.  If S is NULL, nothing will be written anywhere; so to
   determine how many bytes would be written, use NULL for S and
   ((size_t) -1) for MAXSIZE.

   This function behaves like nstrftime, except it allows null
   bytes in FORMAT and it does not support nanoseconds.  */
static size_t
emacs_nmemftime (char *s, size_t maxsize, const char *format,
		 size_t format_len, const struct tm *tp, timezone_t tz, int ns)
{
  size_t total = 0;

  /* Loop through all the null-terminated strings in the format
     argument.  Normally there's just one null-terminated string, but
     there can be arbitrarily many, concatenated together, if the
     format contains '\0' bytes.  nstrftime stops at the first
     '\0' byte so we must invoke it separately for each such string.  */
  for (;;)
    {
      size_t len;
      size_t result;

      if (s)
	s[0] = '\1';

      result = nstrftime (s, maxsize, format, tp, tz, ns);

      if (s)
	{
	  if (result == 0 && s[0] != '\0')
	    return 0;
	  s += result + 1;
	}

      maxsize -= result + 1;
      total += result;
      len = strlen (format);
      if (len == format_len)
	return total;
      total++;
      format += len + 1;
      format_len -= len + 1;
    }
}

static Lisp_Object
format_time_string (char const *format, ptrdiff_t formatlen,
		    struct timespec t, Lisp_Object zone, struct tm *tmp)
{
  char buffer[4000];
  char *buf = buffer;
  ptrdiff_t size = sizeof buffer;
  size_t len;
  int ns = t.tv_nsec;
  USE_SAFE_ALLOCA;

  timezone_t tz = tzlookup (zone, false);
  /* On some systems, like 32-bit MinGW, tv_sec of struct timespec is
     a 64-bit type, but time_t is a 32-bit type.  emacs_localtime_rz
     expects a pointer to time_t value.  */
  time_t tsec = t.tv_sec;
  tmp = emacs_localtime_rz (tz, &tsec, tmp);
  if (! tmp)
    {
      xtzfree (tz);
      time_overflow ();
    }
  synchronize_system_time_locale ();

  while (true)
    {
      buf[0] = '\1';
      len = emacs_nmemftime (buf, size, format, formatlen, tmp, tz, ns);
      if ((0 < len && len < size) || (len == 0 && buf[0] == '\0'))
	break;

      /* Buffer was too small, so make it bigger and try again.  */
      len = emacs_nmemftime (NULL, SIZE_MAX, format, formatlen, tmp, tz, ns);
      if (STRING_BYTES_BOUND <= len)
	{
	  xtzfree (tz);
	  string_overflow ();
	}
      size = len + 1;
      buf = SAFE_ALLOCA (size);
    }

  xtzfree (tz);
  AUTO_STRING_WITH_LEN (bufstring, buf, len);
  Lisp_Object result = code_convert_string_norecord (bufstring,
						     Vlocale_coding_system, 0);
  SAFE_FREE ();
  return result;
}

DEFUN ("format-time-string", Fformat_time_string, Sformat_time_string, 1, 3, 0,
       doc: /* Use FORMAT-STRING to format the time TIME, or now if omitted or nil.
TIME is specified as (HIGH LOW USEC PSEC), as returned by
`current-time' or `file-attributes'.  It can also be a single integer
number of seconds since the epoch.  The obsolete form (HIGH . LOW) is
also still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

The value is a copy of FORMAT-STRING, but with certain constructs replaced
by text that describes the specified date and time in TIME:

%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale's abbreviated month name, %B the full name.
 (%h is not supported on MS-Windows.)
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale's abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale's equivalent of either AM or PM.
%q is the calendar quarter (1–4).
%M is the minute (00-59).
%S is the second (00-59; 00-60 on platforms with leap seconds)
%s is the number of seconds since 1970-01-01 00:00:00 +0000.
%N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
%Z is the time zone abbreviation, %z is the numeric form.

%c is the locale's date and time format.
%x is the locale's "preferred" date format.
%D is like "%m/%d/%y".
%F is the ISO 8601 date format (like "%Y-%m-%d").

%R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
%X is the locale's "preferred" time format.

Finally, %n is a newline, %t is a tab, %% is a literal %, and
unrecognized %-sequences stand for themselves.

Certain flags and modifiers are available with some format controls.
The flags are `_', `-', `^' and `#'.  For certain characters X,
%_X is like %X, but padded with blanks; %-X is like %X,
but without padding.  %^X is like %X, but with all textual
characters up-cased; %#X is like %X, but with letter-case of
all textual characters reversed.
%NX (where N stands for an integer) is like %X,
but takes up at least N (a number) positions.
The modifiers are `E' and `O'.  For certain characters X,
%EX is a locale's alternative version of %X;
%OX is like %X, but uses the locale's number symbols.

For example, to produce full ISO 8601 format, use "%FT%T%z".

usage: (format-time-string FORMAT-STRING &optional TIME ZONE)  */)
  (Lisp_Object format_string, Lisp_Object timeval, Lisp_Object zone)
{
  struct timespec t = lisp_time_argument (timeval);
  struct tm tm;

  CHECK_STRING (format_string);
  format_string = code_convert_string_norecord (format_string,
						Vlocale_coding_system, 1);
  return format_time_string (SSDATA (format_string), SBYTES (format_string),
			     t, zone, &tm);
}

DEFUN ("decode-time", Fdecode_time, Sdecode_time, 0, 2, 0,
       doc: /* Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).
The optional TIME should be a list of (HIGH LOW . IGNORED),
as from `current-time' and `file-attributes', or nil to use the
current time.  It can also be a single integer number of seconds since
the epoch.  The obsolete form (HIGH . LOW) is also still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (the UTC offset in seconds) applied
without consideration for daylight saving time.

The list has the following nine members: SEC is an integer between 0
and 60; SEC is 60 for a leap second, which only some operating systems
support.  MINUTE is an integer between 0 and 59.  HOUR is an integer
between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
integer between 1 and 12.  YEAR is an integer indicating the
four-digit year.  DOW is the day of week, an integer between 0 and 6,
where 0 is Sunday.  DST is t if daylight saving time is in effect,
nil if it is not in effect, and -1 if daylight saving information is
not available.  UTCOFF is an integer indicating the UTC offset in
seconds, i.e., the number of seconds east of Greenwich.  (Note that
Common Lisp has different meanings for DOW and UTCOFF.)

usage: (decode-time &optional TIME ZONE)  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  time_t time_spec = lisp_seconds_argument (specified_time);
  struct tm local_tm, gmt_tm;
  timezone_t tz = tzlookup (zone, false);
  struct tm *tm = emacs_localtime_rz (tz, &time_spec, &local_tm);
  xtzfree (tz);

  if (! (tm
	 && MOST_NEGATIVE_FIXNUM - TM_YEAR_BASE <= local_tm.tm_year
	 && local_tm.tm_year <= MOST_POSITIVE_FIXNUM - TM_YEAR_BASE))
    time_overflow ();

  /* Avoid overflow when INT_MAX < EMACS_INT_MAX.  */
  EMACS_INT tm_year_base = TM_YEAR_BASE;

  return CALLN (Flist,
		make_fixnum (local_tm.tm_sec),
		make_fixnum (local_tm.tm_min),
		make_fixnum (local_tm.tm_hour),
		make_fixnum (local_tm.tm_mday),
		make_fixnum (local_tm.tm_mon + 1),
		make_fixnum (local_tm.tm_year + tm_year_base),
		make_fixnum (local_tm.tm_wday),
		(local_tm.tm_isdst < 0 ? make_fixnum (-1)
		 : local_tm.tm_isdst == 0 ? Qnil : Qt),
		(HAVE_TM_GMTOFF
		 ? make_fixnum (tm_gmtoff (&local_tm))
		 : gmtime_r (&time_spec, &gmt_tm)
		 ? make_fixnum (tm_diff (&local_tm, &gmt_tm))
		 : Qnil));
}

/* Return OBJ - OFFSET, checking that OBJ is a valid fixnum and that
   the result is representable as an int.  */
static int
check_tm_member (Lisp_Object obj, int offset)
{
  CHECK_FIXNUM (obj);
  EMACS_INT n = XFIXNUM (obj);
  int result;
  if (INT_SUBTRACT_WRAPV (n, offset, &result))
    time_overflow ();
  return result;
}

DEFUN ("encode-time", Fencode_time, Sencode_time, 6, MANY, 0,
       doc: /* Convert SECOND, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.
This is the reverse operation of `decode-time', which see.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

You can pass more than 7 arguments; then the first six arguments
are used as SECOND through YEAR, and the *last* argument is used as ZONE.
The intervening arguments are ignored.
This feature lets (apply \\='encode-time (decode-time ...)) work.

Out-of-range values for SECOND, MINUTE, HOUR, DAY, or MONTH are allowed;
for example, a DAY of 0 means the day preceding the given month.
Year numbers less than 100 are treated just like other year numbers.
If you want them to stand for years in this century, you must do that yourself.

Years before 1970 are not guaranteed to work.  On some systems,
year values as low as 1901 do work.

usage: (encode-time SECOND MINUTE HOUR DAY MONTH YEAR &optional ZONE)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  time_t value;
  struct tm tm;
  Lisp_Object zone = (nargs > 6 ? args[nargs - 1] : Qnil);

  tm.tm_sec  = check_tm_member (args[0], 0);
  tm.tm_min  = check_tm_member (args[1], 0);
  tm.tm_hour = check_tm_member (args[2], 0);
  tm.tm_mday = check_tm_member (args[3], 0);
  tm.tm_mon  = check_tm_member (args[4], 1);
  tm.tm_year = check_tm_member (args[5], TM_YEAR_BASE);
  tm.tm_isdst = -1;

  timezone_t tz = tzlookup (zone, false);
  value = emacs_mktime_z (tz, &tm);
  xtzfree (tz);

  if (value == (time_t) -1)
    time_overflow ();

  return list2i (hi_time (value), lo_time (value));
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
       doc: /* Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of integers (HIGH LOW USEC PSEC).
HIGH has the most significant bits of the seconds, while LOW has the
least significant 16 bits.  USEC and PSEC are the microsecond and
picosecond counts.  */)
  (void)
{
  return make_lisp_time (current_timespec ());
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string,
       0, 2, 0,
       doc: /* Return the current local time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed
if the year is in the range 1000-9999.
The format is `Sun Sep 16 01:03:52 1973'.
However, see also the functions `decode-time' and `format-time-string'
which provide a much more powerful and general facility.

If SPECIFIED-TIME is given, it is a time to format instead of the
current time.  The argument should have the form (HIGH LOW . IGNORED).
Thus, you can use times obtained from `current-time' and from
`file-attributes'.  SPECIFIED-TIME can also be a single integer number
of seconds since the epoch.  The obsolete form (HIGH . LOW) is also
still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  time_t value = lisp_seconds_argument (specified_time);
  timezone_t tz = tzlookup (zone, false);

  /* Convert to a string in ctime format, except without the trailing
     newline, and without the 4-digit year limit.  Don't use asctime
     or ctime, as they might dump core if the year is outside the
     range -999 .. 9999.  */
  struct tm tm;
  struct tm *tmp = emacs_localtime_rz (tz, &value, &tm);
  xtzfree (tz);
  if (! tmp)
    time_overflow ();

  static char const wday_name[][4] =
    { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
  static char const mon_name[][4] =
    { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
  printmax_t year_base = TM_YEAR_BASE;
  char buf[sizeof "Mon Apr 30 12:49:17 " + INT_STRLEN_BOUND (int) + 1];
  int len = sprintf (buf, "%s %s%3d %02d:%02d:%02d %"pMd,
		     wday_name[tm.tm_wday], mon_name[tm.tm_mon], tm.tm_mday,
		     tm.tm_hour, tm.tm_min, tm.tm_sec,
		     tm.tm_year + year_base);

  return make_unibyte_string (buf, len);
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 2, 0,
       doc: /* Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
If SPECIFIED-TIME is given, the time zone offset is determined from it
instead of using the current time.  The argument should have the form
\(HIGH LOW . IGNORED).  Thus, you can use times obtained from
`current-time' and from `file-attributes'.  SPECIFIED-TIME can also be
a single integer number of seconds since the epoch.  The obsolete form
(HIGH . LOW) is also still accepted.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

Some operating systems cannot provide all this information to Emacs;
in this case, `current-time-zone' returns a list containing nil for
the data it can't find.  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  struct timespec value;
  struct tm local_tm, gmt_tm;
  Lisp_Object zone_offset, zone_name;

  zone_offset = Qnil;
  value = make_timespec (lisp_seconds_argument (specified_time), 0);
  zone_name = format_time_string ("%Z", sizeof "%Z" - 1, value,
				  zone, &local_tm);

  /* gmtime_r expects a pointer to time_t, but tv_sec of struct
     timespec on some systems (MinGW) is a 64-bit field.  */
  time_t tsec = value.tv_sec;
  if (HAVE_TM_GMTOFF || gmtime_r (&tsec, &gmt_tm))
    {
      long int offset = (HAVE_TM_GMTOFF
			 ? tm_gmtoff (&local_tm)
			 : tm_diff (&local_tm, &gmt_tm));
      zone_offset = make_fixnum (offset);
      if (SCHARS (zone_name) == 0)
	{
	  /* No local time zone name is available; use numeric zone instead.  */
	  long int hour = offset / 3600;
	  int min_sec = offset % 3600;
	  int amin_sec = min_sec < 0 ? - min_sec : min_sec;
	  int min = amin_sec / 60;
	  int sec = amin_sec % 60;
	  int min_prec = min_sec ? 2 : 0;
	  int sec_prec = sec ? 2 : 0;
	  char buf[sizeof "+0000" + INT_STRLEN_BOUND (long int)];
	  zone_name = make_formatted_string (buf, "%c%.2ld%.*d%.*d",
					     (offset < 0 ? '-' : '+'),
					     hour, min_prec, min, sec_prec, sec);
	}
    }

  return list2 (zone_offset, zone_name);
}

DEFUN ("set-time-zone-rule", Fset_time_zone_rule, Sset_time_zone_rule, 1, 1, 0,
       doc: /* Set the Emacs local time zone using TZ, a string specifying a time zone rule.
If TZ is nil or `wall', use system wall clock time; this differs from
the usual Emacs convention where nil means current local time.  If TZ
is t, use Universal Time.  If TZ is a list (as from
`current-time-zone') or an integer (as from `decode-time'), use the
specified time zone without consideration for daylight saving time.

Instead of calling this function, you typically want something else.
To temporarily use a different time zone rule for just one invocation
of `decode-time', `encode-time', or `format-time-string', pass the
function a ZONE argument.  To change local time consistently
throughout Emacs, call (setenv "TZ" TZ): this changes both the
environment of the Emacs process and the variable
`process-environment', whereas `set-time-zone-rule' affects only the
former.  */)
  (Lisp_Object tz)
{
  tzlookup (NILP (tz) ? Qwall : tz, true);
  return Qnil;
}

/* A buffer holding a string of the form "TZ=value", intended
   to be part of the environment.  If TZ is supposed to be unset,
   the buffer string is "tZ=".  */
 static char *tzvalbuf;

/* Get the local time zone rule.  */
char *
emacs_getenv_TZ (void)
{
  return tzvalbuf[0] == 'T' ? tzvalbuf + tzeqlen : 0;
}

/* Set the local time zone rule to TZSTRING, which can be null to
   denote wall clock time.  Do not record the setting in LOCAL_TZ.

   This function is not thread-safe, in theory because putenv is not,
   but mostly because of the static storage it updates.  Other threads
   that invoke localtime etc. may be adversely affected while this
   function is executing.  */

int
emacs_setenv_TZ (const char *tzstring)
{
  static ptrdiff_t tzvalbufsize;
  ptrdiff_t tzstringlen = tzstring ? strlen (tzstring) : 0;
  char *tzval = tzvalbuf;
  bool new_tzvalbuf = tzvalbufsize <= tzeqlen + tzstringlen;

  if (new_tzvalbuf)
    {
      /* Do not attempt to free the old tzvalbuf, since another thread
	 may be using it.  In practice, the first allocation is large
	 enough and memory does not leak.  */
      tzval = xpalloc (NULL, &tzvalbufsize,
		       tzeqlen + tzstringlen - tzvalbufsize + 1, -1, 1);
      tzvalbuf = tzval;
      tzval[1] = 'Z';
      tzval[2] = '=';
    }

  if (tzstring)
    {
      /* Modify TZVAL in place.  Although this is dicey in a
	 multithreaded environment, we know of no portable alternative.
	 Calling putenv or setenv could crash some other thread.  */
      tzval[0] = 'T';
      strcpy (tzval + tzeqlen, tzstring);
    }
  else
    {
      /* Turn 'TZ=whatever' into an empty environment variable 'tZ='.
	 Although this is also dicey, calling unsetenv here can crash Emacs.
	 See Bug#8705.  */
      tzval[0] = 't';
      tzval[tzeqlen] = 0;
    }


#ifndef WINDOWSNT
  /* Modifying *TZVAL merely requires calling tzset (which is the
     caller's responsibility).  However, modifying TZVAL requires
     calling putenv; although this is not thread-safe, in practice this
     runs only on startup when there is only one thread.  */
  bool need_putenv = new_tzvalbuf;
#else
  /* MS-Windows 'putenv' copies the argument string into a block it
     allocates, so modifying *TZVAL will not change the environment.
     However, the other threads run by Emacs on MS-Windows never call
     'xputenv' or 'putenv' or 'unsetenv', so the original cause for the
     dicey in-place modification technique doesn't exist there in the
     first place.  */
  bool need_putenv = true;
#endif
  if (need_putenv)
    xputenv (tzval);

  return 0;
}

void
syms_of_timefns (void)
{
  defsubr (&Scurrent_time);
  defsubr (&Stime_add);
  defsubr (&Stime_subtract);
  defsubr (&Stime_less_p);
  defsubr (&Stime_equal_p);
  defsubr (&Sformat_time_string);
  defsubr (&Sfloat_time);
  defsubr (&Sdecode_time);
  defsubr (&Sencode_time);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_zone);
  defsubr (&Sset_time_zone_rule);
}
