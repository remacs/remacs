BUILT_SOURCES =
CLEANFILES =
EXTRA_DIST =
MOSTLYCLEANDIRS =
MOSTLYCLEANFILES =
noinst_LIBRARIES =
SUFFIXES =

AM_CFLAGS = $(PROFILING_CFLAGS) $(GNULIB_WARN_CFLAGS) $(WERROR_CFLAGS)
DEFAULT_INCLUDES = -I. -I$(top_srcdir)/lib -I../src -I$(top_srcdir)/src

if BUILDING_FOR_WINDOWSNT
include ../nt/gnulib.mk
else
include gnulib.mk

libgnu_a_SOURCES += openat-die.c save-cwd.c
endif

noinst_LIBRARIES += libegnu.a

libegnu_a_SOURCES = $(libgnu_a_SOURCES)
libegnu_a_LIBADD = $(patsubst %.o,e-%.o,$(libgnu_a_LIBADD))
EXTRA_libegnu_a_SOURCES = $(EXTRA_libgnu_a_SOURCES)
libegnu_a_SHORTNAME = e

libegnu_a_CPPFLAGS = $(AM_CPPFLAGS) -Demacs

.PHONY: bootstrap-clean

bootstrap-clean: maintainer-clean
