/* Portable API for dynamic loading.

Copyright 2015 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* Assume modules are enabled on modern systems...  *Yes*, the
   preprocessor macro checks could be more precise.  I don't care.

   If you think the abstraction is too leaky use libltdl (libtool),
   don't reinvent the wheel by fixing this one.  */

#include <config.h>

#include "dynlib.h"

#if defined _WIN32

/* MS-Windows systems.  */

#include <windows.h>

dynlib_handle_ptr
dynlib_open (const char *path)
{

  return (dynlib_handle_ptr) LoadLibrary (path);
}

void *
dynlib_sym (dynlib_handle_ptr h, const char *sym)
{
  return GetProcAddress ((HMODULE) h, sym);
}

bool
dynlib_addr (void *ptr, const char **path, const char **sym)
{
  return false;  /* not implemented */
}

const char *
dynlib_error (void)
{
  /* TODO: use GetLastError(), FormatMessage(), ... */
  return "Can't load DLL";
}

int
dynlib_close (dynlib_handle_ptr h)
{
  return FreeLibrary ((HMODULE) h) != 0;
}

#elif defined HAVE_UNISTD_H

/* POSIX systems.  */

#include <dlfcn.h>

dynlib_handle_ptr
dynlib_open (const char *path)
{
  return dlopen (path, RTLD_LAZY);
}

void *
dynlib_sym (dynlib_handle_ptr h, const char *sym)
{
  return dlsym (h, sym);
}

bool
dynlib_addr (void *ptr, const char **path, const char **sym)
{
#ifdef HAVE_DLADDR
  Dl_info info;
  if (dladdr (ptr, &info) && info.dli_fname && info.dli_sname)
    {
      *path = info.dli_fname;
      *sym = info.dli_sname;
      return true;
    }
#endif
  return false;
}

const char *
dynlib_error (void)
{
  return dlerror ();
}

/* FIXME: Currently there is no way to unload a module, so this
   function is never used.  */
#if false
int
dynlib_close (dynlib_handle_ptr h)
{
  return dlclose (h) == 0;
}
#endif

#else

#error "No dynamic loading for this system"

#endif
