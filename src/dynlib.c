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

#ifdef WINDOWSNT

/* MS-Windows systems.  */

#include <errno.h>
#include "lisp.h"
#include "w32.h"

static DWORD dynlib_last_err;

/* This needs to be called at startup to countermand any non-zero
   values recorded by temacs.  */
void
dynlib_reset_last_error (void)
{
  dynlib_last_err = 0;
}

dynlib_handle_ptr
dynlib_open (const char *dll_fname)
{
  HMODULE hdll;
  char dll_fname_local[MAX_UTF8_PATH];

  if (!dll_fname)
    {
      errno = ENOTSUP;
      return NULL;
    }

  if (!dll_fname)
    hdll = GetModuleHandle (NULL);
  else
    {
      /* LoadLibrary wants backslashes.  */
      strcpy (dll_fname_local, dll_fname);
      unixtodos_filename (dll_fname_local);

      if (w32_unicode_filenames)
	{
	  wchar_t dll_fname_w[MAX_PATH];

	  filename_to_utf16 (dll_fname_local, dll_fname_w);
	  hdll = LoadLibraryW (dll_fname_w);
	}
      else
	{
	  char dll_fname_a[MAX_PATH];

	  filename_to_ansi (dll_fname_local, dll_fname_a);
	  hdll = LoadLibraryA (dll_fname_a);
	}
    }

  if (!hdll)
    dynlib_last_err = GetLastError ();

  return (dynlib_handle_ptr) hdll;
}

void *
dynlib_sym (dynlib_handle_ptr h, const char *sym)
{
  FARPROC sym_addr = NULL;

  if (!h || h == INVALID_HANDLE_VALUE || !sym)
    {
      dynlib_last_err = ERROR_INVALID_PARAMETER;
      return NULL;
    }

  sym_addr = GetProcAddress ((HMODULE) h, sym);
  if (!sym_addr)
    dynlib_last_err = GetLastError ();

  return (void *)sym_addr;
}

bool
dynlib_addr (void *ptr, const char **path, const char **sym)
{
  return false;  /* Not implemented yet.  */
}

const char *
dynlib_error (void)
{
  char *error_string = NULL;

  if (dynlib_last_err)
    {
      error_string = w32_strerror (dynlib_last_err);
      dynlib_last_err = 0;
    }

  return error_string;
}

int
dynlib_close (dynlib_handle_ptr h)
{
  if (!h || h == INVALID_HANDLE_VALUE)
    {
      dynlib_last_err = ERROR_INVALID_PARAMETER;
      return -1;
    }
  /* If the handle is for the main module (the .exe file), it
     shouldn't be passed to FreeLibrary, because GetModuleHandle
     doesn't increment the refcount, but FreeLibrary does decrement
     it.  I don't think this should matter for the main module, but
     just in case, we avoid the call here, relying on another call to
     GetModuleHandle to return the same value.  */
  if (h == GetModuleHandle (NULL))
    return 0;

  if (!FreeLibrary ((HMODULE) h))
    {
      dynlib_last_err = GetLastError ();
      return -1;
    }

  return 0;
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

#if !HAVE_DLFUNC
# define dlfunc dynlib_sym
#endif

dynlib_function_ptr
dynlib_func (dynlib_handle_ptr h, const char *sym)
{
  return (dynlib_function_ptr) dlfunc (h, sym);
}
