/* emacs-module.c - Module loading and runtime implementation

Copyright (C) 2015 Free Software Foundation, Inc.

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

#include <config.h>

#include "emacs-module.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "lisp.h"
#include "dynlib.h"
#include "coding.h"
#include "verify.h"


/* Feature tests.  */

/* True if __attribute__ ((cleanup (...))) works, false otherwise.  */
#ifdef HAVE_VAR_ATTRIBUTE_CLEANUP
enum { module_has_cleanup = true };
#else
enum { module_has_cleanup = false };
#endif

/* Handle to the main thread.  Used to verify that modules call us in
   the right thread.  */
#ifdef HAVE_THREADS_H
# include <threads.h>
static thrd_t main_thread;
#elif defined HAVE_PTHREAD
# include <pthread.h>
static pthread_t main_thread;
#elif defined WINDOWSNT
#include <windows.h>
#include "w32term.h"
static DWORD main_thread;
#endif


/* Memory management.  */

/* An `emacs_value' is just a pointer to a structure holding an
   internal Lisp object.  */
struct emacs_value_tag { Lisp_Object v; };

/* Local value objects use a simple fixed-sized block allocation
   scheme without explicit deallocation.  All local values are
   deallocated when the lifetime of their environment ends.  Keep
   track of a current frame from which new values are allocated,
   appending further dynamically-allocated frames if necessary.  */

enum { value_frame_size = 512 };

/* A block from which `emacs_value' object can be allocated.  */
struct emacs_value_frame
{
  /* Storage for values.  */
  struct emacs_value_tag objects[value_frame_size];

  /* Index of the next free value in `objects'.  */
  int offset;

  /* Pointer to next frame, if any.  */
  struct emacs_value_frame *next;
};

/* A structure that holds an initial frame (so that the first local
   values require no dynamic allocation) and keeps track of the
   current frame.  */
static struct emacs_value_storage
{
  struct emacs_value_frame initial;
  struct emacs_value_frame *current;
} global_storage;


/* Private runtime and environment members.  */

/* The private part of an environment stores the current non local exit state
   and holds the `emacs_value' objects allocated during the lifetime
   of the environment.  */
struct emacs_env_private
{
  enum emacs_funcall_exit pending_non_local_exit;

  /* Dedicated storage for non-local exit symbol and data so that
     storage is always available for them, even in an out-of-memory
     situation.  */
  struct emacs_value_tag non_local_exit_symbol, non_local_exit_data;

  struct emacs_value_storage storage;
};

/* Combine public and private parts in one structure.  This structure
   is used whenever an environment is created.  */
struct env_storage
{
  emacs_env pub;
  struct emacs_env_private priv;
};

/* The private parts of an `emacs_runtime' object contain the initial
   environment.  */
struct emacs_runtime_private
{
  struct env_storage environment;
};



/* Forward declarations.  */

struct module_fun_env;

static Lisp_Object module_format_fun_env (const struct module_fun_env *);
static Lisp_Object value_to_lisp (emacs_value);
static emacs_value allocate_emacs_value (emacs_env *, struct emacs_value_storage *, Lisp_Object);
static emacs_value lisp_to_value (emacs_env *, Lisp_Object);
static enum emacs_funcall_exit module_non_local_exit_check (emacs_env *);
static void check_main_thread (void);
static void finalize_environment (struct env_storage *);
static void initialize_environment (struct env_storage *);
static void module_args_out_of_range (emacs_env *, Lisp_Object, Lisp_Object);
static void module_handle_signal (emacs_env *, Lisp_Object);
static void module_handle_throw (emacs_env *, Lisp_Object);
static void module_non_local_exit_signal_1 (emacs_env *, Lisp_Object, Lisp_Object);
static void module_non_local_exit_throw_1 (emacs_env *, Lisp_Object, Lisp_Object);
static void module_out_of_memory (emacs_env *);
static void module_reset_handlerlist (const int *);
static void module_wrong_type (emacs_env *, Lisp_Object, Lisp_Object);


/* Convenience macros for non-local exit handling.  */

/* Emacs uses setjmp and longjmp for non-local exits, but
   module frames cannot be skipped because they are in general
   not prepared for long jumps (e.g., the behavior in C++ is undefined
   if objects with nontrivial destructors would be skipped).
   Therefore, catch all non-local exits.  There are two kinds of
   non-local exits: `signal' and `throw'.  The macros in this section
   can be used to catch both.  Use macros to avoid additional variants
   of `internal_condition_case' etc., and to avoid worrying about
   passing information to the handler functions.  */

/* Place this macro at the beginning of a function returning a number
   or a pointer to handle signals.  The function must have an ENV
   parameter.  The function will return 0 (or NULL) if a signal is
   caught.  */
#define MODULE_HANDLE_SIGNALS MODULE_HANDLE_SIGNALS_RETURN (0)

/* Place this macro at the beginning of a function returning void to
   handle signals.  The function must have an ENV parameter.  */
#define MODULE_HANDLE_SIGNALS_VOID MODULE_HANDLE_SIGNALS_RETURN ()

#define MODULE_HANDLE_SIGNALS_RETURN(retval)                                   \
  MODULE_SETJMP (CONDITION_CASE, module_handle_signal, retval)

/* Place this macro at the beginning of a function returning a pointer
   to handle non-local exits via `throw'.  The function must have an
   ENV parameter.  The function will return NULL if a `throw' is
   caught.  */
#define MODULE_HANDLE_THROW                                                    \
  MODULE_SETJMP (CATCHER_ALL, module_handle_throw, NULL)

#define MODULE_SETJMP(handlertype, handlerfunc, retval)			       \
  MODULE_SETJMP_1 (handlertype, handlerfunc, retval,			       \
		   internal_handler_##handlertype,			       \
		   internal_cleanup_##handlertype)

/* It is very important that pushing the handler doesn't itself raise
   a signal.  Install the cleanup only after the handler has been
   pushed.  Use __attribute__ ((cleanup)) to avoid
   non-local-exit-prone manual cleanup.

   The do-while forces uses of the macro to be followed by a semicolon.
   This macro cannot enclose its entire body inside a do-while, as the
   code after the macro may longjmp back into the macro, which means
   its local variable C must stay live in later code.  */

#define MODULE_SETJMP_1(handlertype, handlerfunc, retval, c, dummy)	\
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return); \
  struct handler *c = push_handler_nosignal (Qt, handlertype);		\
  if (!c)								\
    {									\
      module_out_of_memory (env);					\
      return retval;							\
    }									\
  verify (module_has_cleanup);						\
  int dummy __attribute__ ((cleanup (module_reset_handlerlist)));	\
  if (sys_setjmp (c->jmp))						\
    {									\
      (handlerfunc) (env, c->val);					\
      return retval;							\
    }									\
  do { } while (false)


/* Function environments.  */

/* A function environment is an auxiliary structure used by
   `module_make_function' to store information about a module
   function.  It is stored in a save pointer and retrieved by
   `module-call'.  Its members correspond to the arguments given to
   `module_make_function'.  */

struct module_fun_env
{
  ptrdiff_t min_arity, max_arity;
  emacs_subr subr;
  void *data;
};

/* The function definition of `module-call'.  `module-call' is
   uninterned because user code couldn't meaningfully use it, so keep
   its definition around somewhere else.  */
static Lisp_Object module_call_func;


/* Implementation of runtime and environment functions.  */

/* Catch signals and throws only if the code can actually signal or
   throw.  If checking is enabled, abort if the current thread is not
   the Emacs main thread.  */

static emacs_env *
module_get_environment (struct emacs_runtime *ert)
{
  check_main_thread ();
  return &ert->private_members->environment.pub;
}

/* To make global refs (GC-protected global values) keep a hash that
   maps global Lisp objects to reference counts.  */

static emacs_value
module_make_global_ref (emacs_env *env, emacs_value ref)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  eassert (HASH_TABLE_P (Vmodule_refs_hash));
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object new_obj = value_to_lisp (ref);
  EMACS_UINT hashcode;
  ptrdiff_t i = hash_lookup (h, new_obj, &hashcode);

  if (i >= 0)
    {
      Lisp_Object value = HASH_VALUE (h, i);
      eassert (NATNUMP (value));
      EMACS_INT refcount = XFASTINT (value) + 1;
      if (refcount > MOST_POSITIVE_FIXNUM)
        {
          module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
          return NULL;
        }
      value = make_natnum (refcount);
      set_hash_value_slot (h, i, value);
    }
  else
    {
      hash_put (h, new_obj, make_natnum (1), hashcode);
    }

  return allocate_emacs_value (env, &global_storage, new_obj);
}

static void
module_free_global_ref (emacs_env *env, emacs_value ref)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  /* TODO: This probably never signals.  */
  /* FIXME: Wait a minute.  Shouldn't this function report an error if
     the hash lookup fails?  */
  MODULE_HANDLE_SIGNALS_VOID;
  eassert (HASH_TABLE_P (Vmodule_refs_hash));
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object obj = value_to_lisp (ref);
  EMACS_UINT hashcode;
  ptrdiff_t i = hash_lookup (h, obj, &hashcode);

  if (i >= 0)
    {
      Lisp_Object value = HASH_VALUE (h, i);
      eassert (NATNUMP (value));
      EMACS_INT refcount = XFASTINT (value) - 1;
      if (refcount > 0)
        {
          value = make_natnum (refcount);
          set_hash_value_slot (h, i, value);
        }
      else
        {
          eassert (refcount == 0);
          hash_remove_from_table (h, value);
        }
    }
}

static enum emacs_funcall_exit
module_non_local_exit_check (emacs_env *env)
{
  check_main_thread ();
  return env->private_members->pending_non_local_exit;
}

static void
module_non_local_exit_clear (emacs_env *env)
{
  check_main_thread ();
  env->private_members->pending_non_local_exit = emacs_funcall_exit_return;
}

static enum emacs_funcall_exit
module_non_local_exit_get (emacs_env *env, emacs_value *sym, emacs_value *data)
{
  check_main_thread ();
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return)
    {
      *sym = &p->non_local_exit_symbol;
      *data = &p->non_local_exit_data;
    }
  return p->pending_non_local_exit;
}

/* Like for `signal', DATA must be a list.  */
static void
module_non_local_exit_signal (emacs_env *env, emacs_value sym, emacs_value data)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  module_non_local_exit_signal_1 (env, value_to_lisp (sym),
				  value_to_lisp (data));
}

static void
module_non_local_exit_throw (emacs_env *env, emacs_value tag, emacs_value value)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  module_non_local_exit_throw_1 (env, value_to_lisp (tag),
				 value_to_lisp (value));
}

/* A module function is lambda function that calls `module-call',
   passing the function pointer of the module function along with the
   module emacs_env pointer as arguments.

	(function (lambda (&rest arglist)
		    (module-call envobj arglist)))  */

static emacs_value
module_make_function (emacs_env *env, ptrdiff_t min_arity, ptrdiff_t max_arity,
		      emacs_subr subr, const char *documentation,
		      void *data)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;

  if (! (0 <= min_arity
	 && (max_arity < 0
	     ? max_arity == emacs_variadic_function
	     : min_arity <= max_arity)))
    xsignal2 (Qinvalid_arity, make_number (min_arity), make_number (max_arity));

  /* FIXME: This should be freed when envobj is GC'd.  */
  struct module_fun_env *envptr = xmalloc (sizeof *envptr);
  envptr->min_arity = min_arity;
  envptr->max_arity = max_arity;
  envptr->subr = subr;
  envptr->data = data;

  Lisp_Object envobj = make_save_ptr (envptr);
  Lisp_Object ret = list4 (Qlambda,
                           list2 (Qand_rest, Qargs),
                           documentation ? build_string (documentation) : Qnil,
                           list3 (module_call_func,
                                  envobj,
                                  Qargs));

  return lisp_to_value (env, ret);
}

static emacs_value
module_funcall (emacs_env *env, emacs_value fun, ptrdiff_t nargs,
		emacs_value args[])
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  MODULE_HANDLE_THROW;

  /* Make a new Lisp_Object array starting with the function as the
     first arg, because that's what Ffuncall takes.  */
  Lisp_Object *newargs;
  USE_SAFE_ALLOCA;
  SAFE_ALLOCA_LISP (newargs, nargs + 1);
  newargs[0] = value_to_lisp (fun);
  for (ptrdiff_t i = 0; i < nargs; i++)
    newargs[1 + i] = value_to_lisp (args[i]);
  emacs_value result = lisp_to_value (env, Ffuncall (nargs + 1, newargs));
  SAFE_FREE ();
  return result;
}

static emacs_value
module_intern (emacs_env *env, const char *name)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  return lisp_to_value (env, intern (name));
}

static emacs_value
module_type_of (emacs_env *env, emacs_value value)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  return lisp_to_value (env, Ftype_of (value_to_lisp (value)));
}

static bool
module_is_not_nil (emacs_env *env, emacs_value value)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  return ! NILP (value_to_lisp (value));
}

static bool
module_eq (emacs_env *env, emacs_value a, emacs_value b)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  return EQ (value_to_lisp (a), value_to_lisp (b));
}

static intmax_t
module_extract_integer (emacs_env *env, emacs_value n)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object l = value_to_lisp (n);
  if (! INTEGERP (l))
    {
      module_wrong_type (env, Qintegerp, l);
      return 0;
    }
  return XINT (l);
}

static emacs_value
module_make_integer (emacs_env *env, intmax_t n)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  if (! (MOST_NEGATIVE_FIXNUM <= n && n <= MOST_POSITIVE_FIXNUM))
    {
      module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return NULL;
    }
  return lisp_to_value (env, make_number (n));
}

static double
module_extract_float (emacs_env *env, emacs_value f)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lisp = value_to_lisp (f);
  if (! FLOATP (lisp))
    {
      module_wrong_type (env, Qfloatp, lisp);
      return 0;
    }
  return XFLOAT_DATA (lisp);
}

static emacs_value
module_make_float (emacs_env *env, double d)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  return lisp_to_value (env, make_float (d));
}

static bool
module_copy_string_contents (emacs_env *env, emacs_value value, char *buffer,
			     ptrdiff_t *length)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  Lisp_Object lisp_str = value_to_lisp (value);
  if (! STRINGP (lisp_str))
    {
      module_wrong_type (env, Qstringp, lisp_str);
      return false;
    }

  ptrdiff_t raw_size = SBYTES (lisp_str);

  /* Emacs internal encoding is more-or-less UTF8, let's assume utf8
     encoded emacs string are the same byte size.  */

  if (!buffer || length == 0 || *length-1 < raw_size)
    {
      *length = raw_size + 1;
      return false;
    }

  Lisp_Object lisp_str_utf8 = ENCODE_UTF_8 (lisp_str);
  eassert (raw_size == SBYTES (lisp_str_utf8));
  *length = raw_size + 1;
  memcpy (buffer, SDATA (lisp_str_utf8), SBYTES (lisp_str_utf8));
  buffer[raw_size] = 0;

  return true;
}

static emacs_value
module_make_string (emacs_env *env, const char *str, ptrdiff_t length)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  if (length > PTRDIFF_MAX)
    {
      module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return NULL;
    }
  /* Assume STR is utf8 encoded.  */
  return lisp_to_value (env, make_string (str, length));
}

static emacs_value
module_make_user_ptr (emacs_env *env, emacs_finalizer_function fin, void *ptr)
{
  check_main_thread ();
  return lisp_to_value (env, make_user_ptr (fin, ptr));
}

static void *
module_get_user_ptr (emacs_env *env, emacs_value uptr)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp))
    {
      module_wrong_type (env, Quser_ptr, lisp);
      return NULL;
    }
  return XUSER_PTR (lisp)->p;
}

static void
module_set_user_ptr (emacs_env *env, emacs_value uptr, void *ptr)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp))
    module_wrong_type (env, Quser_ptr, lisp);
  XUSER_PTR (lisp)->p = ptr;
}

static emacs_finalizer_function
module_get_user_finalizer (emacs_env *env, emacs_value uptr)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp))
    {
      module_wrong_type (env, Quser_ptr, lisp);
      return NULL;
    }
  return XUSER_PTR (lisp)->finalizer;
}

static void
module_set_user_finalizer (emacs_env *env, emacs_value uptr,
			   emacs_finalizer_function fin)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp))
    module_wrong_type (env, Quser_ptr, lisp);
  XUSER_PTR (lisp)->finalizer = fin;
}

static void
module_vec_set (emacs_env *env, emacs_value vec, ptrdiff_t i, emacs_value val)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lvec = value_to_lisp (vec);
  if (! VECTORP (lvec))
    {
      module_wrong_type (env, Qvectorp, lvec);
      return;
    }
  if (! (0 <= i && i < ASIZE (lvec)))
    {
      if (MOST_NEGATIVE_FIXNUM <= i && i <= MOST_POSITIVE_FIXNUM)
	module_args_out_of_range (env, lvec, make_number (i));
      else
	module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return;
    }
  ASET (lvec, i, value_to_lisp (val));
}

static emacs_value
module_vec_get (emacs_env *env, emacs_value vec, ptrdiff_t i)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lvec = value_to_lisp (vec);
  if (! VECTORP (lvec))
    {
      module_wrong_type (env, Qvectorp, lvec);
      return NULL;
    }
  if (! (0 <= i && i < ASIZE (lvec)))
    {
      if (MOST_NEGATIVE_FIXNUM <= i && i <= MOST_POSITIVE_FIXNUM)
	module_args_out_of_range (env, lvec, make_number (i));
      else
	module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return NULL;
    }
  return lisp_to_value (env, AREF (lvec, i));
}

static ptrdiff_t
module_vec_size (emacs_env *env, emacs_value vec)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  Lisp_Object lvec = value_to_lisp (vec);
  if (! VECTORP (lvec))
    {
      module_wrong_type (env, Qvectorp, lvec);
      return 0;
    }
  eassert (ASIZE (lvec) >= 0);
  return ASIZE (lvec);
}


/* Subroutines.  */

DEFUN ("module-load", Fmodule_load, Smodule_load, 1, 1, 0,
       doc: /* Load module FILE.  */)
  (Lisp_Object file)
{
  dynlib_handle_ptr handle;
  emacs_init_function module_init;
  void *gpl_sym;

  CHECK_STRING (file);
  handle = dynlib_open (SSDATA (file));
  if (!handle)
    error ("Cannot load file %s: %s", SDATA (file), dynlib_error ());

  gpl_sym = dynlib_sym (handle, "plugin_is_GPL_compatible");
  if (!gpl_sym)
    error ("Module %s is not GPL compatible", SDATA (file));

  module_init = (emacs_init_function) dynlib_sym (handle, "emacs_module_init");
  if (!module_init)
    error ("Module %s does not have an init function.", SDATA (file));

  struct emacs_runtime_private priv;
  struct emacs_runtime pub =
    {
      .size = sizeof pub,
      .private_members = &priv,
      .get_environment = module_get_environment
    };
  initialize_environment (&priv.environment);
  int r = module_init (&pub);
  finalize_environment (&priv.environment);

  if (r != 0)
    {
      if (! (MOST_NEGATIVE_FIXNUM <= r && r <= MOST_POSITIVE_FIXNUM))
        xsignal0 (Qoverflow_error);
      xsignal2 (Qmodule_load_failed, file, make_number (r));
    }

  return Qt;
}

DEFUN ("module-call", Fmodule_call, Smodule_call, 2, 2, 0,
       doc: /* Internal function to call a module function.
ENVOBJ is a save pointer to a module_fun_env structure.
ARGLIST is a list of arguments passed to SUBRPTR.  */)
  (Lisp_Object envobj, Lisp_Object arglist)
{
  struct module_fun_env *envptr = XSAVE_POINTER (envobj, 0);
  EMACS_INT len = XFASTINT (Flength (arglist));
  eassume (0 <= envptr->min_arity);
  if (! (envptr->min_arity <= len
	 && len <= (envptr->max_arity < 0 ? PTRDIFF_MAX : envptr->max_arity)))
    xsignal2 (Qwrong_number_of_arguments, module_format_fun_env (envptr),
	      make_number (len));

  struct env_storage env;
  initialize_environment (&env);

  emacs_value *args = xnmalloc (len, sizeof *args);

  for (ptrdiff_t i = 0; i < len; i++)
    {
      args[i] = lisp_to_value (&env.pub, XCAR (arglist));
      if (! args[i])
	memory_full (sizeof *args[i]);
      arglist = XCDR (arglist);
    }

  emacs_value ret = envptr->subr (&env.pub, len, args, envptr->data);
  xfree (args);

  switch (env.priv.pending_non_local_exit)
    {
    case emacs_funcall_exit_return:
      finalize_environment (&env);
      if (ret == NULL)
	xsignal1 (Qinvalid_module_call, module_format_fun_env (envptr));
      return value_to_lisp (ret);
    case emacs_funcall_exit_signal:
      {
        Lisp_Object symbol = value_to_lisp (&env.priv.non_local_exit_symbol);
        Lisp_Object data = value_to_lisp (&env.priv.non_local_exit_data);
        finalize_environment (&env);
        xsignal (symbol, data);
      }
    case emacs_funcall_exit_throw:
      {
        Lisp_Object tag = value_to_lisp (&env.priv.non_local_exit_symbol);
        Lisp_Object value = value_to_lisp (&env.priv.non_local_exit_data);
        finalize_environment (&env);
        Fthrow (tag, value);
      }
    default:
      eassume (false);
    }
}


/* Helper functions.  */

static void
check_main_thread (void)
{
#ifdef HAVE_THREADS_H
  eassert (thrd_equal (thdr_current (), main_thread));
#elif defined HAVE_PTHREAD
  eassert (pthread_equal (pthread_self (), main_thread));
#elif defined WINDOWSNT
  eassert (GetCurrentThreadId () == main_thread);
#endif
}

static void
module_non_local_exit_signal_1 (emacs_env *env, Lisp_Object sym,
				Lisp_Object data)
{
  struct emacs_env_private *p = env->private_members;
  eassert (p->pending_non_local_exit == emacs_funcall_exit_return);
  p->pending_non_local_exit = emacs_funcall_exit_signal;
  p->non_local_exit_symbol.v = sym;
  p->non_local_exit_data.v = data;
}

static void
module_non_local_exit_throw_1 (emacs_env *env, Lisp_Object tag,
			       Lisp_Object value)
{
  struct emacs_env_private *p = env->private_members;
  eassert (p->pending_non_local_exit == emacs_funcall_exit_return);
  p->pending_non_local_exit = emacs_funcall_exit_throw;
  p->non_local_exit_symbol.v = tag;
  p->non_local_exit_data.v = value;
}

/* Module version of `wrong_type_argument'.  */
static void
module_wrong_type (emacs_env *env, Lisp_Object predicate, Lisp_Object value)
{
  module_non_local_exit_signal_1 (env, Qwrong_type_argument,
				  list2 (predicate, value));
}

/* Signal an out-of-memory condition to the caller.  */
static void
module_out_of_memory (emacs_env *env)
{
  /* TODO: Reimplement this so it works even if memory-signal-data has
     been modified.  */
  module_non_local_exit_signal_1 (env, XCAR (Vmemory_signal_data),
				  XCDR (Vmemory_signal_data));
}

/* Signal arguments are out of range.  */
static void
module_args_out_of_range (emacs_env *env, Lisp_Object a1, Lisp_Object a2)
{
  module_non_local_exit_signal_1 (env, Qargs_out_of_range, list2 (a1, a2));
}


/* Value conversion.  */

/* Convert an `emacs_value' to the corresponding internal object.
   Never fails.  */
static Lisp_Object
value_to_lisp (emacs_value v)
{
  return v->v;
}

/* Convert an internal object to an `emacs_value'.  Allocate storage
   from the environment; return NULL if allocation fails.  */
static emacs_value
lisp_to_value (emacs_env *env, Lisp_Object o)
{
  struct emacs_env_private *p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return)
    return NULL;
  return allocate_emacs_value (env, &p->storage, o);
}


/* Memory management.  */

/* Must be called for each frame before it can be used for allocation.  */
static void
initialize_frame (struct emacs_value_frame *frame)
{
  frame->offset = 0;
  frame->next = NULL;
}

/* Must be called for any storage object before it can be used for
   allocation.  */
static void
initialize_storage (struct emacs_value_storage *storage)
{
  initialize_frame (&storage->initial);
  storage->current = &storage->initial;
}

/* Must be called for any initialized storage object before its
   lifetime ends.  Free all dynamically-allocated frames.  */
static void
finalize_storage (struct emacs_value_storage *storage)
{
  struct emacs_value_frame *next = storage->initial.next;
  while (next != NULL)
    {
      struct emacs_value_frame *current = next;
      next = current->next;
      free (current);
    }
}

/* Allocate a new value from STORAGE and stores OBJ in it.  Return
   NULL if allocations fails and use ENV for non local exit reporting.  */
static emacs_value
allocate_emacs_value (emacs_env *env, struct emacs_value_storage *storage,
		      Lisp_Object obj)
{
  eassert (storage->current);
  eassert (storage->current->offset < value_frame_size);
  eassert (! storage->current->next);
  if (storage->current->offset == value_frame_size - 1)
    {
      storage->current->next = malloc (sizeof *storage->current->next);
      if (! storage->current->next)
        {
          module_out_of_memory (env);
          return NULL;
        }
      initialize_frame (storage->current->next);
      storage->current = storage->current->next;
    }
  emacs_value value = storage->current->objects + storage->current->offset;
  value->v = obj;
  ++storage->current->offset;
  return value;
}

/* Mark all objects allocated from local environments so that they
   don't get garbage-collected.  */
void mark_modules (void)
{
  for (Lisp_Object tem = Vmodule_environments; CONSP (tem); tem = XCDR (tem))
    {
      struct env_storage *env = XSAVE_POINTER (tem, 0);
      for (struct emacs_value_frame *frame = &env->priv.storage.initial;
	   frame != NULL;
	   frame = frame->next)
        for (int i = 0; i < frame->offset; ++i)
          mark_object (frame->objects[i].v);
    }
}


/* Environment lifetime management.  */

/* Must be called before the environment can be used.  */
static void
initialize_environment (struct env_storage *env)
{
  env->priv.pending_non_local_exit = emacs_funcall_exit_return;
  initialize_storage (&env->priv.storage);
  env->pub.size = sizeof env->pub;
  env->pub.private_members = &env->priv;
  env->pub.make_global_ref = module_make_global_ref;
  env->pub.free_global_ref = module_free_global_ref;
  env->pub.non_local_exit_check = module_non_local_exit_check;
  env->pub.non_local_exit_clear = module_non_local_exit_clear;
  env->pub.non_local_exit_get = module_non_local_exit_get;
  env->pub.non_local_exit_signal = module_non_local_exit_signal;
  env->pub.non_local_exit_throw = module_non_local_exit_throw;
  env->pub.make_function = module_make_function;
  env->pub.funcall = module_funcall;
  env->pub.intern = module_intern;
  env->pub.type_of = module_type_of;
  env->pub.is_not_nil = module_is_not_nil;
  env->pub.eq = module_eq;
  env->pub.extract_integer = module_extract_integer;
  env->pub.make_integer = module_make_integer;
  env->pub.extract_float = module_extract_float;
  env->pub.make_float = module_make_float;
  env->pub.copy_string_contents = module_copy_string_contents;
  env->pub.make_string = module_make_string;
  env->pub.make_user_ptr = module_make_user_ptr;
  env->pub.get_user_ptr = module_get_user_ptr;
  env->pub.set_user_ptr = module_set_user_ptr;
  env->pub.get_user_finalizer = module_get_user_finalizer;
  env->pub.set_user_finalizer = module_set_user_finalizer;
  env->pub.vec_set = module_vec_set;
  env->pub.vec_get = module_vec_get;
  env->pub.vec_size = module_vec_size;
  Vmodule_environments = Fcons (make_save_ptr (env), Vmodule_environments);
}

/* Must be called before the lifetime of the environment object
   ends.  */
static void
finalize_environment (struct env_storage *env)
{
  finalize_storage (&env->priv.storage);
  Vmodule_environments = XCDR (Vmodule_environments);
}


/* Non-local exit handling.  */

/* Must be called after setting up a handler immediately before
   returning from the function.  See the comments in lisp.h and the
   code in eval.c for details.  The macros below arrange for this
   function to be called automatically.  DUMMY is ignored.  */
static void
module_reset_handlerlist (const int *dummy)
{
  handlerlist = handlerlist->next;
}

/* Called on `signal'.  ERR is a pair (SYMBOL . DATA), which gets
   stored in the environment.  Set the pending non-local exit flag.  */
static void
module_handle_signal (emacs_env *env, Lisp_Object err)
{
  module_non_local_exit_signal_1 (env, XCAR (err), XCDR (err));
}

/* Called on `throw'.  TAG_VAL is a pair (TAG . VALUE), which gets
   stored in the environment.  Set the pending non-local exit flag.  */
static void
module_handle_throw (emacs_env *env, Lisp_Object tag_val)
{
  module_non_local_exit_throw_1 (env, XCAR (tag_val), XCDR (tag_val));
}


/* Function environments.  */

/* Return a string object that contains a user-friendly
   representation of the function environment.  */
static Lisp_Object
module_format_fun_env (const struct module_fun_env *env)
{
  /* Try to print a function name if possible.  */
  const char *path, *sym;
  if (dynlib_addr (env->subr, &path, &sym))
    {
      static char const format[] = "#<module function %s from %s>";
      int size = snprintf (NULL, 0, format, sym, path);
      eassert (size > 0);
      char buffer[size + 1];
      snprintf (buffer, sizeof buffer, format, sym, path);
      return make_unibyte_string (buffer, size);
    }
  else
    {
      static char const format[] = "#<module function at %p>";
      void *subr = env->subr;
      int size = snprintf (NULL, 0, format, subr);
      eassert (size > 0);
      char buffer[size + 1];
      snprintf (buffer, sizeof buffer, format, subr);
      return make_unibyte_string (buffer, size);
    }
}


/* Segment initializer.  */

void
syms_of_module (void)
{
  DEFSYM (Qmodule_refs_hash, "module-refs-hash");
  DEFVAR_LISP ("module-refs-hash", Vmodule_refs_hash,
	       doc: /* Module global referrence table.  */);

  Vmodule_refs_hash
    = make_hash_table (hashtest_eq, make_number (DEFAULT_HASH_SIZE),
		       make_float (DEFAULT_REHASH_SIZE),
		       make_float (DEFAULT_REHASH_THRESHOLD),
		       Qnil);
  Funintern (Qmodule_refs_hash, Qnil);

  DEFSYM (Qmodule_environments, "module-environments");
  DEFVAR_LISP ("module-environments", Vmodule_environments,
               doc: /* List of active module environments.  */);
  Vmodule_environments = Qnil;
  /* Unintern `module-environments' because it is only used
     internally.  */
  Funintern (Qmodule_environments, Qnil);

  DEFSYM (Qmodule_load_failed, "module-load-failed");
  Fput (Qmodule_load_failed, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qmodule_load_failed, Qerror));
  Fput (Qmodule_load_failed, Qerror_message,
        build_pure_c_string ("Module load failed"));

  DEFSYM (Qinvalid_module_call, "invalid-module-call");
  Fput (Qinvalid_module_call, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qinvalid_module_call, Qerror));
  Fput (Qinvalid_module_call, Qerror_message,
        build_pure_c_string ("Invalid module call"));

  DEFSYM (Qinvalid_arity, "invalid-arity");
  Fput (Qinvalid_arity, Qerror_conditions,
        listn (CONSTYPE_PURE, 2, Qinvalid_arity, Qerror));
  Fput (Qinvalid_arity, Qerror_message,
        build_pure_c_string ("Invalid function arity"));

  initialize_storage (&global_storage);

  /* Unintern `module-refs-hash' because it is internal-only and Lisp
     code or modules should not access it.  */
  Funintern (Qmodule_refs_hash, Qnil);

  defsubr (&Smodule_load);

  /* Don't call defsubr on `module-call' because that would intern it,
     but `module-call' is an internal function that users cannot
     meaningfully use.  Instead, assign its definition to a private
     variable.  */
  XSETPVECTYPE (&Smodule_call, PVEC_SUBR);
  XSETSUBR (module_call_func, &Smodule_call);
}

/* Unlike syms_of_module, this initializer is called even from an
   initialized (dumped) Emacs.  */

void
module_init (void)
{
  /* It is not guaranteed that dynamic initializers run in the main thread,
     therefore detect the main thread here.  */
#ifdef HAVE_THREADS_H
  main_thread = thrd_current ();
#elif defined HAVE_PTHREAD
  main_thread = pthread_self ();
#elif defined WINDOWSNT
  /* The 'main' function already recorded the main thread's thread ID,
     so we need just to use it . */
  main_thread = dwMainThreadId;
#endif
}
