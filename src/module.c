/*
  module.c - Module loading and runtime implementation
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
  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include <config.h>
#include "lisp.h"
#include "emacs_module.h"
#include "dynlib.h"
#include "coding.h"
#include "verify.h"


/* Feature tests */

enum {
  /* 1 if we have __attribute__((cleanup(...))), 0 otherwise */
  module_has_cleanup =
#ifdef HAVE_VAR_ATTRIBUTE_CLEANUP
  1
#else
  0
#endif
};

/* Handle to the main thread.  Used to verify that modules call us in
   the right thread. */
#if defined(HAVE_THREADS_H)
#include <threads.h>
static thrd_t main_thread;
#elif defined(HAVE_PTHREAD)
#include <pthread.h>
static pthread_t main_thread;
#elif defined(WINDOWSNT)
#include <windows.h>
/* On Windows, we store a handle to the main thread instead of the
   thread ID because the latter can be reused when a thread terminates. */
static HANDLE main_thread;
#endif


/* Implementation of runtime and environment functions */

static emacs_env* module_get_environment (struct emacs_runtime *ert);

static emacs_value module_make_global_ref (emacs_env *env,
                                           emacs_value ref);
static void module_free_global_ref (emacs_env *env,
                                    emacs_value ref);
static enum emacs_funcall_exit module_non_local_exit_check (emacs_env *env);
static void module_non_local_exit_clear (emacs_env *env);
static enum emacs_funcall_exit module_non_local_exit_get (emacs_env *env, emacs_value *sym, emacs_value *data);
static void module_non_local_exit_signal (emacs_env *env, emacs_value sym, emacs_value data);
static void module_non_local_exit_throw (emacs_env *env, emacs_value tag, emacs_value value);
static emacs_value module_make_function (emacs_env *env,
                                         int min_arity,
                                         int max_arity,
                                         emacs_subr subr,
                                         const char *documentation,
                                         void *data);
static emacs_value module_funcall (emacs_env *env,
                                   emacs_value fun,
                                   int nargs,
                                   emacs_value args[]);
static emacs_value module_intern (emacs_env *env, const char *name);
static emacs_value module_type_of (emacs_env *env, emacs_value value);
static bool module_is_not_nil (emacs_env *env, emacs_value value);
static bool module_eq (emacs_env *env, emacs_value a, emacs_value b);
static int64_t module_extract_integer (emacs_env *env, emacs_value n);
static emacs_value module_make_integer (emacs_env *env, int64_t n);
static emacs_value module_make_float (emacs_env *env, double d);
static double module_extract_float (emacs_env *env, emacs_value f);
static bool module_copy_string_contents (emacs_env *env,
                                         emacs_value value,
                                         char *buffer,
                                         size_t* length);
static emacs_value module_make_string (emacs_env *env, const char *str, size_t lenght);
static emacs_value module_make_user_ptr (emacs_env *env,
                                         emacs_finalizer_function fin,
                                         void *ptr);
static void* module_get_user_ptr (emacs_env *env, emacs_value uptr);
static void module_set_user_ptr (emacs_env *env, emacs_value uptr, void *ptr);
static emacs_finalizer_function module_get_user_finalizer (emacs_env *env, emacs_value uptr);
static void module_set_user_finalizer (emacs_env *env,
                                       emacs_value uptr,
                                       emacs_finalizer_function fin);


/* Helper functions */

/* If checking is enabled, abort if the current thread is not the
   Emacs main thread. */
static void check_main_thread (void);

/* Internal versions of `module_non_local_exit_signal' and `module_non_local_exit_throw'. */
static void module_non_local_exit_signal_1 (emacs_env *env, Lisp_Object sym, Lisp_Object data);
static void module_non_local_exit_throw_1 (emacs_env *env, Lisp_Object tag, Lisp_Object value);

/* Module version of `wrong_type_argument'. */
static void module_wrong_type (emacs_env *env, Lisp_Object predicate, Lisp_Object value);

/* Signal an out-of-memory condition to the caller. */
static void module_out_of_memory (emacs_env *env);

/* Signal arguments are out of range. */
static void module_args_out_of_range (emacs_env *env, Lisp_Object a1, Lisp_Object a2);


/* Value conversion */

/* Converts an `emacs_value' to the corresponding internal object.
   Never fails. */
static Lisp_Object value_to_lisp (emacs_value v);

/* Converts an internal object to an `emacs_value'.  Allocates storage
   from the environment; returns NULL if allocation fails. */
static emacs_value lisp_to_value (emacs_env *env, Lisp_Object o);


/* Memory management */

/* An `emacs_value' is just a pointer to a structure holding an
   internal Lisp object. */
struct emacs_value_tag { Lisp_Object v; };

/* Local value objects use a simple fixed-sized block allocation
   scheme without explicit deallocation.  All local values are
   deallocated when the lifetime of their environment ends.  We keep
   track of a current frame from which new values are allocated,
   appending further dynamically-allocated frames if necessary. */

enum { value_frame_size = 512 };

/* A block from which `emacs_value' object can be allocated. */
struct emacs_value_frame {
  /* Storage for values */
  struct emacs_value_tag objects[value_frame_size];

  /* Index of the next free value in `objects' */
  size_t offset;

  /* Pointer to next frame, if any */
  struct emacs_value_frame *next;
};

/* Must be called for each frame before it can be used for
   allocation. */
static void initialize_frame (struct emacs_value_frame *frame);

/* A structure that holds an initial frame (so that the first local
   values require no dynamic allocation) and keeps track of the
   current frame. */
static struct emacs_value_storage {
  struct emacs_value_frame initial;
  struct emacs_value_frame *current;
} global_storage;

/* Must be called for any storage object before it can be used for
   allocation. */
static void initialize_storage (struct emacs_value_storage *storage);

/* Must be called for any initialized storage object before its
   lifetime ends.  Frees all dynamically-allocated frames. */
static void finalize_storage (struct emacs_value_storage *storage);

/* Allocates a new value from STORAGE and stores OBJ in it.  Returns
   NULL if allocations fails and uses ENV for non local exit reporting. */
static emacs_value allocate_emacs_value (emacs_env *env, struct emacs_value_storage *storage,
                                         Lisp_Object obj);


/* Private runtime and environment members */

/* The private part of an environment stores the current non local exit state
   and holds the `emacs_value' objects allocated during the lifetime
   of the environment. */
struct emacs_env_private {
  enum emacs_funcall_exit pending_non_local_exit;

  /* Dedicated storage for non-local exit symbol and data so that we always
     have storage available for them, even in an out-of-memory
     situation. */
  struct emacs_value_tag non_local_exit_symbol, non_local_exit_data;

  struct emacs_value_storage storage;
};

/* Combines public and private parts in one structure.  This structure
   is used whenever an environment is created. */
struct env_storage {
  emacs_env pub;
  struct emacs_env_private priv;
};

/* Must be called before the environment can be used. */
static void initialize_environment (struct env_storage *env);

/* Must be called before the lifetime of the environment object
   ends. */
static void finalize_environment (struct env_storage *env);

/* The private parts of an `emacs_runtime' object contain the initial
   environment. */
struct emacs_runtime_private {
  struct env_storage environment;
};


/* Convenience macros for non-local exit handling */

/* Emacs uses setjmp(3) and longjmp(3) for non-local exits, but we
   can't allow module frames to be skipped because they are in general
   not prepared for long jumps (e.g. the behavior in C++ is undefined
   if objects with nontrivial destructors would be skipped).
   Therefore we catch all non-local exits.  There are two kinds of
   non-local exits: `signal' and `throw'.  The macros in this section
   can be used to catch both.  We use macros so that we don't have to
   write lots of additional variants of `internal_condition_case'
   etc. and don't have to worry about passing information to the
   handler functions. */

/* Called on `signal'.  ERR will be a cons cell (SYMBOL . DATA), which
   gets stored in the environment.  Sets the pending non-local exit flag. */
static void module_handle_signal (emacs_env *env, Lisp_Object err);

/* Called on `throw'.  TAG_VAL will be a cons cell (TAG . VALUE),
   which gets stored in the environment.  Sets the pending non-local exit
   flag. */
static void module_handle_throw (emacs_env *env, Lisp_Object tag_val);

/* Must be called after setting up a handler immediately before
   returning from the function.  See the comments in lisp.h and the
   code in eval.c for details.  The macros below arrange for this
   function to be called automatically.  DUMMY is ignored. */
static void module_reset_handlerlist (const int *dummy);

/* Place this macro at the beginning of a function returning a number
   or a pointer to handle signals.  The function must have an ENV
   parameter.  The function will return 0 (or NULL) if a signal is
   caught. */
#define MODULE_HANDLE_SIGNALS MODULE_HANDLE_SIGNALS_RETURN(0)

/* Place this macro at the beginning of a function returning void to
   handle signals.  The function must have an ENV parameter. */
#define MODULE_HANDLE_SIGNALS_VOID MODULE_HANDLE_SIGNALS_RETURN()

#define MODULE_HANDLE_SIGNALS_RETURN(retval)                                   \
  MODULE_SETJMP(CONDITION_CASE, module_handle_signal, retval)

/* Place this macro at the beginning of a function returning a pointer
   to handle non-local exits via `throw'.  The function must have an
   ENV parameter.  The function will return NULL if a `throw' is
   caught. */
#define MODULE_HANDLE_THROW                                                    \
  MODULE_SETJMP(CATCHER_ALL, module_handle_throw, NULL)

#define MODULE_SETJMP(handlertype, handlerfunc, retval)                        \
  MODULE_SETJMP_1(handlertype, handlerfunc, retval,                            \
                  internal_handler_##handlertype,                              \
                  internal_cleanup_##handlertype)

#define MODULE_SETJMP_1(handlertype, handlerfunc, retval, c, dummy)            \
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);    \
  struct handler *c;                                                           \
  /* It is very important that pushing the handler doesn't itself raise a      \
     signal. */                                                                \
  if (!push_handler_nosignal(&c, Qt, handlertype)) {                           \
    module_out_of_memory(env);                                                 \
    return retval;                                                             \
  }                                                                            \
  verify(module_has_cleanup);                                                  \
  /* We can install the cleanup only after the handler has been pushed.  Use   \
     __attribute__((cleanup)) to avoid non-local-exit-prone manual cleanup. */ \
  const int dummy __attribute__((cleanup(module_reset_handlerlist)));          \
  if (sys_setjmp(c->jmp)) {                                                    \
    (handlerfunc)(env, c->val);                                                \
    return retval;                                                             \
  }                                                                            \
  /* Force the macro to be followed by a semicolon. */                         \
  do {                                                                         \
  } while (0)


/* Function environments */

/* A function environment is an auxiliary structure used by
   `module_make_function' to store information about a module
   function.  It is stored in a save pointer and retrieved by
   `module-call'.  Its members correspond to the arguments given to
   `module_make_function'. */

struct module_fun_env
{
  int min_arity, max_arity;
  emacs_subr subr;
  void *data;
};

/* Returns a string object that contains a user-friendly
   representation of the function environment. */
static Lisp_Object module_format_fun_env (const struct module_fun_env *env);

/* Holds the function definition of `module-call'.  `module-call' is
   uninterned because user code couldn't meaningfully use it, so we
   have to keep its definition around somewhere else. */
static Lisp_Object module_call_func;


/* Implementation of runtime and environment functions */

/* We catch signals and throws only if the code can actually signal or
   throw. */

static emacs_env* module_get_environment (struct emacs_runtime *ert)
{
  check_main_thread ();
  return &ert->private_members->environment.pub;
}

/*
 * To make global refs (GC-protected global values) we keep a hash
 * that maps global Lisp objects to reference counts.
 */

static emacs_value module_make_global_ref (emacs_env *env,
                                           emacs_value ref)
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
      const EMACS_UINT refcount = XFASTINT (value);
      if (refcount >= MOST_POSITIVE_FIXNUM)
        {
          module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
          return NULL;
        }
      XSETFASTINT (value, refcount + 1);
      set_hash_value_slot (h, i, value);
    }
  else
    {
      hash_put (h, new_obj, make_natnum (1), hashcode);
    }

  return allocate_emacs_value (env, &global_storage, new_obj);
}

static void module_free_global_ref (emacs_env *env,
                                    emacs_value ref)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  /* TODO: This probably never signals. */
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
      const EMACS_UINT refcount = XFASTINT (value);
      eassert (refcount > 0);
      if (refcount > 1)
        {
          XSETFASTINT (value, refcount - 1);
          set_hash_value_slot (h, i, value);
        }
      else
        {
          hash_remove_from_table (h, value);
        }
    }
}

static enum emacs_funcall_exit module_non_local_exit_check (emacs_env *env)
{
  check_main_thread ();
  return env->private_members->pending_non_local_exit;
}

static void module_non_local_exit_clear (emacs_env *env)
{
  check_main_thread ();
  env->private_members->pending_non_local_exit = emacs_funcall_exit_return;
}

static enum emacs_funcall_exit module_non_local_exit_get (emacs_env *env, emacs_value *sym, emacs_value *data)
{
  check_main_thread ();
  struct emacs_env_private *const p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return)
    {
      *sym = &p->non_local_exit_symbol;
      *data = &p->non_local_exit_data;
    }
  return p->pending_non_local_exit;
}

/*
 * Like for `signal', DATA must be a list
 */
static void module_non_local_exit_signal (emacs_env *env, emacs_value sym, emacs_value data)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  module_non_local_exit_signal_1 (env, value_to_lisp (sym), value_to_lisp (data));
}

static void module_non_local_exit_throw (emacs_env *env, emacs_value tag, emacs_value value)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  module_non_local_exit_throw_1 (env, value_to_lisp (tag), value_to_lisp (value));
}

/*
 * A module function is lambda function that calls `module-call',
 * passing the function pointer of the module function along with the
 * module emacs_env pointer as arguments.
 *
 *   (function
 *    (lambda
 *     (&rest arglist)
 *     (module-call
 *      envobj
 *      arglist)))
 *
 */
static emacs_value module_make_function (emacs_env *env,
                                         int min_arity,
                                         int max_arity,
                                         emacs_subr subr,
                                         const char *const documentation,
                                         void *data)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;

  if (min_arity > MOST_POSITIVE_FIXNUM || max_arity > MOST_POSITIVE_FIXNUM)
    xsignal0 (Qoverflow_error);

  if (min_arity < 0 ||
      (max_arity >= 0 && max_arity < min_arity) ||
      (max_arity < 0 && max_arity != emacs_variadic_function))
    xsignal2 (Qinvalid_arity, make_number (min_arity), make_number (max_arity));

  Lisp_Object envobj;

  /* XXX: This should need to be freed when envobj is GC'd */
  struct module_fun_env *envptr = xzalloc (sizeof (*envptr));
  envptr->min_arity = min_arity;
  envptr->max_arity = max_arity;
  envptr->subr = subr;
  envptr->data = data;
  envobj = make_save_ptr (envptr);

  Lisp_Object ret = list4 (Qlambda,
                           list2 (Qand_rest, Qargs),
                           documentation ? build_string (documentation) : Qnil,
                           list3 (module_call_func,
                                  envobj,
                                  Qargs));

  return lisp_to_value (env, ret);
}

static emacs_value module_funcall (emacs_env *env,
                                   emacs_value fun,
                                   int nargs,
                                   emacs_value args[])
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  MODULE_HANDLE_THROW;

  /*
   *  Make a new Lisp_Object array starting with the function as the
   *  first arg, because that's what Ffuncall takes
   */
  Lisp_Object newargs[nargs + 1];
  newargs[0] = value_to_lisp (fun);
  for (int i = 0; i < nargs; i++)
    newargs[1 + i] = value_to_lisp (args[i]);
  return lisp_to_value (env, Ffuncall (nargs + 1, newargs));
}

static emacs_value module_intern (emacs_env *env, const char *name)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  return lisp_to_value (env, intern (name));
}

static emacs_value module_type_of (emacs_env *env, emacs_value value)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  return lisp_to_value (env, Ftype_of (value_to_lisp (value)));
}

static bool module_is_not_nil (emacs_env *env, emacs_value value)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  return ! NILP (value_to_lisp (value));
}

static bool module_eq (emacs_env *env, emacs_value a, emacs_value b)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  return EQ (value_to_lisp (a), value_to_lisp (b));
}

static int64_t module_extract_integer (emacs_env *env, emacs_value n)
{
  verify (INT64_MIN <= MOST_NEGATIVE_FIXNUM);
  verify (INT64_MAX >= MOST_POSITIVE_FIXNUM);
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  const Lisp_Object l = value_to_lisp (n);
  if (! INTEGERP (l))
    {
      module_wrong_type (env, Qintegerp, l);
      return 0;
    }
  return XINT (l);
}

static emacs_value module_make_integer (emacs_env *env, int64_t n)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  if (n < MOST_NEGATIVE_FIXNUM)
    {
      module_non_local_exit_signal_1 (env, Qunderflow_error, Qnil);
      return NULL;
    }
  if (n > MOST_POSITIVE_FIXNUM)
    {
      module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return NULL;
    }
  return lisp_to_value (env, make_number (n));
}

static double module_extract_float (emacs_env *env, emacs_value f)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  const Lisp_Object lisp = value_to_lisp (f);
  if (! FLOATP (lisp))
    {
      module_wrong_type (env, Qfloatp, lisp);
      return 0;
    }
  return XFLOAT_DATA (lisp);
}

static emacs_value module_make_float (emacs_env *env, double d)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  return lisp_to_value (env, make_float (d));
}

static bool module_copy_string_contents (emacs_env *env,
                                         emacs_value value,
                                         char *buffer,
                                         size_t* length)
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

  size_t raw_size = SBYTES (lisp_str);

  /*
   * Emacs internal encoding is more-or-less UTF8, let's assume utf8
   * encoded emacs string are the same byte size.
   */

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

static emacs_value module_make_string (emacs_env *env, const char *str, size_t length)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  MODULE_HANDLE_SIGNALS;
  if (length > PTRDIFF_MAX)
    {
      module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return NULL;
    }
  /* Assume STR is utf8 encoded */
  return lisp_to_value (env, make_string (str, length));
}

static emacs_value module_make_user_ptr (emacs_env *env,
                                         emacs_finalizer_function fin,
                                         void *ptr)
{
  check_main_thread ();
  return lisp_to_value (env, make_user_ptr (fin, ptr));
}

static void* module_get_user_ptr (emacs_env *env, emacs_value uptr)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  const Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp))
    {
      module_wrong_type (env, Quser_ptr, lisp);
      return NULL;
    }
  return XUSER_PTR (lisp)->p;
}

static void module_set_user_ptr (emacs_env *env, emacs_value uptr, void *ptr)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  const Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp)) module_wrong_type (env, Quser_ptr, lisp);
  XUSER_PTR (lisp)->p = ptr;
}

static emacs_finalizer_function module_get_user_finalizer (emacs_env *env, emacs_value uptr)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  const Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp))
    {
      module_wrong_type (env, Quser_ptr, lisp);
      return NULL;
    }
  return XUSER_PTR (lisp)->finalizer;
}

static void module_set_user_finalizer (emacs_env *env,
                                           emacs_value uptr,
                                           emacs_finalizer_function fin)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  const Lisp_Object lisp = value_to_lisp (uptr);
  if (! USER_PTRP (lisp)) module_wrong_type (env, Quser_ptr, lisp);
  XUSER_PTR (lisp)->finalizer = fin;
}

static void module_vec_set (emacs_env *env,
			    emacs_value vec,
			    size_t i,
			    emacs_value val)
{
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  if (i > MOST_POSITIVE_FIXNUM)
    {
      module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return;
    }
  Lisp_Object lvec = value_to_lisp (vec);
  if (! VECTORP (lvec))
    {
      module_wrong_type (env, Qvectorp, lvec);
      return;
    }
  if (i >= ASIZE (lvec))
    {
      module_args_out_of_range (env, lvec, make_number (i));
      return;
    }
  ASET (lvec, i, value_to_lisp (val));
}

static emacs_value module_vec_get (emacs_env *env,
                                   emacs_value vec,
                                   size_t i)
{
  /* Type of ASIZE (lvec) is ptrdiff_t, make sure it fits */
  verify (PTRDIFF_MAX <= SIZE_MAX);
  check_main_thread ();
  eassert (module_non_local_exit_check (env) == emacs_funcall_exit_return);
  if (i > MOST_POSITIVE_FIXNUM)
    {
      module_non_local_exit_signal_1 (env, Qoverflow_error, Qnil);
      return NULL;
    }
  Lisp_Object lvec = value_to_lisp (vec);
  if (! VECTORP (lvec))
    {
      module_wrong_type (env, Qvectorp, lvec);
      return NULL;
    }
  /* Prevent error-prone comparison between types of different signedness. */
  const size_t size = ASIZE (lvec);
  eassert (size >= 0);
  if (i >= size)
    {
      if (i > MOST_POSITIVE_FIXNUM) i = MOST_POSITIVE_FIXNUM;
      module_args_out_of_range (env, lvec, make_number (i));
      return NULL;
    }
  return lisp_to_value (env, AREF (lvec, i));
}

static size_t module_vec_size (emacs_env *env,
                               emacs_value vec)
{
  /* Type of ASIZE (lvec) is ptrdiff_t, make sure it fits */
  verify (PTRDIFF_MAX <= SIZE_MAX);
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


/* Subroutines */

DEFUN ("module-load", Fmodule_load, Smodule_load, 1, 1, 0,
       doc: /* Load module FILE.  */)
  (Lisp_Object file)
{
  dynlib_handle_ptr handle;
  emacs_init_function module_init;
  void *gpl_sym;
  Lisp_Object doc_name, args[2];

  CHECK_STRING (file);
  handle = dynlib_open (SDATA (file));
  if (!handle)
    error ("Cannot load file %s: %s", SDATA (file), dynlib_error ());

  gpl_sym = dynlib_sym (handle, "plugin_is_GPL_compatible");
  if (!gpl_sym)
    error ("Module %s is not GPL compatible", SDATA (file));

  module_init = (emacs_init_function) dynlib_sym (handle, "emacs_module_init");
  if (!module_init)
    error ("Module %s does not have an init function.", SDATA (file));

  struct {
    struct emacs_runtime pub;
    struct emacs_runtime_private priv;
  } runtime = {
    .pub = {
      .size = sizeof runtime.pub,
      .get_environment = module_get_environment,
      .private_members = &runtime.priv
    }
  };
  initialize_environment (&runtime.priv.environment);
  int r = module_init (&runtime.pub);
  finalize_environment (&runtime.priv.environment);

  if (r != 0)
    {
      if (r < MOST_NEGATIVE_FIXNUM)
        xsignal0 (Qunderflow_error);
      if (r > MOST_POSITIVE_FIXNUM)
        xsignal0 (Qoverflow_error);
      xsignal2 (Qmodule_load_failed, file, make_number (r));
    }

  return Qt;
}

DEFUN ("module-call", Fmodule_call, Smodule_call, 2, 2, 0,
       doc: /* Internal function to call a module function.
ENVOBJ is a save pointer to a module_fun_env structure.
ARGLIST is a list of arguments passed to SUBRPTR. */)
  (Lisp_Object envobj, Lisp_Object arglist)
{
  const struct module_fun_env *const envptr =
    (const struct module_fun_env *) XSAVE_POINTER (envobj, 0);
  const EMACS_INT len = XINT (Flength (arglist));
  eassert (len >= 0);
  if (len > MOST_POSITIVE_FIXNUM)
    xsignal0 (Qoverflow_error);
  if (len > INT_MAX || len < envptr->min_arity || (envptr->max_arity >= 0 && len > envptr->max_arity))
    xsignal2 (Qwrong_number_of_arguments, module_format_fun_env (envptr), make_number (len));

  struct env_storage env;
  initialize_environment (&env);

  emacs_value *args = xzalloc (len * sizeof (*args));
  int i;

  for (i = 0; i < len; i++)
    {
      args[i] = lisp_to_value (&env.pub, XCAR (arglist));
      if (! args[i]) memory_full (sizeof *args[i]);
      arglist = XCDR (arglist);
    }

  emacs_value ret = envptr->subr (&env.pub, len, args, envptr->data);
  xfree (args);

  switch (env.priv.pending_non_local_exit)
    {
    case emacs_funcall_exit_return:
      finalize_environment (&env);
      if (ret == NULL) xsignal1 (Qinvalid_module_call, module_format_fun_env (envptr));
      return value_to_lisp (ret);
    case emacs_funcall_exit_signal:
      {
        const Lisp_Object symbol = value_to_lisp (&env.priv.non_local_exit_symbol);
        const Lisp_Object data = value_to_lisp (&env.priv.non_local_exit_data);
        finalize_environment (&env);
        xsignal (symbol, data);
      }
    case emacs_funcall_exit_throw:
      {
        const Lisp_Object tag = value_to_lisp (&env.priv.non_local_exit_symbol);
        const Lisp_Object value = value_to_lisp (&env.priv.non_local_exit_data);
        finalize_environment (&env);
        Fthrow (tag, value);
      }
    }
}


/* Helper functions */

static void check_main_thread (void)
{
#if defined(HAVE_THREADS_H)
  eassert (thrd_equal (thdr_current (), main_thread);
#elif defined(HAVE_PTHREAD)
  eassert (pthread_equal (pthread_self (), main_thread));
#elif defined(WINDOWSNT)
  /* CompareObjectHandles would be perfect, but is only available in
     Windows 10.  Also check whether the thread is still running to
     protect against thread identifier reuse. */
  eassert (GetCurrentThreadID () == GetThreadID (main_thread) &&
           WaitForSingleObject (main_thread, 0) == WAIT_TIMEOUT);
#endif
}

static void module_non_local_exit_signal_1 (emacs_env *env, Lisp_Object sym, Lisp_Object data)
{
  struct emacs_env_private *const p = env->private_members;
  eassert (p->pending_non_local_exit == emacs_funcall_exit_return);
  p->pending_non_local_exit = emacs_funcall_exit_signal;
  p->non_local_exit_symbol.v = sym;
  p->non_local_exit_data.v = data;
}

static void module_non_local_exit_throw_1 (emacs_env *env, Lisp_Object tag, Lisp_Object value)
{
  struct emacs_env_private *const p = env->private_members;
  eassert (p->pending_non_local_exit == emacs_funcall_exit_return);
  p->pending_non_local_exit = emacs_funcall_exit_throw;
  p->non_local_exit_symbol.v = tag;
  p->non_local_exit_data.v = value;
}

static void module_wrong_type (emacs_env *env, Lisp_Object predicate, Lisp_Object value)
{
  module_non_local_exit_signal_1 (env, Qwrong_type_argument, list2 (predicate, value));
}

static void module_out_of_memory (emacs_env *env)
{
  // TODO: Reimplement this so it works even if memory-signal-data has been modified.
  module_non_local_exit_signal_1 (env, XCAR (Vmemory_signal_data), XCDR (Vmemory_signal_data));
}

static void module_args_out_of_range (emacs_env *env, Lisp_Object a1, Lisp_Object a2)
{
  module_non_local_exit_signal_1 (env, Qargs_out_of_range, list2 (a1, a2));
}


/* Value conversion */

static Lisp_Object value_to_lisp (emacs_value v)
{
  return v->v;
}

static emacs_value lisp_to_value (emacs_env *env, Lisp_Object o)
{
  struct emacs_env_private *const p = env->private_members;
  if (p->pending_non_local_exit != emacs_funcall_exit_return) return NULL;
  return allocate_emacs_value (env, &p->storage, o);
}


/* Memory management */

static void initialize_frame (struct emacs_value_frame *frame)
{
  frame->offset = 0;
  frame->next = NULL;
}

static void initialize_storage (struct emacs_value_storage *storage)
{
  initialize_frame (&storage->initial);
  storage->current = &storage->initial;
}

static void finalize_storage (struct emacs_value_storage *storage)
{
  struct emacs_value_frame *next = storage->initial.next;
  while (next != NULL)
    {
      struct emacs_value_frame *const current = next;
      next = current->next;
      free (current);
    }
}

static emacs_value allocate_emacs_value (emacs_env *env, struct emacs_value_storage *storage,
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
  const emacs_value value = storage->current->objects + storage->current->offset;
  value->v = obj;
  ++storage->current->offset;
  return value;
}

/* Mark all objects allocated from local environments so that they
   don't get garbage-collected. */
void mark_modules (void)
{
  for (Lisp_Object tem = Vmodule_environments; CONSP (tem); tem = XCDR (tem))
    {
      const struct env_storage *const env = XSAVE_POINTER (tem, 0);
      for (const struct emacs_value_frame *frame = &env->priv.storage.initial; frame != NULL; frame = frame->next)
        for (size_t i = 0; i < frame->offset; ++i)
          mark_object (frame->objects[i].v);
    }
}


/* Environment lifetime management */

static void initialize_environment (struct env_storage *env)
{
  env->priv.pending_non_local_exit = emacs_funcall_exit_return;
  initialize_storage (&env->priv.storage);
  env->pub.size            = sizeof env->pub;
  env->pub.private_members = &env->priv;
  env->pub.make_global_ref = module_make_global_ref;
  env->pub.free_global_ref = module_free_global_ref;
  env->pub.non_local_exit_check     = module_non_local_exit_check;
  env->pub.non_local_exit_clear     = module_non_local_exit_clear;
  env->pub.non_local_exit_get       = module_non_local_exit_get;
  env->pub.non_local_exit_signal    = module_non_local_exit_signal;
  env->pub.non_local_exit_throw     = module_non_local_exit_throw;
  env->pub.make_function   = module_make_function;
  env->pub.funcall         = module_funcall;
  env->pub.intern          = module_intern;
  env->pub.type_of         = module_type_of;
  env->pub.is_not_nil      = module_is_not_nil;
  env->pub.eq              = module_eq;
  env->pub.extract_integer   = module_extract_integer;
  env->pub.make_integer     = module_make_integer;
  env->pub.extract_float = module_extract_float;
  env->pub.make_float      = module_make_float;
  env->pub.copy_string_contents = module_copy_string_contents;
  env->pub.make_string     = module_make_string;
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

static void finalize_environment (struct env_storage *env)
{
  finalize_storage (&env->priv.storage);
  Vmodule_environments = XCDR (Vmodule_environments);
}


/* Non-local exit handling */

static void module_reset_handlerlist(const int *dummy)
{
  handlerlist = handlerlist->next;
}

static void module_handle_signal (emacs_env *const env, const Lisp_Object err)
{
  module_non_local_exit_signal_1 (env, XCAR (err), XCDR (err));
}

static void module_handle_throw (emacs_env *const env, const Lisp_Object tag_val)
{
  module_non_local_exit_throw_1 (env, XCAR (tag_val), XCDR (tag_val));
}


/* Function environments */

static Lisp_Object module_format_fun_env (const struct module_fun_env *const env)
{
  /* Try to print a function name if possible. */
  const char *path, *sym;
  if (dynlib_addr (env->subr, &path, &sym))
    {
      const char *const format = "#<module function %s from %s>";
      const int size = snprintf (NULL, 0, format, sym, path);
      eassert (size > 0);
      char buffer[size + 1];
      snprintf (buffer, sizeof buffer, format, sym, path);
      return make_unibyte_string (buffer, size);
    }
  else
    {
      const char *const format = "#<module function at %p>";
      const void *const subr = env->subr;
      const int size = snprintf (NULL, 0, format, subr);
      eassert (size > 0);
      char buffer[size + 1];
      snprintf (buffer, sizeof buffer, format, subr);
      return make_unibyte_string (buffer, size);
    }
}


/* Segment initializer */

void syms_of_module (void)
{
  DEFSYM (Qmodule_refs_hash, "module-refs-hash");
  DEFVAR_LISP ("module-refs-hash", Vmodule_refs_hash,
	       doc: /* Module global referrence table.  */);

  Vmodule_refs_hash = make_hash_table (hashtest_eq, make_number (DEFAULT_HASH_SIZE),
                                       make_float (DEFAULT_REHASH_SIZE),
                                       make_float (DEFAULT_REHASH_THRESHOLD),
                                       Qnil);
  Funintern (Qmodule_refs_hash, Qnil);

  DEFSYM (Qmodule_environments, "module-environments");
  DEFVAR_LISP ("module-environments", Vmodule_environments,
               doc: /* List of active module environments. */);
  Vmodule_environments = Qnil;
  /* Unintern `module-environments' because it is only used
     internally. */
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
     code or modules should not access it. */
  Funintern (Qmodule_refs_hash, Qnil);

  defsubr (&Smodule_load);

  /* Don't call defsubr on `module-call' because that would intern it,
     but `module-call' is an internal function that users cannot
     meaningfully use.  Instead, assign its definition to a private
     variable. */
  XSETPVECTYPE (&Smodule_call, PVEC_SUBR);
  XSETSUBR (module_call_func, &Smodule_call);
}

/* Unlike syms_of_module, this initializer is called even from an
 * initialized (dumped) Emacs. */

void module_init (void)
{
  /* It is not guaranteed that dynamic initializers run in the main thread,
     therefore we detect the main thread here. */
#if defined(HAVE_THREADS_H)
  main_thread = thrd_current ();
#elif defined(HAVE_PTHREAD)
  main_thread = pthread_self ();
#elif defined(WINDOWSNT)
  /* GetCurrentProcess returns a pseudohandle, which we have to duplicate. */
  if (! DuplicateHandle (GetCurrentProcess(), GetCurrentThread(),
                         GetCurrentProcess(), &main_thread,
                         SYNCHRONIZE | THREAD_QUERY_LIMITED_INFORMATION,
                         FALSE, 0))
    emacs_abort ();
#endif
}
