/* emacs-module.h - GNU Emacs module API.

Copyright (C) 2015-2017 Free Software Foundation, Inc.

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

#ifndef EMACS_MODULE_H
#define EMACS_MODULE_H

#include <stdint.h>
#include <stddef.h>

#ifndef __cplusplus
#include <stdbool.h>
#endif

#if defined __cplusplus && __cplusplus >= 201103L
# define EMACS_NOEXCEPT noexcept
#else
# define EMACS_NOEXCEPT
#endif

#ifdef __has_attribute
#if __has_attribute(__nonnull__)
# define EMACS_ATTRIBUTE_NONNULL(...) __attribute__((__nonnull__(__VA_ARGS__)))
#endif
#endif
#ifndef EMACS_ATTRIBUTE_NONNULL
# define EMACS_ATTRIBUTE_NONNULL(...)
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Current environment.  */
typedef struct emacs_env_25 emacs_env;

/* Opaque pointer representing an Emacs Lisp value.
   BEWARE: Do not assume NULL is a valid value!  */
typedef struct emacs_value_tag *emacs_value;

enum { emacs_variadic_function = -2 };

/* Struct passed to a module init function (emacs_module_init).  */
struct emacs_runtime
{
  /* Structure size (for version checking).  */
  ptrdiff_t size;

  /* Private data; users should not touch this.  */
  struct emacs_runtime_private *private_members;

  /* Return an environment pointer.  */
  emacs_env *(*get_environment) (struct emacs_runtime *ert)
    EMACS_ATTRIBUTE_NONNULL(1);
};


/* Possible Emacs function call outcomes.  */
enum emacs_funcall_exit
{
  /* Function has returned normally.  */
  emacs_funcall_exit_return = 0,

  /* Function has signaled an error using `signal'.  */
  emacs_funcall_exit_signal = 1,

  /* Function has exit using `throw'.  */
  emacs_funcall_exit_throw = 2,
};

struct emacs_env_25
{
  /* Structure size (for version checking).  */
  ptrdiff_t size;

  /* Private data; users should not touch this.  */
  struct emacs_env_private *private_members;

  /* Memory management.  */

  emacs_value (*make_global_ref) (emacs_env *env,
				  emacs_value any_reference)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*free_global_ref) (emacs_env *env,
			   emacs_value global_reference)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Non-local exit handling.  */

  enum emacs_funcall_exit (*non_local_exit_check) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*non_local_exit_clear) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);

  enum emacs_funcall_exit (*non_local_exit_get)
    (emacs_env *env,
     emacs_value *non_local_exit_symbol_out,
     emacs_value *non_local_exit_data_out)
    EMACS_ATTRIBUTE_NONNULL(1, 2, 3);

  void (*non_local_exit_signal) (emacs_env *env,
				 emacs_value non_local_exit_symbol,
				 emacs_value non_local_exit_data)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*non_local_exit_throw) (emacs_env *env,
				emacs_value tag,
				emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Function registration.  */

  emacs_value (*make_function) (emacs_env *env,
				ptrdiff_t min_arity,
				ptrdiff_t max_arity,
				emacs_value (*function) (emacs_env *env,
							 ptrdiff_t nargs,
							 emacs_value args[],
							 void *)
				  EMACS_NOEXCEPT
                                  EMACS_ATTRIBUTE_NONNULL(1),
				const char *documentation,
				void *data)
    EMACS_ATTRIBUTE_NONNULL(1, 4);

  emacs_value (*funcall) (emacs_env *env,
                          emacs_value function,
                          ptrdiff_t nargs,
                          emacs_value args[])
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*intern) (emacs_env *env,
                         const char *symbol_name)
    EMACS_ATTRIBUTE_NONNULL(1, 2);

  /* Type conversion.  */

  emacs_value (*type_of) (emacs_env *env,
			  emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  bool (*is_not_nil) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  bool (*eq) (emacs_env *env, emacs_value a, emacs_value b)
    EMACS_ATTRIBUTE_NONNULL(1);

  intmax_t (*extract_integer) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*make_integer) (emacs_env *env, intmax_t value)
    EMACS_ATTRIBUTE_NONNULL(1);

  double (*extract_float) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*make_float) (emacs_env *env, double value)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Copy the content of the Lisp string VALUE to BUFFER as an utf8
     null-terminated string.

     SIZE must point to the total size of the buffer.  If BUFFER is
     NULL or if SIZE is not big enough, write the required buffer size
     to SIZE and return false.

     Note that SIZE must include the last null byte (e.g. "abc" needs
     a buffer of size 4).

     Return true if the string was successfully copied.  */

  bool (*copy_string_contents) (emacs_env *env,
                                emacs_value value,
                                char *buffer,
                                ptrdiff_t *size_inout)
    EMACS_ATTRIBUTE_NONNULL(1, 4);

  /* Create a Lisp string from a utf8 encoded string.  */
  emacs_value (*make_string) (emacs_env *env,
			      const char *contents, ptrdiff_t length)
    EMACS_ATTRIBUTE_NONNULL(1, 2);

  /* Embedded pointer type.  */
  emacs_value (*make_user_ptr) (emacs_env *env,
				void (*fin) (void *) EMACS_NOEXCEPT,
				void *ptr)
    EMACS_ATTRIBUTE_NONNULL(1);

  void *(*get_user_ptr) (emacs_env *env, emacs_value uptr)
    EMACS_ATTRIBUTE_NONNULL(1);
  void (*set_user_ptr) (emacs_env *env, emacs_value uptr, void *ptr)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*(*get_user_finalizer) (emacs_env *env, emacs_value uptr))
    (void *) EMACS_NOEXCEPT EMACS_ATTRIBUTE_NONNULL(1);
  void (*set_user_finalizer) (emacs_env *env,
			      emacs_value uptr,
			      void (*fin) (void *) EMACS_NOEXCEPT)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Vector functions.  */
  emacs_value (*vec_get) (emacs_env *env, emacs_value vec, ptrdiff_t i)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*vec_set) (emacs_env *env, emacs_value vec, ptrdiff_t i,
		   emacs_value val)
    EMACS_ATTRIBUTE_NONNULL(1);

  ptrdiff_t (*vec_size) (emacs_env *env, emacs_value vec)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Returns whether a quit is pending.  */
  bool (*should_quit) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);
};

/* Every module should define a function as follows.  */
extern int emacs_module_init (struct emacs_runtime *ert)
  EMACS_ATTRIBUTE_NONNULL(1);

#ifdef __cplusplus
}
#endif

#endif /* EMACS_MODULE_H */
