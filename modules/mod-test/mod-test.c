#include <assert.h>
#include <stdio.h>
#include <emacs_module.h>

int plugin_is_GPL_compatible;

/*
 * Always return symbol 't'
 */
static emacs_value Fmod_test_return_t (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  return env->intern (env, "t");
}


/*
 * Expose simple sum function
 */
static int64_t sum (int64_t a, int64_t b)
{
  return a + b;
}

static emacs_value Fmod_test_sum (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  int64_t a = env->extract_integer (env, args[0]);
  int64_t b = env->extract_integer (env, args[1]);

  int64_t r = sum(a, b);

  return env->make_integer (env, r);
}


/*
 * Signal '(error 56)
 */
static emacs_value Fmod_test_signal (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  env->non_local_exit_signal (env, env->intern (env, "error"), env->make_integer (env, 56));
  return NULL;
}


/*
 * Throw '(tag 65)
 */
static emacs_value Fmod_test_throw (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  env->non_local_exit_throw (env, env->intern (env, "tag"), env->make_integer (env, 65));
  return NULL;
}


/*
 * Call argument function, catch all non-local exists and return
 * either normal result or a list describing the non-local exit.
 */
static emacs_value Fmod_test_non_local_exit_funcall (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (nargs == 1);
  const emacs_value result = env->funcall (env, args[0], 0, NULL);
  emacs_value non_local_exit_symbol, non_local_exit_data;
  enum emacs_funcall_exit code = env->non_local_exit_get (env, &non_local_exit_symbol, &non_local_exit_data);
  switch (code)
    {
    case emacs_funcall_exit_return:
      return result;
    case emacs_funcall_exit_signal:
      {
        env->non_local_exit_clear (env);
        const emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "signal"), non_local_exit_symbol, non_local_exit_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    case emacs_funcall_exit_throw:
      {
        env->non_local_exit_clear (env);
        const emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "throw"), non_local_exit_symbol, non_local_exit_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    }
  /* never reached */
  return env->intern (env, "nil");;
}


/*
 * Return a global referrence
 */
static emacs_value Fmod_test_globref_make (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  /* make a big string and make it global */
  size_t i;
  char str[26*100];

  for (i = 0; i < sizeof (str); i++)
    {
      str[i] = 'a' + (i % 26);
    }

  /* we don't need to null-terminate str */
  emacs_value lisp_str = env->make_string (env, str, sizeof (str));
  return env->make_global_ref (env, lisp_str);
}


/*
 * Return a copy of the argument string where every 'a' is replaced with 'b'.
 */
static emacs_value Fmod_test_string_a_to_b (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  emacs_value lisp_str = args[0];
  size_t size = 0;
  char * buf = NULL;
  size_t i;

  env->copy_string_contents (env, lisp_str, buf, &size);
  buf = malloc (size);
  env->copy_string_contents (env, lisp_str, buf, &size);

  for (i = 0; i+1 < size; i++) {
    if (buf[i] == 'a')
      buf[i] = 'b';
  }

  return env->make_string (env, buf, size-1);
}


/*
 * Embedded pointers in lisp objects.
 */

/* C struct (pointer to) that will be embedded */
struct super_struct
{
  int amazing_int;
  char large_unused_buffer[512];
};

/* Associated finalizer */
static void finalizer (void *p)
{
  if (p)
    free (p);
}

/*
 * Return a new user-pointer to a super_struct, with amazing_int set
 * to the passed parameter.
 */
static emacs_value Fmod_test_userptr_make (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  struct super_struct *p = calloc (1, sizeof(*p));
  p->amazing_int = env->extract_integer (env, args[0]);
  return env->make_user_ptr (env, finalizer, p);
}

/*
 * Return the amazing_int of a passed 'user-pointer to a super_struct'.
 */
static emacs_value Fmod_test_userptr_get (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  struct super_struct *p = env->get_user_ptr (env, args[0]);
  return env->make_integer (env, p->amazing_int);
}


/*
 * Fill vector in args[0] with value in args[1]
 */
static emacs_value Fmod_test_vector_fill (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  size_t i;
  emacs_value vec = args[0];
  emacs_value val = args[1];
  const size_t size = env->vec_size (env, vec);
  for (i = 0; i < size; i++)
    env->vec_set (env, vec, i, val);
  return env->intern (env, "t");
}


/*
 * Return whether all elements of vector in args[0] are 'eq' to value in args[1]
 */
static emacs_value Fmod_test_vector_eq (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  size_t i;
  emacs_value vec = args[0];
  emacs_value val = args[1];
  const size_t size = env->vec_size (env, vec);
  for (i = 0; i < size; i++)
    if (!env->eq (env, env->vec_get (env, vec, i), val))
        return env->intern (env, "nil");
  return env->intern (env, "t");
}


/*
 * Lisp utilities for easier readability (simple wrappers)
 */

/* Provide FEATURE to Emacs */
static void provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Binds NAME to FUN */
static void bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/*
 * Module init function.
 */
int emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("mod-test-return-t", Fmod_test_return_t, 1, 1, NULL, NULL);
  DEFUN ("mod-test-sum", Fmod_test_sum, 2, 2, "Return A + B", NULL);
  DEFUN ("mod-test-signal", Fmod_test_signal, 0, 0, NULL, NULL);
  DEFUN ("mod-test-throw", Fmod_test_throw, 0, 0, NULL, NULL);
  DEFUN ("mod-test-non-local-exit-funcall", Fmod_test_non_local_exit_funcall, 1, 1, NULL, NULL);
  DEFUN ("mod-test-globref-make", Fmod_test_globref_make, 0, 0, NULL, NULL);
  DEFUN ("mod-test-string-a-to-b", Fmod_test_string_a_to_b, 1, 1, NULL, NULL);
  DEFUN ("mod-test-userptr-make", Fmod_test_userptr_make, 1, 1, NULL, NULL);
  DEFUN ("mod-test-userptr-get", Fmod_test_userptr_get, 1, 1, NULL, NULL);
  DEFUN ("mod-test-vector-fill", Fmod_test_vector_fill, 2, 2, NULL, NULL);
  DEFUN ("mod-test-vector-eq", Fmod_test_vector_eq, 2, 2, NULL, NULL);

#undef DEFUN

  provide (env, "mod-test");
  return 0;
}
