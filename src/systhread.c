/* System thread definitions
   Copyright (C) 2012 Free Software Foundation, Inc.

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
#include <setjmp.h>
#include "lisp.h"

#ifdef HAVE_PTHREAD

#include <sched.h>

void
sys_mutex_init (sys_mutex_t *mutex)
{
  pthread_mutex_init (mutex, NULL);
}

void
sys_mutex_lock (sys_mutex_t *mutex)
{
  pthread_mutex_lock (mutex);
}

void
sys_mutex_unlock (sys_mutex_t *mutex)
{
  pthread_mutex_unlock (mutex);
}

void
sys_mutex_destroy (sys_mutex_t *mutex)
{
  pthread_mutex_destroy (mutex);
}

void
sys_cond_init (sys_cond_t *cond)
{
  pthread_cond_init (cond, NULL);
}

void
sys_cond_wait (sys_cond_t *cond, sys_mutex_t *mutex)
{
  pthread_cond_wait (cond, mutex);
}

void
sys_cond_signal (sys_cond_t *cond)
{
  pthread_cond_signal (cond);
}

void
sys_cond_broadcast (sys_cond_t *cond)
{
  pthread_cond_broadcast (cond);
}

void
sys_cond_destroy (sys_cond_t *cond)
{
  pthread_cond_destroy (cond);
}

void
lisp_mutex_init (lisp_mutex_t *mutex)
{
  mutex->owner = NULL;
  mutex->count = 0;
  /* A lisp "mutex" is really a condition variable.  */
  pthread_cond_init (&mutex->condition, NULL);
}

void
lisp_mutex_lock (lisp_mutex_t *mutex)
{
  struct thread_state *self;

  if (mutex->owner == NULL)
    {
      mutex->owner = current_thread;
      mutex->count = 1;
      return;
    }
  if (mutex->owner == current_thread)
    {
      ++mutex->count;
      return;
    }

  self = current_thread;
  self->wait_condvar = &mutex->condition;
  while (mutex->owner != NULL && EQ (self->error_symbol, Qnil))
    pthread_cond_wait (&mutex->condition, &global_lock);
  self->wait_condvar = NULL;

  post_acquire_global_lock (self);

  mutex->owner = self;
  mutex->count = 1;
}

void
lisp_mutex_unlock (lisp_mutex_t *mutex)
{
  struct thread_state *self = current_thread;

  if (mutex->owner != current_thread)
    error ("blah");

  if (--mutex->count > 0)
    return;

  mutex->owner = NULL;
  pthread_cond_broadcast (&mutex->condition);

  post_acquire_global_lock (self);
}

void
lisp_mutex_destroy (lisp_mutex_t *mutex)
{
  sys_cond_destroy (&mutex->condition);
}

sys_thread_t
sys_thread_self (void)
{
  return pthread_self ();
}

int
sys_thread_equal (sys_thread_t one, sys_thread_t two)
{
  return pthread_equal (one, two);
}

int
sys_thread_create (sys_thread_t *thread_ptr, thread_creation_function *func,
		   void *arg)
{
  pthread_attr_t attr;
  int result = 0;

  if (pthread_attr_init (&attr))
    return 0;

  if (!pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED))
    result = pthread_create (thread_ptr, &attr, func, arg) == 0;

  pthread_attr_destroy (&attr);

  return result;
}

void
sys_thread_yield (void)
{
  sched_yield ();
}

#else

#error port me

#endif
