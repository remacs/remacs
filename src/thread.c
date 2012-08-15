/* Threading code.
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
#include "character.h"
#include "buffer.h"

/* FIXME */
extern void unbind_for_thread_switch (void);
extern void rebind_for_thread_switch (void);

static struct thread_state primary_thread;

struct thread_state *current_thread = &primary_thread;

static struct thread_state *all_threads = &primary_thread;

sys_mutex_t global_lock;

Lisp_Object Qthreadp, Qmutexp;



struct Lisp_Mutex
{
  struct vectorlike_header header;

  lisp_mutex_t mutex;
};

DEFUN ("make-mutex", Fmake_mutex, Smake_mutex, 0, 0, 0,
       doc: /* FIXME */)
  (void)
{
  struct Lisp_Mutex *mutex;
  Lisp_Object result;

  mutex = ALLOCATE_PSEUDOVECTOR (struct Lisp_Mutex, mutex, PVEC_MUTEX);
  memset ((char *) mutex + offsetof (struct Lisp_Mutex, mutex),
	  0, sizeof (struct Lisp_Mutex) - offsetof (struct Lisp_Mutex,
						    mutex));
  lisp_mutex_init (&mutex->mutex);

  XSETMUTEX (result, mutex);
  return result;
}

static void
mutex_lock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;

  /* This calls post_acquire_global_lock.  */
  lisp_mutex_lock (&mutex->mutex);
}

DEFUN ("mutex-lock", Fmutex_lock, Smutex_lock, 1, 1, 0,
       doc: /* FIXME */)
  (Lisp_Object obj)
{
  struct Lisp_Mutex *mutex;

  CHECK_MUTEX (obj);
  mutex = XMUTEX (obj);

  flush_stack_call_func (mutex_lock_callback, mutex);
  return Qnil;
}

static void
mutex_unlock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;

  /* This calls post_acquire_global_lock.  */
  lisp_mutex_unlock (&mutex->mutex);
}

DEFUN ("mutex-unlock", Fmutex_unlock, Smutex_unlock, 1, 1, 0,
       doc: /* FIXME */)
  (Lisp_Object obj)
{
  struct Lisp_Mutex *mutex;

  CHECK_MUTEX (obj);
  mutex = XMUTEX (obj);

  flush_stack_call_func (mutex_unlock_callback, mutex);
  return Qnil;
}

void
finalize_one_mutex (struct Lisp_Mutex *mutex)
{
  lisp_mutex_destroy (&mutex->mutex);
}



static void
release_global_lock (void)
{
  sys_mutex_unlock (&global_lock);
}

/* You must call this after acquiring the global lock.
   acquire_global_lock does it for you.  */
void
post_acquire_global_lock (struct thread_state *self)
{
  Lisp_Object buffer;

  if (self != current_thread)
    {
      unbind_for_thread_switch ();
      current_thread = self;
      rebind_for_thread_switch ();
    }

  /* We need special handling to re-set the buffer.  */
  XSETBUFFER (buffer, self->m_current_buffer);
  self->m_current_buffer = 0;
  set_buffer_internal (XBUFFER (buffer));

  if (!EQ (current_thread->error_symbol, Qnil))
    {
      Lisp_Object sym = current_thread->error_symbol;
      Lisp_Object data = current_thread->error_data;

      current_thread->error_symbol = Qnil;
      current_thread->error_data = Qnil;
      Fsignal (sym, data);
    }
}

static void
acquire_global_lock (struct thread_state *self)
{
  sys_mutex_lock (&global_lock);
  post_acquire_global_lock (self);
}



static void
mark_one_thread (struct thread_state *thread)
{
  struct specbinding *bind;
  struct handler *handler;
  Lisp_Object tem;

  for (bind = thread->m_specpdl; bind != thread->m_specpdl_ptr; bind++)
    {
      mark_object (bind->symbol);
      mark_object (bind->old_value);
      mark_object (bind->saved_value);
    }

#if (GC_MARK_STACK == GC_MAKE_GCPROS_NOOPS \
     || GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS)
  mark_stack (thread->m_stack_bottom, thread->stack_top);
#else
  {
    struct gcpro *tail;
    for (tail = thread->m_gcprolist; tail; tail = tail->next)
      for (i = 0; i < tail->nvars; i++)
	mark_object (tail->var[i]);
  }

#if BYTE_MARK_STACK
  if (thread->m_byte_stack_list)
    mark_byte_stack (thread->m_byte_stack_list);
#endif

  mark_catchlist (thread->m_catchlist);

  for (handler = thread->m_handlerlist; handler; handler = handler->next)
    {
      mark_object (handler->handler);
      mark_object (handler->var);
    }

  mark_backtrace (thread->m_backtrace_list);
#endif

  if (thread->m_current_buffer)
    {
      XSETBUFFER (tem, thread->m_current_buffer);
      mark_object (tem);
    }

  mark_object (thread->m_last_thing_searched);

  if (thread->m_saved_last_thing_searched)
    mark_object (thread->m_saved_last_thing_searched);
}

static void
mark_threads_callback (void *ignore)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      Lisp_Object thread_obj;

      XSETTHREAD (thread_obj, iter);
      mark_object (thread_obj);
      mark_one_thread (iter);
    }
}

void
mark_threads (void)
{
  flush_stack_call_func (mark_threads_callback, NULL);
}

void
unmark_threads (void)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    if (iter->m_byte_stack_list)
      unmark_byte_stack (iter->m_byte_stack_list);
}



static void
yield_callback (void *ignore)
{
  struct thread_state *self = current_thread;

  release_global_lock ();
  sys_thread_yield ();
  acquire_global_lock (self);
}

DEFUN ("thread-yield", Fthread_yield, Sthread_yield, 0, 0, 0,
       doc: /* Yield the CPU to another thread.  */)
     (void)
{
  flush_stack_call_func (yield_callback, NULL);
  return Qnil;
}

static Lisp_Object
invoke_thread_function (void)
{
  Lisp_Object iter;

  int count = SPECPDL_INDEX ();

  Ffuncall (1, &current_thread->function);
  return unbind_to (count, Qnil);
}

static Lisp_Object
do_nothing (Lisp_Object whatever)
{
  return whatever;
}

static void *
run_thread (void *state)
{
  char stack_pos;
  struct thread_state *self = state;
  struct thread_state **iter;

  self->m_stack_bottom = &stack_pos;
  self->stack_top = self->m_stack_bottom = &stack_pos;
  self->thread_id = sys_thread_self ();

  acquire_global_lock (self);

  /* It might be nice to do something with errors here.  */
  internal_condition_case (invoke_thread_function, Qt, do_nothing);

  unbind_for_thread_switch ();

  /* Unlink this thread from the list of all threads.  */
  for (iter = &all_threads; *iter != self; iter = &(*iter)->next_thread)
    ;
  *iter = (*iter)->next_thread;

  self->m_last_thing_searched = Qnil;
  self->m_saved_last_thing_searched = Qnil;
  self->name = Qnil;
  self->function = Qnil;
  self->error_symbol = Qnil;
  self->error_data = Qnil;
  xfree (self->m_specpdl);
  self->m_specpdl = NULL;
  self->m_specpdl_ptr = NULL;
  self->m_specpdl_size = 0;

  sys_cond_broadcast (&self->thread_condvar);

  release_global_lock ();

  return NULL;
}

void
finalize_one_thread (struct thread_state *state)
{
  sys_cond_destroy (&state->thread_condvar);
}

DEFUN ("make-thread", Fmake_thread, Smake_thread, 1, 2, 0,
       doc: /* Start a new thread and run FUNCTION in it.
When the function exits, the thread dies.
If NAME is given, it names the new thread.  */)
  (Lisp_Object function, Lisp_Object name)
{
  sys_thread_t thr;
  struct thread_state *new_thread;
  Lisp_Object result;

  /* Can't start a thread in temacs.  */
  if (!initialized)
    abort ();

  new_thread = ALLOCATE_PSEUDOVECTOR (struct thread_state, m_gcprolist,
				      PVEC_THREAD);
  memset ((char *) new_thread + offsetof (struct thread_state, m_gcprolist),
	  0, sizeof (struct thread_state) - offsetof (struct thread_state,
						      m_gcprolist));

  new_thread->function = function;
  new_thread->name = name;
  new_thread->m_last_thing_searched = Qnil; /* copy from parent? */
  new_thread->m_saved_last_thing_searched = Qnil;
  new_thread->m_current_buffer = current_thread->m_current_buffer;
  new_thread->error_symbol = Qnil;
  new_thread->error_data = Qnil;

  new_thread->m_specpdl_size = 50;
  new_thread->m_specpdl = xmalloc (new_thread->m_specpdl_size
				   * sizeof (struct specbinding));
  new_thread->m_specpdl_ptr = new_thread->m_specpdl;

  sys_cond_init (&new_thread->thread_condvar);

  /* We'll need locking here eventually.  */
  new_thread->next_thread = all_threads;
  all_threads = new_thread;

  if (! sys_thread_create (&thr, run_thread, new_thread))
    {
      /* Restore the previous situation.  */
      all_threads = all_threads->next_thread;
      error ("Could not start a new thread");
    }

  /* FIXME: race here where new thread might not be filled in?  */
  XSETTHREAD (result, new_thread);
  return result;
}

DEFUN ("current-thread", Fcurrent_thread, Scurrent_thread, 0, 0, 0,
       doc: /* Return the current thread.  */)
  (void)
{
  Lisp_Object result;
  XSETTHREAD (result, current_thread);
  return result;
}

DEFUN ("thread-name", Fthread_name, Sthread_name, 1, 1, 0,
       doc: /* Return the name of the THREAD.
The name is the same object that was passed to `make-thread'.  */)
     (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return tstate->name;
}

static void
thread_signal_callback (void *arg)
{
  struct thread_state *tstate = arg;
  struct thread_state *self = current_thread;

  sys_cond_broadcast (tstate->wait_condvar);
  post_acquire_global_lock (self);
}

DEFUN ("thread-signal", Fthread_signal, Sthread_signal, 3, 3, 0,
       doc: /* FIXME */)
  (Lisp_Object thread, Lisp_Object error_symbol, Lisp_Object data)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    Fsignal (error_symbol, data);

  /* What to do if thread is already signalled?  */
  /* What if error_symbol is Qnil?  */
  tstate->error_symbol = error_symbol;
  tstate->error_data = data;

  if (tstate->wait_condvar)
    flush_stack_call_func (thread_signal_callback, tstate);

  return Qnil;
}

DEFUN ("thread-alive-p", Fthread_alive_p, Sthread_alive_p, 1, 1, 0,
       doc: /* FIXME */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  /* m_specpdl is set when the thread is created and cleared when the
     thread dies.  */
  return tstate->m_specpdl == NULL ? Qnil : Qt;
}

static void
thread_join_callback (void *arg)
{
  struct thread_state *tstate = arg;
  struct thread_state *self = current_thread;

  self->wait_condvar = &tstate->thread_condvar;
  while (tstate->m_specpdl != NULL && EQ (self->error_symbol, Qnil))
    sys_cond_wait (self->wait_condvar, &global_lock);

  self->wait_condvar = NULL;
  post_acquire_global_lock (self);
}

DEFUN ("thread-join", Fthread_join, Sthread_join, 1, 1, 0,
       doc: /* FIXME */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate->m_specpdl != NULL)
    flush_stack_call_func (thread_join_callback, tstate);

  return Qnil;
}

DEFUN ("all-threads", Fall_threads, Sall_threads, 0, 0, 0,
       doc: /* Return a list of all threads.  */)
     (void)
{
  Lisp_Object result = Qnil;
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      Lisp_Object thread;

      XSETTHREAD (thread, iter);
      result = Fcons (thread, result);
    }

  return result;
}



static void
init_primary_thread (void)
{
  primary_thread.header.size
    = PSEUDOVECSIZE (struct thread_state, m_gcprolist);
  XSETPVECTYPE (&primary_thread, PVEC_THREAD);
  primary_thread.m_last_thing_searched = Qnil;
  primary_thread.m_saved_last_thing_searched = Qnil;
  primary_thread.name = Qnil;
  primary_thread.function = Qnil;
  primary_thread.error_symbol = Qnil;
  primary_thread.error_data = Qnil;

  sys_cond_init (&primary_thread.thread_condvar);
}

void
init_threads_once (void)
{
  init_primary_thread ();
}

void
init_threads (void)
{
  init_primary_thread ();

  sys_mutex_init (&global_lock);
  sys_mutex_lock (&global_lock);
}

void
syms_of_threads (void)
{
  defsubr (&Sthread_yield);
  defsubr (&Smake_thread);
  defsubr (&Scurrent_thread);
  defsubr (&Sthread_name);
  defsubr (&Sthread_signal);
  defsubr (&Sthread_alive_p);
  defsubr (&Sthread_join);
  defsubr (&Sall_threads);
  defsubr (&Smake_mutex);
  defsubr (&Smutex_lock);
  defsubr (&Smutex_unlock);

  Qthreadp = intern_c_string ("threadp");
  staticpro (&Qthreadp);
  Qmutexp = intern_c_string ("mutexp");
  staticpro (&Qmutexp);
}
