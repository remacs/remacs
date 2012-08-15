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

struct thread_state the_only_thread;

struct thread_state *current_thread = &the_only_thread;

struct thread_state *all_threads = &the_only_thread;

sys_mutex_t global_lock;

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

void
init_threads_once (void)
{
  the_only_thread.header.size
    = PSEUDOVECSIZE (struct thread_state, m_gcprolist);
  XSETPVECTYPE (&the_only_thread, PVEC_THREAD);
  the_only_thread.m_last_thing_searched = Qnil;
  the_only_thread.m_saved_last_thing_searched = Qnil;
}

void
init_threads (void)
{
  sys_mutex_init (&global_lock);
  sys_mutex_lock (&global_lock);
}
