/* Thread definitions
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

#ifndef THREAD_H
#define THREAD_H

#include "regex.h"

#include "sysselect.h"		/* FIXME */
#include "systime.h"		/* FIXME */

struct thread_state
{
  struct vectorlike_header header;

  /* The buffer in which the last search was performed, or
     Qt if the last search was done in a string;
     Qnil if no searching has been done yet.  */
  Lisp_Object m_last_thing_searched;
#define last_thing_searched (current_thread->m_last_thing_searched)

  Lisp_Object m_saved_last_thing_searched;
#define saved_last_thing_searched (current_thread->m_saved_last_thing_searched)

  /* The thread's name.  */
  Lisp_Object name;

  /* The thread's function.  */
  Lisp_Object function;

  /* If non-nil, this thread has been signalled.  */
  Lisp_Object error_symbol;
  Lisp_Object error_data;

  /* If we are waiting for some event, this holds the object we are
     waiting on.  */
  Lisp_Object event_object;

  /* m_gcprolist must be the first non-lisp field.  */
  /* Recording what needs to be marked for gc.  */
  struct gcpro *m_gcprolist;
#define gcprolist (current_thread->m_gcprolist)

  /* A list of currently active byte-code execution value stacks.
     Fbyte_code adds an entry to the head of this list before it starts
     processing byte-code, and it removed the entry again when it is
     done.  Signalling an error truncates the list analoguous to
     gcprolist.  */
  struct byte_stack *m_byte_stack_list;
#define byte_stack_list (current_thread->m_byte_stack_list)

  /* An address near the bottom of the stack.
     Tells GC how to save a copy of the stack.  */
  char *m_stack_bottom;
#define stack_bottom (current_thread->m_stack_bottom)

  /* An address near the top of the stack.  */
  char *stack_top;

  struct backtrace *m_backtrace_list;
#define backtrace_list (current_thread->m_backtrace_list)

  struct catchtag *m_catchlist;
#define catchlist (current_thread->m_catchlist)

  /* Chain of condition handlers currently in effect.
     The elements of this chain are contained in the stack frames
     of Fcondition_case and internal_condition_case.
     When an error is signaled (by calling Fsignal, below),
     this chain is searched for an element that applies.  */
  struct handler *m_handlerlist;
#define handlerlist (current_thread->m_handlerlist)

  /* Count levels of GCPRO to detect failure to UNGCPRO.  */
  int m_gcpro_level;
#define gcpro_level (current_thread->m_gcpro_level)

  /* Current number of specbindings allocated in specpdl.  */
  ptrdiff_t m_specpdl_size;
#define specpdl_size (current_thread->m_specpdl_size)

  /* Pointer to beginning of specpdl.  */
  struct specbinding *m_specpdl;
#define specpdl (current_thread->m_specpdl)

  /* Pointer to first unused element in specpdl.  */
  struct specbinding *m_specpdl_ptr;
#define specpdl_ptr (current_thread->m_specpdl_ptr)

  /* Pointer to the first "saved" element in specpdl.  When this
     thread is swapped out, the current values of all specpdl bindings
     are pushed onto the specpdl; then these are popped again when
     switching back to this thread.  */
  struct specbinding *m_saved_specpdl_ptr;

  /* Depth in Lisp evaluations and function calls.  */
  EMACS_INT m_lisp_eval_depth;
#define lisp_eval_depth (current_thread->m_lisp_eval_depth)

  /* This points to the current buffer.  */
  struct buffer *m_current_buffer;
#define current_buffer (current_thread->m_current_buffer)

  /* Every call to re_match, etc., must pass &search_regs as the regs
     argument unless you can show it is unnecessary (i.e., if re_match
     is certainly going to be called again before region-around-match
     can be called).

     Since the registers are now dynamically allocated, we need to make
     sure not to refer to the Nth register before checking that it has
     been allocated by checking search_regs.num_regs.

     The regex code keeps track of whether it has allocated the search
     buffer using bits in the re_pattern_buffer.  This means that whenever
     you compile a new pattern, it completely forgets whether it has
     allocated any registers, and will allocate new registers the next
     time you call a searching or matching function.  Therefore, we need
     to call re_set_registers after compiling a new pattern or after
     setting the match registers, so that the regex functions will be
     able to free or re-allocate it properly.  */
  struct re_registers m_search_regs;
#define search_regs (current_thread->m_search_regs)

  /* If non-zero the match data have been saved in saved_search_regs
     during the execution of a sentinel or filter. */
  int m_search_regs_saved;
#define search_regs_saved (current_thread->m_search_regs_saved)

  struct re_registers m_saved_search_regs;
#define saved_search_regs (current_thread->m_saved_search_regs)

  /* This is the string or buffer in which we
     are matching.  It is used for looking up syntax properties.  */
  Lisp_Object m_re_match_object;
#define re_match_object (current_thread->m_re_match_object)

  /* Set by `re_set_syntax' to the current regexp syntax to recognize.  Can
     also be assigned to arbitrarily: each pattern buffer stores its own
     syntax, so it can be changed between regex compilations.  */
  reg_syntax_t m_re_syntax_options;
#define re_syntax_options (current_thread->m_re_syntax_options)

  /* Regexp to use to replace spaces, or NULL meaning don't.  */
  /*re_char*/ unsigned char *m_whitespace_regexp;
#define whitespace_regexp (current_thread->m_whitespace_regexp)

  /* This variable is different from waiting_for_input in keyboard.c.
     It is used to communicate to a lisp process-filter/sentinel (via the
     function Fwaiting_for_user_input_p) whether Emacs was waiting
     for user-input when that process-filter was called.
     waiting_for_input cannot be used as that is by definition 0 when
     lisp code is being evalled.
     This is also used in record_asynch_buffer_change.
     For that purpose, this must be 0
     when not inside wait_reading_process_output.  */
  int m_waiting_for_user_input_p;
#define waiting_for_user_input_p (current_thread->m_waiting_for_user_input_p)

  /* The OS identifier for this thread.  */
  sys_thread_t thread_id;

  /* The condition variable for this thread.  This is associated with
     the global lock.  This thread broadcasts to it when it exits.  */
  sys_cond_t thread_condvar;

  /* This thread might be waiting for some condition.  If so, this
     points to the condition.  If the thread is interrupted, the
     interrupter should broadcast to this condition.  */
  sys_cond_t *wait_condvar;

  /* Threads are kept on a linked list.  */
  struct thread_state *next_thread;
};

struct Lisp_Mutex
{
  struct vectorlike_header header;

  Lisp_Object name;

  lisp_mutex_t mutex;
};

extern struct thread_state *current_thread;

extern sys_mutex_t global_lock;
extern void post_acquire_global_lock (struct thread_state *);

extern void unmark_threads (void);
extern void finalize_one_thread (struct thread_state *state);
extern void finalize_one_mutex (struct Lisp_Mutex *);

extern void init_threads_once (void);
extern void init_threads (void);
extern void syms_of_threads (void);

typedef int select_func (int, SELECT_TYPE *, SELECT_TYPE *, SELECT_TYPE *,
			 EMACS_TIME *, sigset_t *);

int thread_select  (select_func *func, int max_fds, SELECT_TYPE *rfds,
		    SELECT_TYPE *wfds, SELECT_TYPE *efds, EMACS_TIME *timeout,
		    sigset_t *sigmask);

#endif /* THREAD_H */
