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

#ifndef SYSTHREAD_H
#define SYSTHREAD_H

#ifdef HAVE_PTHREAD

#include <pthread.h>

/* A system mutex is just a pthread mutex.  This is only used for the
   GIL.  */
typedef pthread_mutex_t sys_mutex_t;

typedef pthread_cond_t sys_cond_t;

/* A system thread.  */
typedef pthread_t sys_thread_t;

#else

#error port me

#endif

typedef void *(thread_creation_function) (void *);

extern void sys_mutex_init (sys_mutex_t *);
extern void sys_mutex_lock (sys_mutex_t *);
extern void sys_mutex_unlock (sys_mutex_t *);
extern void sys_mutex_destroy (sys_mutex_t *);

extern void sys_cond_init (sys_cond_t *);
extern void sys_cond_wait (sys_cond_t *, sys_mutex_t *);
extern void sys_cond_signal (sys_cond_t *);
extern void sys_cond_broadcast (sys_cond_t *);
extern void sys_cond_destroy (sys_cond_t *);

extern sys_thread_t sys_thread_self (void);
extern int sys_thread_equal (sys_thread_t, sys_thread_t);

extern int sys_thread_create (sys_thread_t *, thread_creation_function *,
			      void *);

extern void sys_thread_yield (void);

#endif /* SYSTHREAD_H */
