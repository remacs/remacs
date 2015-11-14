/* Filesystem notifications support with kqueue API.
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

#ifdef HAVE_KQUEUE
#include <stdio.h>
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/file.h>
#include "lisp.h"
#include "keyboard.h"
#include "process.h"


/* File handle for kqueue.  */
static int kqueuefd = -1;

/* This is a list, elements are (DESCRIPTOR FILE FLAGS CALLBACK [DIRLIST])  */
static Lisp_Object watch_list;

/* Generate a file notification event.  */
static void
kqueue_generate_event
(Lisp_Object ident, Lisp_Object actions, Lisp_Object file, Lisp_Object callback)
{
  struct input_event event;
  EVENT_INIT (event);
  event.kind = FILE_NOTIFY_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list2 (Fcons (ident, Fcons (actions, Fcons (file, Qnil))),
		     callback);

  /* Store it into the input event queue.  */
  kbd_buffer_store_event (&event);
}

/* This compares two directory listings in case of a `write' event for
   a directory.  The old directory listing is stored in watch_object,
   it will be replaced by a new directory listing at the end.  */
static void
kqueue_compare_dir_list (Lisp_Object watch_object)
{
  Lisp_Object dir, callback, old_dl, new_dl, dl, actions;

  dir = XCAR (XCDR (watch_object));
  callback = XCAR (XCDR (XCDR (XCDR (watch_object))));
  old_dl = XCAR (XCDR (XCDR (XCDR (XCDR (watch_object)))));
  new_dl = directory_files_internal (dir, Qnil, Qnil, Qnil, 1, Qnil);

  for (dl = old_dl; ! NILP (dl); dl = XCDR (dl)) {
    Lisp_Object old_entry, new_entry;
    old_entry = XCAR (dl);
    new_entry = Fassoc (XCAR (old_entry), new_dl);

    /* We ignore "." and "..".  */
    if ((strcmp (".", SSDATA (XCAR (old_entry))) == 0) ||
	(strcmp ("..", SSDATA (XCAR (old_entry))) == 0))
      continue;

    /* A file has disappeared.  */
    if (NILP (new_entry))
      kqueue_generate_event
	(XCAR (watch_object), Fcons (Qdelete, Qnil),
	 XCAR (old_entry), callback);

    else {
      /* A file has changed.  We compare last modification time.  */
      if (NILP
	  (Fequal
	   (XCAR (XCDR (XCDR (XCDR (XCDR (XCDR (XCDR (old_entry))))))),
	    XCAR (XCDR (XCDR (XCDR (XCDR (XCDR (XCDR (new_entry))))))))))
	kqueue_generate_event
	  (XCAR (watch_object), Fcons (Qwrite, Qnil),
	   XCAR (old_entry), callback);

      /* A file attribute has changed.  We compare last status change time.  */
      if (NILP
	  (Fequal
	   (XCAR (XCDR (XCDR (XCDR (XCDR (XCDR (XCDR (XCDR (old_entry)))))))),
	    XCAR (XCDR (XCDR (XCDR (XCDR (XCDR (XCDR (XCDR (new_entry)))))))))))
	kqueue_generate_event
	  (XCAR (watch_object), Fcons (Qattrib, Qnil),
	   XCAR (old_entry), callback);
    }
  }

  for (dl = new_dl; ! NILP (dl); dl = XCDR (dl)) {
    Lisp_Object old_entry, new_entry;
    new_entry = XCAR (dl);
    old_entry = Fassoc (XCAR (new_entry), old_dl);

    /* We ignore "." and "..".  */
    if ((strcmp (".", SSDATA (XCAR (new_entry))) == 0) ||
	(strcmp ("..", SSDATA (XCAR (new_entry))) == 0))
      continue;

    /* A new file has appeared.  */
    if (NILP (old_entry))
      kqueue_generate_event
	(XCAR (watch_object), Fcons (Qcreate, Qnil),
	 XCAR (new_entry), callback);
  }

  /* Replace directory listing with the new one.  */
  XSETCDR (XCDR (XCDR (XCDR (watch_object))), Fcons (new_dl, Qnil));
  return;
}

/* This is the callback function for arriving input on kqueuefd.  It
   shall create a Lisp event, and put it into Emacs input queue.  */
static void
kqueue_callback (int fd, void *data)
{
  for (;;) {
    struct kevent kev;
    static const struct timespec nullts = { 0, 0 };
    Lisp_Object monitor_object, watch_object, file, callback, dirp, actions;

    /* Read one event.  */
    int ret = kevent (kqueuefd, NULL, 0, &kev, 1, &nullts);
    if (ret < 1) {
      /* All events read.  */
      return;
    }

    /* Determine file name and callback function.  */
    monitor_object = make_number (kev.ident);
    watch_object = assq_no_quit (monitor_object, watch_list);

    if (CONSP (watch_object)) {
      file = XCAR (XCDR (watch_object));
      callback = XCAR (XCDR (XCDR (XCDR (watch_object))));
      dirp = XCDR (XCDR (XCDR (XCDR (watch_object))));
    }
    else
      continue;

    /* Determine event actions.  */
    actions = Qnil;
    if (kev.fflags & NOTE_DELETE)
      actions = Fcons (Qdelete, actions);
    if (kev.fflags & NOTE_WRITE) {
      if (NILP (dirp))
	actions = Fcons (Qwrite, actions);
      else
	kqueue_compare_dir_list (watch_object);
    }
    if (kev.fflags & NOTE_EXTEND)
      actions = Fcons (Qextend, actions);
    if (kev.fflags & NOTE_ATTRIB)
      actions = Fcons (Qattrib, actions);
    if (kev.fflags & NOTE_LINK)
      actions = Fcons (Qlink, actions);
    if (kev.fflags & NOTE_RENAME)
      actions = Fcons (Qrename, actions);

    /* Construct an event.  */
    if (! NILP (actions))
      kqueue_generate_event (monitor_object, actions, file, callback);

    /* Cancel monitor if file or directory is deleted.  */
    if (kev.fflags & (NOTE_DELETE | NOTE_RENAME))
      Fkqueue_rm_watch (monitor_object);
  }
  return;
}

DEFUN ("kqueue-add-watch", Fkqueue_add_watch, Skqueue_add_watch, 3, 3, 0,
       doc: /* Add a watch for filesystem events pertaining to FILE.

This arranges for filesystem events pertaining to FILE to be reported
to Emacs.  Use `kqueue-rm-watch' to cancel the watch.

Returned value is a descriptor for the added watch.  If the file cannot be
watched for some reason, this function signals a `file-notify-error' error.

FLAGS is a list of events to be watched for.  It can include the
following symbols:

  `create' -- FILE was created
  `delete' -- FILE was deleted
  `write'  -- FILE has changed
  `extend' -- FILE was extended
  `attrib' -- a FILE attribute was changed
  `link'   -- a FILE's link count was changed
  `rename' -- FILE was moved to FILE1

When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTIONS FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTIONS is a list of events.

FILE is the name of the file whose event is being reported.  FILE1
will be reported only in case of the `rename' event.  */)
  (Lisp_Object file, Lisp_Object flags, Lisp_Object callback)
{
  Lisp_Object watch_object, dir_list;
  int fd;
  u_short fflags = 0;
  struct kevent ev;

  /* Check parameters.  */
  CHECK_STRING (file);
  file = Fdirectory_file_name (Fexpand_file_name (file, Qnil));
  if (NILP (Ffile_exists_p (file)))
    report_file_error ("File does not exist", file);

  CHECK_LIST (flags);

  if (! FUNCTIONP (callback))
    wrong_type_argument (Qinvalid_function, callback);

  if (kqueuefd < 0)
    {
      /* Create kqueue descriptor.  */
      kqueuefd = kqueue ();
      if (kqueuefd < 0)
	report_file_notify_error ("File watching is not available", Qnil);

      /* Start monitoring for possible I/O.  */
      add_read_fd (kqueuefd, kqueue_callback, NULL);

      watch_list = Qnil;
    }

  /* Open file.  */
  file = ENCODE_FILE (file);
  fd = emacs_open (SSDATA (file), O_RDONLY, 0);
  if (fd == -1)
    report_file_error ("File cannot be opened", file);

  /* Assemble filter flags  */
  if (! NILP (Fmember (Qdelete, flags))) fflags |= NOTE_DELETE;
  if (! NILP (Fmember (Qwrite, flags)))  fflags |= NOTE_WRITE;
  if (! NILP (Fmember (Qextend, flags))) fflags |= NOTE_EXTEND;
  if (! NILP (Fmember (Qattrib, flags))) fflags |= NOTE_ATTRIB;
  if (! NILP (Fmember (Qlink, flags)))   fflags |= NOTE_LINK;
  if (! NILP (Fmember (Qrename, flags))) fflags |= NOTE_RENAME;

  /* Register event.  */
  EV_SET (&ev, fd, EVFILT_VNODE, EV_ADD | EV_ENABLE | EV_CLEAR,
	  fflags, 0, NULL);

  if (kevent (kqueuefd, &ev, 1, NULL, 0, NULL) < 0) {
    emacs_close (fd);
    report_file_error ("Cannot watch file", file);
  }

  /* Store watch object in watch list.  */
  Lisp_Object watch_descriptor = make_number (fd);
  if (NILP (Ffile_directory_p (file)))
    watch_object = list4 (watch_descriptor, file, flags, callback);
  else {
    dir_list = directory_files_internal (file, Qnil, Qnil, Qnil, 1, Qnil);
    watch_object = list5 (watch_descriptor, file, flags, callback, dir_list);
  }
  watch_list = Fcons (watch_object, watch_list);

  return watch_descriptor;
}

DEFUN ("kqueue-rm-watch", Fkqueue_rm_watch, Skqueue_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `kqueue-add-watch'.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = assq_no_quit (watch_descriptor, watch_list);

  if (! CONSP (watch_object))
    xsignal2 (Qfile_notify_error, build_string ("Not a watch descriptor"),
	      watch_descriptor);

  eassert (INTEGERP (watch_descriptor));
  int fd = XINT (watch_descriptor);
  if ( fd >= 0)
    emacs_close (fd);

  /* Remove watch descriptor from watch list.  */
  watch_list = Fdelq (watch_object, watch_list);

  if (NILP (watch_list) && (kqueuefd >= 0)) {
    delete_read_fd (kqueuefd);
    emacs_close (kqueuefd);
    kqueuefd = -1;
  }

  return Qt;
}

DEFUN ("kqueue-valid-p", Fkqueue_valid_p, Skqueue_valid_p, 1, 1, 0,
       doc: /* "Check a watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `kqueue-add-watch'.

A watch can become invalid if the file or directory it watches is
deleted, or if the watcher thread exits abnormally for any other
reason.  Removing the watch by calling `kqueue-rm-watch' also makes it
invalid.  */)
     (Lisp_Object watch_descriptor)
{
  return NILP (assq_no_quit (watch_descriptor, watch_list)) ? Qnil : Qt;
}


void
globals_of_kqueue (void)
{
  watch_list = Qnil;
}

void
syms_of_kqueue (void)
{
  defsubr (&Skqueue_add_watch);
  defsubr (&Skqueue_rm_watch);
  defsubr (&Skqueue_valid_p);

  /* Event types.  */
  DEFSYM (Qcreate, "create");
  DEFSYM (Qdelete, "delete");	/* NOTE_DELETE  */
  DEFSYM (Qwrite, "write");	/* NOTE_WRITE  */
  DEFSYM (Qextend, "extend");	/* NOTE_EXTEND  */
  DEFSYM (Qattrib, "attrib");	/* NOTE_ATTRIB  */
  DEFSYM (Qlink, "link");	/* NOTE_LINK  */
  DEFSYM (Qrename, "rename");	/* NOTE_RENAME  */

  staticpro (&watch_list);

  Fprovide (intern_c_string ("kqueue"), Qnil);
}

#endif /* HAVE_KQUEUE  */

/* TODO
   * Add FILE1 in case of `rename'.  */

/* PROBLEMS
   * https://bugs.launchpad.net/ubuntu/+source/libkqueue/+bug/1514837
     prevents tests on Ubuntu.  */
