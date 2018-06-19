;;; backtrace-tests.el --- Tests for emacs-lisp/backtrace.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'backtrace)
(require 'ert)
(require 'seq)

;; Create a backtrace frames list with several frames.
;; TODO load this from an el file in backtrace-resources/ so the tests
;; can be byte-compiled.
(defvar backtrace-tests--frames nil)

(defun backtrace-tests--func1 (arg1 arg2)
  (setq backtrace-tests--frames (backtrace-get-frames nil))
  (list arg1 arg2))

(defun backtrace-tests--func2 (arg)
  (list arg))

(defun backtrace-tests--func3 (arg)
  (let ((foo (list 'a arg 'b)))
    (list foo (backtrace-tests--func2 arg) (backtrace-tests--func1 arg 0))))

(defun backtrace-tests--create-backtrace-frames ()
  (backtrace-tests--func3 "string")
  ;; Discard frames before this one.
  (let (this-index)
    (dotimes (index (length backtrace-tests--frames))
      (when (eq (backtrace-frame-fun (nth index backtrace-tests--frames))
                'backtrace-tests--create-backtrace-frames)
        (setq this-index index)))
    (setq backtrace-tests--frames (seq-subseq backtrace-tests--frames
                                              0 (1+ this-index)))))

(backtrace-tests--create-backtrace-frames)

;; TODO check that debugger-batch-max-lines still works

(defun backtrace-tests--insert-header ()
  (insert "Test header\n"))

(defmacro backtrace-tests--with-buffer (&rest body)
  `(with-temp-buffer
     (backtrace-mode)
     (setq backtrace-frames backtrace-tests--frames)
     (setq backtrace-insert-header-function #'backtrace-tests--insert-header)
     (backtrace-print)
     ,@body))

;;; Tests
(ert-deftest backtrace-tests--to-string ()
  (should (string= (backtrace-to-string backtrace-tests--frames)
                    "  backtrace-get-frames(nil)
  (setq backtrace-tests--frames (backtrace-get-frames nil))
  backtrace-tests--func1(\"string\" 0)
  (list foo (backtrace-tests--func2 arg) (backtrace-tests--func1 arg 0))
  (let ((foo (list 'a arg 'b))) (list foo (backtrace-tests--func2 arg) (backtrace-tests--func1 arg 0)))
  backtrace-tests--func3(\"string\")
  backtrace-tests--create-backtrace-frames()
")))

(provide 'backtrace-tests)

;; These tests expect to see non-byte compiled stack frames.
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; backtrace-tests.el ends here
