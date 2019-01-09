;;; eval-tests.el --- unit tests for src/eval.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;;; Commentary:

;; Unit tests for src/eval.c.

;;; Code:

(require 'ert)

(defvar byte-compile-debug)

(ert-deftest eval-tests--bugs-24912-and-24913 ()
  "Check that Emacs doesn't accept weird argument lists.
Bug#24912 and Bug#24913."
  (dolist (args '((&optional) (&rest) (&optional &rest) (&rest &optional)
                  (&optional &rest a) (&optional a &rest)
                  (&rest a &optional) (&rest &optional a)
                  (&optional &optional) (&optional &optional a)
                  (&optional a &optional b)
                  (&rest &rest) (&rest &rest a)
                  (&rest a &rest b)))
    (should-error (eval `(funcall (lambda ,args)) t) :type 'invalid-function)
    (should-error (byte-compile-check-lambda-list args))
    (let ((byte-compile-debug t))
      (should-error (eval `(byte-compile (lambda ,args)) t)))))

(defun eval-tests--exceed-specbind-limit ()
  (defvar eval-tests--var1)
  (defvar eval-tests--var2)
  ;; Bind two variables, to make extra sure we hit the
  ;; `max-specpdl-size' limit before the `max-lisp-eval-depth' limit.
  (let ((eval-tests--var1 1)
        (eval-tests--var2 2))
    ;; Recurse until we hit the limit.
    (eval-tests--exceed-specbind-limit)))

(ert-deftest eval-exceed-specbind-with-signal-hook ()
  "Test for Bug#30481.
Check that Emacs doesn't crash when exceeding specbind limit with
`signal-hook-function' bound.  NOTE: Without the fix for
Bug#30481, this test can appear to pass, but cause a
crash/abort/malloc assert failure on the next test."
  (let ((max-specpdl-size (/ max-lisp-eval-depth 2))
        (signal-hook-function #'ignore))
    (should-error (eval-tests--exceed-specbind-limit))))

(defun eval-tests--exceed-specbind-limit ()
  (defvar eval-tests--var1)
  (defvar eval-tests--var2)
  ;; Bind two variables, to make extra sure we hit the
  ;; `max-specpdl-size' limit before the `max-lisp-eval-depth' limit.
  (let ((eval-tests--var1 1)
        (eval-tests--var2 2))
    ;; Recurse until we hit the limit.
    (eval-tests--exceed-specbind-limit)))

(ert-deftest eval-exceed-specbind-with-signal-hook ()
  "Test for Bug#30481.
Check that Emacs doesn't crash when exceeding specbind limit with
`signal-hook-function' bound.  NOTE: Without the fix for
Bug#30481, this test can appear to pass, but cause a
crash/abort/malloc assert failure on the next test."
  (let ((max-specpdl-size (/ max-lisp-eval-depth 2))
        (signal-hook-function #'ignore))
    (should-error (eval-tests--exceed-specbind-limit))))

;;; eval-tests.el ends here
