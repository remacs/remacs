;;; eval-tests.el --- unit tests for src/eval.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for src/eval.c.

;;; Code:

(require 'ert)

(ert-deftest eval-tests--bug24673 ()
  "Checks that Bug#24673 has been fixed."
  ;; This should not crash.
  (should-error (funcall '(closure)) :type 'invalid-function))

;;; eval-tests.el ends here
