;;; find-func-tests.el --- Test suite for find-func library.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'find-func)

(defun find-func-tests-set-up-point-for-buf (bufname &rest body)
  "Move point to the start of the buffer for each test run.
BUFNAME is the name of the buffer to reset.
BODY forms will be executed after point has been repositioned."
  (declare (indent 1))
  `(save-excursion
     (when-let (buf (get-buffer ,bufname))
       (with-current-buffer buf
         (goto-char (point-min))))
     ,@body))

(ert-deftest find-func-tests-error-if-rust-source-does-not-exist ()
  "Check an error is raised if the specified Rust source file does not exist."
  (should-error (find-function-search-for-symbol 'setcar nil "rust_src/foo.rs")))

(ert-deftest find-func-tests-error-if-function-not-defined ()
  "Check an error is raised if there is no such defun in the target file."
  (should-error (find-function-search-for-symbol 'foo nil "rust_src/cons.rs")))

(ert-deftest find-func-tests-finds-function-def-in-rust-source-file ()
  "Check that the definition location can be found for Rust functions."
  (find-func-tests-set-up-point-for-buf "lists.rs"
    (let* ((result (find-function-search-for-symbol 'setcar nil "rust_src/src/lists.rs"))
           (buffer (car result))
           (location (cdr result)))

      (should (equal "lists.rs" (buffer-name buffer)))
      (should (numberp location))

      (with-current-buffer buffer
        (goto-char location)
        (let ((line (buffer-substring-no-properties (point) (line-end-position))))
          (should (string-match-p (rx bol "fn setcar") line)))))))

(ert-deftest find-func-tests-finds-function-def-in-C-source-file ()
  "Check that the definition location can be found for C functions.
If the function being tested is ported to Rust, this test will need to be updated."
  (find-func-tests-set-up-point-for-buf "eval.c"
    (let* ((result (find-function-search-for-symbol 'cons nil "src/alloc.c"))
           (buffer (car result))
           (location (cdr result)))

      (should (equal "alloc.c" (buffer-name buffer)))
      (should (numberp location))

      (with-current-buffer buffer
        (goto-char location)
        (let ((line (buffer-substring-no-properties (point) (line-end-position))))
          (should (string-match-p (rx bol "DEFUN" (* space) "(\"cons\"") line)))))))

(ert-deftest find-func-tests-finds-function-def-in-lisp-source-file ()
  "Check that the definition location can be found for lisp functions."
  (find-func-tests-set-up-point-for-buf "find-func.el"
    (let* ((result (find-function-search-for-symbol 'find-function nil "find-func.el"))
           (buffer (car result))
           (location (cdr result)))

      (should (string-match-p (rx "find-func.el" (? ".gz")) (buffer-name buffer)))
      (should (numberp location))

      (with-current-buffer buffer
        (goto-char location)
        (let ((line (buffer-substring-no-properties (point) (line-end-position))))
          (should (string-match-p (rx bol "(defun" (+ space) "find-function") line)))))))

(provide 'find-func-tests)

;;; find-func-tests.el ends here
