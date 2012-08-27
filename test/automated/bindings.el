;;; bindings.el --- tests for variable bindings

;; Copyright (C) 2012  Free Software Foundation, Inc.

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

;;; Code:

(defvar binding-test-buffer-A (get-buffer-create "A"))
(defvar binding-test-buffer-B (get-buffer-create "B"))

(defvar binding-test-always-local 'always)
(make-variable-buffer-local 'binding-test-always-local)

(defvar binding-test-some-local 'some)
(with-current-buffer binding-test-buffer-A
  (set (make-local-variable 'binding-test-some-local) 'local))

(ert-deftest binding-test-manual ()
  "A test case from the elisp manual."
  (save-excursion
    (set-buffer binding-test-buffer-A)
    (let ((binding-test-some-local 'something-else))
      (should (eq binding-test-some-local 'something-else))
      (set-buffer binding-test-buffer-B)
      (should (eq binding-test-some-local 'some)))
    (should (eq binding-test-some-local 'some))
    (set-buffer binding-test-buffer-A)
    (should (eq binding-test-some-local 'local))))

(ert-deftest binding-test-setq-default ()
  "Test that a setq-default has no effect when there is a local binding."
  (save-excursion
    (set-buffer binding-test-buffer-B)
    ;; This variable is not local in this buffer.
    (let ((binding-test-some-local 'something-else))
      (setq-default binding-test-some-local 'new-default))
    (should (eq binding-test-some-local 'some))))

(ert-deftest binding-test-makunbound ()
  "Tests of makunbound, from the manual."
  (save-excursion
    (set-buffer binding-test-buffer-B)
    (should (boundp 'binding-test-some-local))
    (let ((binding-test-some-local 'outer))
      (let ((binding-test-some-local 'inner))
	(makunbound 'binding-test-some-local)
	(should (not (boundp 'binding-test-some-local))))
      (should (and (boundp 'binding-test-some-local)
		   (eq binding-test-some-local 'outer))))))

(ert-deftest binding-test-defvar-bool ()
  "Test DEFVAR_BOOL"
  (let ((display-hourglass 5))
    (should (eq display-hourglass t))))

(ert-deftest binding-test-defvar-int ()
  "Test DEFVAR_INT"
  (should-error (setq gc-cons-threshold 5.0) :type 'wrong-type-argument))

(ert-deftest binding-test-set-constant-t ()
  "Test setting the constant t"
  (should-error (setq t 'bob) :type 'setting-constant))

(ert-deftest binding-test-set-constant-nil ()
  "Test setting the constant nil"
  (should-error (setq nil 'bob) :type 'setting-constant))

(ert-deftest binding-test-set-constant-keyword ()
  "Test setting a keyword constant"
  (should-error (setq :keyword 'bob) :type 'setting-constant))

(ert-deftest binding-test-set-constant-nil ()
  "Test setting a keyword to itself"
  (should (setq :keyword :keyword)))

;; More tests to write -
;; kill-local-variable
;; defconst; can modify
;; defvar and defconst modify the local binding [ doesn't matter for us ]
;; various kinds of special internal forwarding objects
;;   a couple examples in manual, not enough
;; frame-local vars
;; variable aliases

;;; bindings.el ends here
