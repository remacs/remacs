;;; lisp-mode-tests.el --- Test Lisp editing commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

(require 'ert)
(require 'cl-lib)
(require 'lisp-mode)

(ert-deftest indent-sexp ()
  "Test basics of \\[indent-sexp]."
  (with-temp-buffer
    (insert "\
\(a
 (prog1
     (prog1
         1
       2)
   2)
 (1
  \"string
noindent\" (\"string2
noindent\" 3
4)
  2)                                    ; comment
 ;; comment
 b)")
    (goto-char (point-min))
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (dolist (mode '(fundamental-mode emacs-lisp-mode))
        (funcall mode)
        (indent-sexp)
        ;; Don't mess up correctly indented code.
        (should (string= (buffer-string) correct))
        ;; Correctly add indentation.
        (save-excursion
          (while (not (eobp))
            (delete-horizontal-space)
            (forward-line)))
        (indent-sexp)
        (should (equal (buffer-string) correct))
        ;; Correctly remove indentation.
        (save-excursion
          (let ((n 0))
            (while (not (eobp))
              (unless (looking-at "noindent")
                (insert (make-string n ?\s)))
              (cl-incf n)
              (forward-line))))
        (indent-sexp)
        (should (equal (buffer-string) correct))))))

(ert-deftest indent-subsexp ()
  "Make sure calling `indent-sexp' inside a sexp works."
  (with-temp-buffer
    (insert "\
\(d1 xx
    (d2 yy
	zz)
    11)")
    (let ((correct (buffer-string)))
      (search-backward "d2")
      (up-list -1)
      (indent-sexp)
      (should (equal (buffer-string) correct))
      (backward-sexp)
      (end-of-line)
      (indent-sexp)
      (should (equal (buffer-string) correct)))))

(ert-deftest indent-sexp-in-string ()
  "Make sure calling `indent-sexp' inside a string works."
  ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21343.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\";\"")
    (let ((correct (buffer-string)))
      (search-backward ";")
      (indent-sexp)
      (should (equal (buffer-string) correct)))))

(provide 'lisp-mode-tests)
;;; lisp-mode-tests.el ends here
