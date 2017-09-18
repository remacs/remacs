;;; json-tests.el --- unit tests for json.c          -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

;; Unit tests for src/json.c.

;;; Code:

(require 'cl-lib)
(require 'map)

(ert-deftest json-serialize/roundtrip ()
  (let ((lisp [nil :json-false t 0 123 -456 3.75 "foo"])
        (json "[null,false,true,0,123,-456,3.75,\"foo\"]"))
    (should (equal (json-serialize lisp) json))
    (with-temp-buffer
      (json-insert lisp)
      (should (equal (buffer-string) json))
      (should (eobp)))
    (should (equal (json-parse-string json) lisp))
    (with-temp-buffer
      (insert json)
      (goto-char 1)
      (should (equal (json-parse-buffer) lisp))
      (should (eobp)))))

(ert-deftest json-serialize/object ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "abc" [1 2 t] table)
    (puthash "def" nil table)
    (should (equal (json-serialize table)
                   "{\"abc\":[1,2,true],\"def\":null}"))))

(ert-deftest json-parse-string/object ()
  (let ((actual
         (json-parse-string
          "{ \"abc\" : [1, 2, true], \"def\" : null, \"abc\" : [9, false] }\n")))
    (should (hash-table-p actual))
    (should (equal (hash-table-count actual) 2))
    (should (equal (cl-sort (map-pairs actual) #'string< :key #'car)
                   '(("abc" . [9 :json-false]) ("def"))))))

(provide 'json-tests)
;;; json-tests.el ends here
