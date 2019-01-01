;;; cl-print-tests.el --- Test suite for the cl-print facility.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2019 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

(cl-defstruct cl-print--test a b)

(ert-deftest cl-print-tests-1 ()
  "Test cl-print code."
  (let ((x (make-cl-print--test :a 1 :b 2)))
    (let ((print-circle nil))
      (should (equal (cl-prin1-to-string `((x . ,x) (y . ,x)))
                     "((x . #s(cl-print--test :a 1 :b 2)) (y . #s(cl-print--test :a 1 :b 2)))")))
    (let ((print-circle t))
      (should (equal (cl-prin1-to-string `((x . ,x) (y . ,x)))
                     "((x . #1=#s(cl-print--test :a 1 :b 2)) (y . #1#))")))
    (should (string-match "\\`#f(compiled-function (x) \"[^\"]+\" [^\)]*)\\'"
                          (cl-prin1-to-string (symbol-function #'caar))))))

(ert-deftest cl-print-tests-2 ()
  (let ((x (record 'foo 1 2 3)))
    (should (equal
             x
             (car (read-from-string (with-output-to-string (prin1 x))))))
    (let ((print-circle t))
      (should (string-match
               "\\`(#1=#s(foo 1 2 3) #1#)\\'"
               (cl-prin1-to-string (list x x)))))))

(cl-defstruct (cl-print-tests-struct
               (:constructor cl-print-tests-con))
  a b c d e)

(ert-deftest cl-print-tests-3 ()
  "CL printing observes `print-length'."
  (let ((long-list (make-list 5 'a))
        (long-vec (make-vector 5 'b))
        (long-struct (cl-print-tests-con))
        (print-length 4))
    (should (equal "(a a a a ...)" (cl-prin1-to-string long-list)))
    (should (equal "[b b b b ...]" (cl-prin1-to-string long-vec)))
    (should (equal "#s(cl-print-tests-struct :a nil :b nil :c nil :d nil ...)"
                   (cl-prin1-to-string long-struct)))))

(ert-deftest cl-print-tests-4 ()
  "CL printing observes `print-level'."
  (let ((deep-list '(a (b (c (d (e))))))
        (deep-struct (cl-print-tests-con))
        (print-level 4))
    (setf (cl-print-tests-struct-a deep-struct) deep-list)
    (should (equal "(a (b (c (d ...))))" (cl-prin1-to-string deep-list)))
    (should (equal "#s(cl-print-tests-struct :a (a (b (c ...))) :b nil :c nil :d nil :e nil)"
                   (cl-prin1-to-string deep-struct)))))

(ert-deftest cl-print-tests-5 ()
  "CL printing observes `print-quoted'."
  (let ((quoted-stuff '('a #'b `(,c ,@d))))
    (let ((print-quoted t))
      (should (equal "('a #'b `(,c ,@d))"
                     (cl-prin1-to-string quoted-stuff))))
    (let ((print-quoted nil))
      (should (equal "((quote a) (function b) (\\` ((\\, c) (\\,@ d))))"
                     (cl-prin1-to-string quoted-stuff))))))

(ert-deftest cl-print-circle ()
  (let ((x '(#1=(a . #1#) #1#)))
    (let ((print-circle nil))
      (should (string-match "\\`((a . #[0-9]) (a . #[0-9]))\\'"
                            (cl-prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(#1=(a . #1#) #1#)" (cl-prin1-to-string x))))))

(ert-deftest cl-print-circle-2 ()
  ;; Bug#31146.
  (let ((x '(0 . #1=(0 . #1#))))
    (let ((print-circle nil))
      (should (string-match "\\`(0 0 . #[0-9])\\'"
                            (cl-prin1-to-string x))))
    (let ((print-circle t))
      (should (equal "(0 . #1=(0 . #1#))" (cl-prin1-to-string x))))))


;;; cl-print-tests.el ends here.
