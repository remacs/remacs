;;; obarray-tests.el --- Tests for obarray -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Przemysław Wojnowski <esperanto@cumego.com>

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

;;; Code:

(require 'obarray)
(require 'ert)

(ert-deftest obarray-p-test ()
  "Should assert that given object is an obarray."
  (should-not (obarray-p 42))
  (should-not (obarray-p "aoeu"))
  (should-not (obarray-p '()))
  (should-not (obarray-p []))
  (should (obarray-p (make-vector 7 0))))

(ert-deftest obarray-p-unchecked-content-test ()
  "Should fail to check content of passed obarray."
  :expected-result :failed
  (should-not (obarray-p ["a" "b" "c"]))
  (should-not (obarray-p [1 2 3])))

(ert-deftest obarray-make-default-test ()
  (let ((table (obarray-make)))
    (should (obarray-p table))
    (should (equal (make-vector 59 0) table))))

(ert-deftest obarray-make-with-size-test ()
  (should-error (obarray-make -1) :type 'wrong-type-argument)
  (should-error (obarray-make 0) :type 'wrong-type-argument)
  (let ((table (obarray-make 1)))
    (should (obarray-p table))
    (should (equal (make-vector 1 0) table))))

(ert-deftest obarray-get-test ()
  (let ((table (obarray-make 3)))
    (should-not (obarray-get table "aoeu"))
    (intern "aoeu" table)
    (should (string= "aoeu" (obarray-get table "aoeu")))))

(ert-deftest obarray-put-test ()
  (let ((table (obarray-make 3)))
    (should-not (obarray-get table "aoeu"))
    (should (string= "aoeu" (obarray-put table "aoeu")))
    (should (string= "aoeu" (obarray-get table "aoeu")))))

(ert-deftest obarray-remove-test ()
  (let ((table (obarray-make 3)))
    (should-not (obarray-get table "aoeu"))
    (should-not (obarray-remove table "aoeu"))
    (should (string= "aoeu" (obarray-put table "aoeu")))
    (should (string= "aoeu" (obarray-get table "aoeu")))
    (should (obarray-remove table "aoeu"))
    (should-not (obarray-get table "aoeu"))))

(ert-deftest obarray-foreach-test ()
  "Should execute function on all elements of obarray."
  (let* ((table (obarray-make 3))
         (syms '())
         (collect-names (lambda (sym) (push (symbol-name sym) syms))))
    (obarray-foreach collect-names table)
    (should (null syms))
    (obarray-put table "a")
    (obarray-put table "b")
    (obarray-put table "c")
    (obarray-foreach collect-names table)
    (should (equal (sort syms #'string<) '("a" "b" "c")))))

(provide 'obarray-tests)
;;; obarray-tests.el ends here
