;;; cl-extra-tests.el --- tests for emacs-lisp/cl-extra.el  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest cl-get ()
  (put 'cl-get-test 'x 1)
  (put 'cl-get-test 'y nil)
  (should (eq (cl-get 'cl-get-test 'x) 1))
  (should (eq (cl-get 'cl-get-test 'y :none) nil))
  (should (eq (cl-get 'cl-get-test 'z :none) :none)))

(ert-deftest cl-getf ()
  (let ((plist '(x 1 y nil)))
    (should (eq (cl-getf plist 'x) 1))
    (should (eq (cl-getf plist 'y :none) nil))
    (should (eq (cl-getf plist 'z :none) :none))))

;;; cl-extra-tests.el ends here
