;;; cl-seq-tests.el --- Tests for cl-seq.el functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

;; Author: Nicolas Richard <youngfrog@members.fsf.org>

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

(require 'ert)
(require 'cl-seq)

(ert-deftest cl-union-test-00 ()
  (let ((str1 "foo")
        (str2 (make-string 3 ?o)))
    ;; Emacs may make two string literals eql when reading.
    (aset str2 0 ?f)
    (should (not (eql str1 str2)))
    (should (equal str1 str2))
    (should (equal (cl-union (list str1) (list str2))
                   (list str2)))
    (should (equal (cl-union (list str1) (list str2) :test 'eql)
                   (list str1 str2)))))

(provide 'cl-seq-tests)
;;; cl-seq-tests.el ends here
