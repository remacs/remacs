;;; let-alist.el --- tests for file handling. -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

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

(require 'ert)

(ert-deftest let-alist-surface-test ()
  "Tests basic macro expansion for `let-alist'."
  (should
   (equal '(let ((symbol data))
             (let ((.test-one (cdr (assq 'test-one symbol)))
                   (.test-two (cdr (assq 'test-two symbol))))
               (list .test-one .test-two
                     .test-two .test-two)))
          (cl-letf (((symbol-function #'gensym) (lambda (x) 'symbol)))
            (macroexpand
             '(let-alist data (list .test-one .test-two
                                    .test-two .test-two)))))))

(defvar let-alist--test-counter 0
  "Used to count number of times a function is called.")

(ert-deftest let-alist-evaluate-once ()
  "Check that the alist argument is only evaluated once."
  (let ((let-alist--test-counter 0))
    (should
     (equal
      (let-alist (list
                  (cons 'test-two (cl-incf let-alist--test-counter))
                  (cons 'test-three (cl-incf let-alist--test-counter)))
        (list .test-one .test-two .test-two .test-three .cl-incf))
      '(nil 1 1 2 nil)))))


;;; let-alist.el ends here
