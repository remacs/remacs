;;; lcms-tests.el --- tests for Little CMS interface -*- lexical-binding: t -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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

;; Some "exact" values computed using the colorspacious python library
;; written by Nathaniel J. Smith.  See
;; https://colorspacious.readthedocs.io/en/v1.1.0/

;; Other references:
;; http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf

;;; Code:

(require 'ert)
(require 'color)

(defconst lcms-colorspacious-d65 '(0.95047 1.0 1.08883)
  "D65 white point from colorspacious.")

(defun lcms-approx-p (a b &optional delta)
  "Check if A and B are within relative error DELTA of one another.
B is considered the exact value."
  (> (or delta 0.001) (abs (1- (/ a b)))))

(defun lcms-triple-approx-p (a b &optional delta)
  "Like `lcms-approx-p' except for color triples."
  (pcase-let ((`(,a1 ,a2 ,a3) a)
              (`(,b1 ,b2 ,b3) b))
   (and (lcms-approx-p a1 b1 delta)
        (lcms-approx-p a2 b2 delta)
        (lcms-approx-p a3 b3 delta))))

(ert-deftest lcms-cri-cam02-ucs ()
  "Test use of `lcms-cam02-ucs'."
  (should-error (lcms-cam02-ucs '(0 0 0) '(0 0 0) "error"))
  (should-error (lcms-cam02-ucs '(0 0 0) 'error))
  (should-not
   (lcms-approx-p
    (let ((lcms-d65-xyz '(0.44757 1.0 0.40745)))
      (lcms-cam02-ucs '(0.5 0.5 0.5) '(0 0 0)))
    (lcms-cam02-ucs '(0.5 0.5 0.5) '(0 0 0))))
  (should (eql 0.0 (lcms-cam02-ucs '(0.5 0.5 0.5) '(0.5 0.5 0.5))))
  (should
   (lcms-approx-p (lcms-cam02-ucs lcms-colorspacious-d65
                                  '(0 0 0)
                                  lcms-colorspacious-d65)
                  100.0)))

(ert-deftest lcms-whitepoint ()
  "Test use of `lcms-temp->white-point'."
  (should-error (lcms-temp->white-point 3999))
  (should-error (lcms-temp->white-point 25001))
  ;; D55
  (should
   (lcms-triple-approx-p
    (apply #'color-xyz-to-xyy (lcms-temp->white-point 5503))
    '(0.33242 0.34743 1.0)))
  ;; D65
  (should
   (lcms-triple-approx-p
    (apply #'color-xyz-to-xyy (lcms-temp->white-point 6504))
    '(0.31271 0.32902 1.0)))
  ;; D75
  (should
   (lcms-triple-approx-p
    (apply #'color-xyz-to-xyy (lcms-temp->white-point 7504))
    '(0.29902 0.31485 1.0))))

;;; lcms-tests.el ends here
