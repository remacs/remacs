;;; image-tests.el --- tests for image.el -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'image)

(ert-deftest image--set-property ()
  "Test `image--set-property' behavior."
  (let ((image (list 'image)))
    ;; Add properties.
    (setf (image-property image :scale) 1)
    (should (equal image '(image :scale 1)))
    (setf (image-property image :width) 8)
    (should (equal image '(image :scale 1 :width 8)))
    (setf (image-property image :height) 16)
    (should (equal image '(image :scale 1 :width 8 :height 16)))
    ;; Delete properties.
    (setf (image-property image :type) nil)
    (should (equal image '(image :scale 1 :width 8 :height 16)))
    (setf (image-property image :scale) nil)
    (should (equal image '(image :width 8 :height 16)))
    (setf (image-property image :height) nil)
    (should (equal image '(image :width 8)))
    (setf (image-property image :width) nil)
    (should (equal image '(image)))))

;;; image-tests.el ends here
