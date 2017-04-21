;;; image-size-tests.el -- tests for image scaling

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; To test: Load the file and eval (image-size-tests).
;; A non-erroring result is a success.

;;; Code:

(defmacro im-should (form)
  `(unless ,form
     (error "%s didn't succeed" ',form)))

(defun im-image (&rest props)
  (let ((image-scaling-factor 1))
    (apply
     #'create-image
     (expand-file-name "test/data/image/blank-200x100.png" source-directory)
     'imagemagick nil props)))

(defun im-compare (image width height)
  (let ((size (image-size image t)))
    (and (= (car size) width)
         (= (cdr size) height))))

(defun image-size-tests ()
  (unless (imagemagick-types)
    (error "This only makes sense if ImageMagick is installed"))
  ;; Default sizes.
  (im-should (im-compare (im-image) 200 100))
  ;; Changing one dimension changes the other.
  (im-should (im-compare (im-image :width 100) 100 50))
  (im-should (im-compare (im-image :height 50) 100 50))
  ;; The same with :max-width etc.
  (im-should (im-compare (im-image :max-width 100) 100 50))
  (im-should (im-compare (im-image :max-height 50) 100 50))
  ;; :width wins over :max-width etc
  (im-should (im-compare (im-image :width 300 :max-width 100) 300 150))
  (im-should (im-compare (im-image :height 200 :max-height 100) 400 200))
  ;; Specifying both width and height is fine.
  (im-should (im-compare (im-image :width 300 :height 50) 300 50))
  ;; A too-large :max-width (etc) has no effect.
  (im-should (im-compare (im-image :max-width 300) 200 100))
  (im-should (im-compare (im-image :max-height 300) 200 100))
  ;; Both max-width/height.
  (im-should (im-compare (im-image :max-width 100 :max-height 75) 100 50))
  (im-should (im-compare (im-image :max-width 100 :max-height 25) 50 25)))

;;; image-size-tests.el ends here
