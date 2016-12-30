;;; ffap-tests.el --- Test suite for ffap.el -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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
(require 'ffap)

(ert-deftest ffap-tests-25243 ()
  "Test for http://debbugs.gnu.org/25243 ."
  (let ((file (make-temp-file "test-Bug#25243")))
    (unwind-protect
        (with-temp-file file
          (let ((str "diff --git b/lisp/ffap.el a/lisp/ffap.el
index 3d7cebadcf..ad4b70d737 100644
--- b/lisp/ffap.el
+++ a/lisp/ffap.el
@@ -203,6 +203,9 @@ ffap-foo-at-bar-prefix
"))
            (transient-mark-mode 1)
            (when (natnump ffap-max-region-length)
              (insert
               (concat
                str
                (make-string ffap-max-region-length #xa)
                (format "%s ENDS HERE" file)))
              (mark-whole-buffer)
              (should (equal "" (ffap-string-at-point)))
              (should (equal '(1 1) ffap-string-at-point-region)))))
      (and (file-exists-p file) (delete-file file)))))

(provide 'ffap-tests)

;;; ffap-tests.el ends here
