;;; network-stream-tests.el --- tests for network processes       -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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

(require 'shr)

(defun shr-test (name)
  (with-temp-buffer
    (insert-file-contents (format "data/shr/%s.html" name))
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          (shr-width 80)
          (shr-use-fonts nil))
      (erase-buffer)
      (shr-insert-document dom)
      (cons (buffer-substring-no-properties (point-min) (point-max))
            (with-temp-buffer
              (insert-file-contents (format "data/shr/%s.txt" name))
              (buffer-string))))))

(ert-deftest rendering ()
  (skip-unless (fboundp 'libxml-parse-html-region))
  (dolist (file (directory-files "data/shr" nil "\\.html\\'"))
    (let* ((name (replace-regexp-in-string "\\.html\\'" "" file))
           (result (shr-test name)))
      (unless (equal (car result) (cdr result))
        (should (not (list name (car result) (cdr result))))))))

(require 'shr)

;;; shr-stream-tests.el ends here
