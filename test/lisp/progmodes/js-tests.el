;;; js-tests.el --- Test suite for js-mode

;; Copyright (C) 2017 Free Software Foundation, Inc.

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
(require 'js)

(ert-deftest js-mode-fill-bug-19399 ()
  (with-temp-buffer
    (insert "/")
    (save-excursion (insert "/ comment"))
    (js-mode)
    (fill-paragraph)
    (should (equal (buffer-substring (point-min) (point-max))
                   "// comment"))))

(ert-deftest js-mode-fill-bug-22431 ()
  (with-temp-buffer
    (insert "/**\n")
    (insert " * Load the inspector's shared head.js for use by tests that ")
    (insert "need to open the something or other")
    (js-mode)
    ;; This fails with auto-fill but not fill-paragraph.
    (do-auto-fill)
    (should (equal (buffer-substring (point-min) (point-max))
                   "/**
 * Load the inspector's shared head.js for use by tests that need to
 * open the something or other"))))

(ert-deftest js-mode-fill-bug-22431-fill-paragraph-at-start ()
  (with-temp-buffer
    (insert "/**\n")
    (insert " * Load the inspector's shared head.js for use by tests that ")
    (insert "need to open the something or other")
    (js-mode)
    (goto-char (point-min))
    (fill-paragraph)
    (should (equal (buffer-substring (point-min) (point-max))
                   "/**
 * Load the inspector's shared head.js for use by tests that need to
 * open the something or other"))))

(provide 'js-tests)

;;; js-tests.el ends here
