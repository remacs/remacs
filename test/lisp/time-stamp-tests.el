;;; time-stamp-tests.el --- tests for time-stamp.el -*- lexical-binding: t -*-

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
(require 'time-stamp)

(defun test-stamp (ts-format time)
  (time-stamp--format (time-stamp-string-preprocess ts-format time) time))

(ert-deftest test-time-stamp ()
  (let ((user-login-name "foo")
        (time-stamp-time-zone t)
	(time '(23847 24475 657815 318000))
	(format "%:y-%02m-%02d %02H:%02M:%02S %u"))
    (should (equal (test-stamp format time)
                   "2019-07-11 16:11:07 foo"))))

;;; time-stamp-tests.el ends here
