;;; rfc2047-tests.el --- tests for rfc2047.el -*- lexical-binding: t -*-

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
(require 'rfc2047)

(ert-deftest test-rfc2047-fold-short ()
  (with-temp-buffer
    (insert "Organization: Lots Of Short Words Here Lots Of Short Words Here Lots Of Short Words Here\n")
    (goto-char (point-min))
    (rfc2047-fold-field)
    (should (equal (buffer-string)
                   "Organization: Lots Of Short Words Here Lots Of Short Words Here Lots Of
 Short Words Here
"))))

(ert-deftest test-rfc2047-fold-encoded ()
  (with-temp-buffer
    (insert "Subject: This is =?utf-8?Q?=C3=A1?= long subject that's =?utf-8?Q?v=C3=A9ry?= long and =?utf-8?Q?ver=C3=BD?= encoded yes indeed it =?utf-8?Q?=C3=ADs?=\n")
    (goto-char (point-min))
    (rfc2047-fold-field)
    (should (equal (buffer-string)
                   "Subject: This is =?utf-8?Q?=C3=A1?= long subject that's
 =?utf-8?Q?v=C3=A9ry?= long and =?utf-8?Q?ver=C3=BD?= encoded yes indeed it
 =?utf-8?Q?=C3=ADs?=
"))))

;;; rfc2047-tests.el ends here
