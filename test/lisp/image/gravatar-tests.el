;;; gravatar-tests.el --- tests for gravatar.el -*- lexical-binding: t -*-

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
(require 'gravatar)

(ert-deftest gravatar-hash ()
  "Test `gravatar-hash'."
  (should (equal (gravatar-hash "") "d41d8cd98f00b204e9800998ecf8427e"))
  (let ((hash "acbd18db4cc2f85cedef654fccc4a4d8"))
    (should (equal (gravatar-hash "foo") hash))
    (should (equal (gravatar-hash "foo ") hash))
    (should (equal (gravatar-hash " foo") hash))
    (should (equal (gravatar-hash " foo ") hash))))

;;; gravatar-tests.el ends here
