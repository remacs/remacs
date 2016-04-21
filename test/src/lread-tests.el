;;; lread-tests.el --- tests for lread.c -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Google Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;;; Commentary:

;; Unit tests for code in src/lread.c.

;;; Code:

(ert-deftest lread-char-number ()
  (should (equal ?\N{U+A817} #xA817)))

(ert-deftest lread-char-name ()
  (should (equal ?\N{SYLOTI  NAGRI LETTER
                 DHO}
                 #xA817)))

(ert-deftest lread-char-invalid-number ()
  (should-error (read "?\\N{U+110000}") :type 'invalid-read-syntax))

(ert-deftest lread-char-invalid-name ()
  (should-error (read "?\\N{DOES NOT EXIST}")) :type 'invalid-read-syntax)

(ert-deftest lread-char-non-ascii-name ()
  (should-error (read "?\\N{LATIN CAPITAL LETTER Ø}")
                :type 'invalid-read-syntax))

(ert-deftest lread-char-empty-name ()
  (should-error (read "?\\N{}") :type 'invalid-read-syntax))

(ert-deftest lread-char-cjk-name ()
  (should (equal ?\N{CJK IDEOGRAPH-2B734} #x2B734)))

(ert-deftest lread-char-invalid-cjk-name ()
  (should-error (read "?\\N{CJK IDEOGRAPH-2B735}") :type 'invalid-read-syntax))

(ert-deftest lread-string-char-number ()
  (should (equal "a\N{U+A817}b" "a\uA817b")))

(ert-deftest lread-string-char-name ()
  (should (equal "a\N{SYLOTI NAGRI  LETTER DHO}b" "a\uA817b")))

;;; lread-tests.el ends here
