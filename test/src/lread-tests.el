;;; lread-tests.el --- tests for lread.c -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

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
  (should (equal (read "?\\N{U+A817}") #xA817)))

(ert-deftest lread-char-name ()
  (should (equal (read "?\\N{SYLOTI  NAGRI LETTER \n DHO}")
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

(ert-deftest lread-char-surrogate-1 ()
  (should-error (read "?\\N{U+D800}") :type 'invalid-read-syntax))
(ert-deftest lread-char-surrogate-2 ()
  (should-error (read "?\\N{U+D801}") :type 'invalid-read-syntax))
(ert-deftest lread-char-surrogate-3 ()
  (should-error (read "?\\N{U+Dffe}") :type 'invalid-read-syntax))
(ert-deftest lread-char-surrogate-4 ()
  (should-error (read "?\\N{U+DFFF}") :type 'invalid-read-syntax))

(ert-deftest lread-string-char-number-1 ()
  (should (equal (read "a\\N{U+A817}b") "a\uA817bx")))
(ert-deftest lread-string-char-number-2 ()
  (should-error (read "?\\N{0.5}") :type 'invalid-read-syntax))
(ert-deftest lread-string-char-number-3 ()
  (should-error (read "?\\N{U+-0}") :type 'invalid-read-syntax))

(ert-deftest lread-string-char-name ()
  (should (equal (read "a\\N{SYLOTI NAGRI  LETTER DHO}b") "a\uA817b")))

;;; lread-tests.el ends here
