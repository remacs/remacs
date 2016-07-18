;;; regex-tests.el --- tests for regex.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

(ert-deftest regex-word-cc-fallback-test ()
  "Test that ‘[[:cc:]]*x’ matches ‘x’ (bug#24020).

Test that a regex of the form \"[[:cc:]]*x\" where CC is
a character class which matches a multibyte character X, matches
string \"x\".

For example, ‘[[:word:]]*\u2620’ regex (note: \u2620 is a word
character) must match a string \"\u2420\"."
  (dolist (class '("[[:word:]]" "\\sw"))
    (dolist (repeat '("*" "+"))
      (dolist (suffix '("" "b" "bar" "\u2620"))
        (dolist (string '("" "foo"))
          (when (not (and (string-equal repeat "+")
                          (string-equal string "")))
            (should (string-match (concat "^" class repeat suffix "$")
                                  (concat string suffix)))))))))

(defun regex--test-cc (name matching not-matching)
  (should (string-match-p (concat "^[[:" name ":]]*$") matching))
  (should (string-match-p (concat "^[[:" name ":]]*?\u2622$")
                          (concat matching "\u2622")))
  (should (string-match-p (concat "^[^[:" name ":]]*$") not-matching))
  (should (string-match-p (concat "^[^[:" name ":]]*\u2622$")
                          (concat not-matching "\u2622")))
  (with-temp-buffer
    (insert matching)
    (let ((p (point)))
      (insert not-matching)
      (goto-char (point-min))
      (skip-chars-forward (concat "[:" name ":]"))
      (should (equal (point) p))
      (skip-chars-forward (concat "^[:" name ":]"))
      (should (equal (point) (point-max)))
      (goto-char (point-min))
      (skip-chars-forward (concat "[:" name ":]\u2622"))
      (should (or (equal (point) p) (equal (point) (1+ p)))))))

(ert-deftest regex-character-classes ()
  "Perform sanity test of regexes using character classes.

Go over all the supported character classes and test whether the
classes and their inversions match what they are supposed to
match.  The test is done using `string-match-p' as well as
`skip-chars-forward'."
  (let (case-fold-search)
    (regex--test-cc "alnum" "abcABC012łąka" "-, \t\n")
    (regex--test-cc "alpha" "abcABCłąka" "-,012 \t\n")
    (regex--test-cc "digit" "012" "abcABCłąka-, \t\n")
    (regex--test-cc "xdigit" "0123aBc" "łąk-, \t\n")
    (regex--test-cc "upper" "ABCŁĄKA" "abc012-, \t\n")
    (regex--test-cc "lower" "abcłąka" "ABC012-, \t\n")

    (regex--test-cc "word" "abcABC012\u2620" "-, \t\n")

    (regex--test-cc "punct" ".,-" "abcABC012\u2620 \t\n")
    (regex--test-cc "cntrl" "\1\2\t\n" ".,-abcABC012\u2620 ")
    (regex--test-cc "graph" "abcłąka\u2620-," " \t\n\1")
    (regex--test-cc "print" "abcłąka\u2620-, " "\t\n\1")

    (regex--test-cc "space" " \t\n\u2001" "abcABCł0123")
    (regex--test-cc "blank" " \t" "\n\u2001")

    (regex--test-cc "ascii" "abcABC012 \t\n\1" "łą\u2620")
    (regex--test-cc "nonascii" "łą\u2622" "abcABC012 \t\n\1")
    (regex--test-cc "unibyte" "abcABC012 \t\n\1" "łą\u2622")
    (regex--test-cc "multibyte" "łą\u2622" "abcABC012 \t\n\1")))

;;; regex-tests.el ends here
