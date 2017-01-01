;;; abbrev-tests.el --- Test suite for abbrevs.

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>
;; Keywords: abbrevs

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

;; `kill-all-abbrevs-test' will remove all user *and* system abbrevs
;; if called noninteractively with the init file loaded.

;;; Code:

(require 'ert)
(require 'abbrev)
(require 'seq)

;; set up test abbrev table and abbrev entry
(defun setup-test-abbrev-table ()
  (defvar ert-test-abbrevs nil)
  (define-abbrev-table 'ert-test-abbrevs '(("a-e-t" "abbrev-ert-test")))
  (abbrev-table-put ert-test-abbrevs :ert-test "ert-test-value")
  ert-test-abbrevs)

(ert-deftest copy-abbrev-table-test ()
  (defvar foo-abbrev-table nil)         ; Avoid compiler warning
  (define-abbrev-table 'foo-abbrev-table
    '())
  (should (abbrev-table-p foo-abbrev-table))
  ;; Bug 21828
  (let ((new-foo-abbrev-table
         (condition-case nil
             (copy-abbrev-table foo-abbrev-table)
           (error nil))))
    (should (abbrev-table-p new-foo-abbrev-table)))
  (should-not (string-equal (buffer-name) "*Backtrace*")))

(ert-deftest kill-all-abbrevs-test ()
  "Test undefining all defined abbrevs"
  (unless noninteractive
    (ert-skip "Cannot test kill-all-abbrevs in interactive mode"))

  (let ((num-tables 0))
    ;; ensure at least one abbrev exists
    (should (abbrev-table-p (setup-test-abbrev-table)))
    (setf num-tables (length abbrev-table-name-list))
    (kill-all-abbrevs)

    ;; no tables should have been removed/added
    (should (= num-tables (length abbrev-table-name-list)))
    ;; number of empty tables should be the same as number of tables
    (should (= num-tables (length (seq-filter
                                   (lambda (table)
                                       (abbrev-table-empty-p (symbol-value table)))
                                   abbrev-table-name-list))))))

(ert-deftest abbrev-table-name-test ()
  "Test returning name of abbrev-table"
  (let ((ert-test-abbrevs (setup-test-abbrev-table))
        (no-such-table nil))
    (should (equal 'ert-test-abbrevs (abbrev-table-name ert-test-abbrevs)))
    (should (equal nil (abbrev-table-name no-such-table)))))

(ert-deftest clear-abbrev-table-test ()
  "Test clearing single abbrev table"
  (let ((ert-test-abbrevs (setup-test-abbrev-table)))
    (should (equal "a-e-t" (symbol-name
                            (abbrev-symbol "a-e-t" ert-test-abbrevs))))
    (should (equal "abbrev-ert-test" (symbol-value
                                      (abbrev-symbol "a-e-t" ert-test-abbrevs))))

    (clear-abbrev-table ert-test-abbrevs)

    (should (equal "nil" (symbol-name
                          (abbrev-symbol "a-e-t" ert-test-abbrevs))))
    (should (equal nil (symbol-value
                        (abbrev-symbol "a-e-t" ert-test-abbrevs))))
    (should (equal t (abbrev-table-empty-p ert-test-abbrevs)))))

(provide 'abbrev-tests)

;;; abbrev-tests.el ends here
