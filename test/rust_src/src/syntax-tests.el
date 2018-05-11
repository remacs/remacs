;;; syntax-tests.el --- tests for syntax.rs functions

;;; Code:

(require 'ert)

(ert-deftest test-syntax-table ()
  (should (syntax-table-p (syntax-table))))
