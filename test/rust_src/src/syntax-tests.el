;;; syntax-tests.el --- tests for syntax.rs functions

;;; Code:

(require 'ert)

(ert-deftest test-syntax-table ()
  (should (syntax-table-p (syntax-table))))

(ert-deftest test-set-syntax-table ()
  (with-temp-buffer
    (let ((st (make-syntax-table)))
      (set-syntax-table st)
      (should (eq st (syntax-table))))))