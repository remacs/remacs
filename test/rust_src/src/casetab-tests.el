;;; casetab-tests.el --- Tests for casetab.rs

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest casetab-test--case-table-p ()
  (should (case-table-p (standard-case-table)))
  (should-not (case-table-p (syntax-table)))
  (should-not (case-table-p (category-table)))
  )

(provide 'casetab-tests)
;;; casetab-tests.el ends here
