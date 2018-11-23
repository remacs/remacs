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

(ert-deftest test-forward-word ()
  (let ((str "test forward-word"))
    (with-temp-buffer
      (should (eq (forward-word -1) nil))           
      (insert str)
      (goto-char (point-min))
      (forward-word)
      (should (looking-back "test"))
      (forward-word 2)
      (should (looking-back "forward-word"))
      (should (eq (forward-word 1) nil)))))
