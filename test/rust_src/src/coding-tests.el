(require 'ert)

(ert-deftest coding-system-p ()
  (should (eq t (coding-system-p nil)))
  (should (eq t (coding-system-p 'utf-8)))
  (should (eq nil (coding-system-p "utf-8")))
  (should (eq nil (coding-system-p 'test-coding)))
  (put 'test-coding 'coding-system-define-form "test coding")
  (should (eq t (coding-system-p 'test-coding))))
