;;; alloc-tests.el ---  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest memory-use-count ()
  (let ((counts (memory-use-counts)))
    (should (= 8 (length counts)))
    (dolist (count counts)
      (should (integerp count)))))

(provide 'alloc-tests)
;;; alloc-tests.el ends here
