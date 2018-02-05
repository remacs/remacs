;;; lists-tests.el --- Tests for lists.rs

;;; Code:

(require 'ert)

(ert-deftest lists-test--nth-base ()
  (should (eq (nth 0 '(a b c)) 'a))
  (should (eq (nth 1 '(a b c)) 'b))
  (should (eq (nth 2 '(a b c)) 'c))
  (should (eq (nth 3 '(a b c)) nil))
  (should (eq (nth -1 '(a b c)) 'a))
  )

(ert-deftest lists-test--nthcdr-base ()
  (should (eq (nth 0 '(a b c)) 'a))
  (should (eq (nth 1 '(a b c)) 'b))
  (should (eq (nth 2 '(a b c)) 'c))
  (should (eq (nth 3 '(a b c)) nil))
  (should (eq (nth -1 '(a b c)) 'a))
  )

(provide 'rust-lists-tests)
;;; lists-tests.el ends here
