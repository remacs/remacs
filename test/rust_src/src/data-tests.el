;;; data-tests.el --- Tests for data.rs

;;; Code:

(require 'ert)

(ert-deftest data-test--aref-base ()
  "Verify (aref) base cases"
  (should-error (aref "abc" -1) :type 'args-out-of-range)
  (should (eq (aref "abcdef" 3)
              ?d))
  (should-error (aref "abc" 3)
                :type 'args-out-of-range)
  (should (eq (aref (bool-vector t nil t nil) 1)
              nil))
  (should-error (aref (bool-vector t nil t nil) 4)
                :type 'args-out-of-range)
  (let ((tbl (make-char-table 'foo 0)))
    (should (eq (aref tbl 0)
                0)))
  (let* ((x 1)
         (y 2)
         (z 3)
         (v [x y z]))
    (should (eq (aref v 1)
                'y))
    (should-error (aref v 5)
                  :type 'args-out-of-range))
  (should (eq (aref (kbd "<f1> SPC") 1)
              32))
  (let ((r (make-record 'foo 9 'Z)))
    (should (eq (aref r 1)
                'Z))
    (should-error (aref r 10)
                  :type 'args-out-of-range))
  (should-error (aref 100 1)
                :type 'wrong-type-argument))

(provide 'data-tests)
;;; chartab-tests.el ends here
