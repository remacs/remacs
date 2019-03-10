;;; numbers-tests.el --- tests for numbers.rs functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-random ()
  (should (numberp (random))))

(ert-deftest test-random-t ()
  (should (numberp (random t))))

(ert-deftest test-random-seed ()
  (let* ((seed "test-seed")
         (value1 (random seed))
         (value2 (random)))
    (should (numberp value1))
    (should (= value1 (random seed)))
    (should (= value2 (random)))))

(ert-deftest test-random-range ()
  (should (< 0 (random 10000) 10000)))

(provide 'numbers-tests)
;;; numbers-tests.el ends here
