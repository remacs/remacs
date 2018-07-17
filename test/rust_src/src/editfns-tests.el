;;; editfns-tests.el --- tests for editfns.rs functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-char-before ()
  (let ((char1 97)
        (char2 949))
    (with-temp-buffer 
      (should (eq (char-before) nil))
      (should (eq (char-before -1) nil))
      (should (eq (char-before 100) nil))
      (insert "a")
      (should (char-equal (char-before) char1))
      (insert "ε")
      (should (char-equal (char-before) char2))
      (should (char-equal (char-before 2) char1)))))
