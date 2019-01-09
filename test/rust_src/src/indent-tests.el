;;; indent-tests.el --- tests for indent.c functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest move-to-column ()
  (insert "\tsome text")
  (move-to-column 12)
  (should (eq (current-column) 12))
  (should (eq (point) 6))
  ;; Test move to end of character
  (move-to-column 4)
  (should (eq (current-column) 8))
  (should (eq (point) 2))
  ;; Test FORCE with tabs
  (move-to-column 4 t)
  (should (eq (current-column) 4))
  (should (eq (point) 5))
  ;; Test move to end of line
  (move-to-column 30)
  (should (eq (current-column) 17))
  (should (eq (point) 15))
  ;; Test adding spaces/tabs to end of line only happens when FORCE is t
  (move-to-column 30 1)
  (should (eq (current-column) 17))
  (should (eq (point) 15))
  (move-to-column 30 t)
  (should (eq (current-column) 30))
  (should (eq (point) 22)))

(ert-deftest test-current-indentation-spaces ()
  (insert "     some text")
  (should (equal (current-indentation) 5))
  (insert "\n some text")
  (should (equal (current-indentation) 1)))

(ert-deftest test-current-indentation-tabs ()
  (insert "\tsome more text")
  (should (equal (current-indentation) 8)))

;;; indent-tests.el ends here
