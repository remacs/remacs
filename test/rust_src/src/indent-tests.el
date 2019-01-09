;;; indent-tests.el --- tests for indent.c functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest move-to-column-basic ()
  (insert "some text")
  (move-to-column 6)
  (should (eq (current-column) 6))
  (should (eq (point) 7)))

(ert-deftest move-to-column-tabs ()
  ;; Test move to end of character
  (insert "\ttext")
  (move-to-column 4)
  (should (eq (current-column) 8))
  (should (eq (point) 2))
  ;; Test FORCE with tabs
  (move-to-column 4 t)
  (should (eq (current-column) 4))
  (should (eq (point) 5))
  ;; character after point should still be a tab
  (should (eq (char-after) 9)))

(ert-deftest move-to-column-eol ()
  ;; Test move to end of line
  (insert "some text")
  (move-to-column 30)
  (should (eq (current-column) 9))
  (should (eq (point) 10))
  ;; Test adding spaces/tabs to end of line only happens when FORCE is t
  (move-to-column 30 1)
  (should (eq (current-column) 9))
  (should (eq (point) 10))
  (move-to-column 30 t)
  (should (eq (current-column) 30))
  (should (eq (point) 18)))

(ert-deftest test-current-indentation-spaces ()
  (insert "     some text")
  (should (equal (current-indentation) 5))
  (insert "\n some text")
  (should (equal (current-indentation) 1)))

(ert-deftest test-current-indentation-tabs ()
  (insert "\tsome more text")
  (should (equal (current-indentation) 8)))

;;; indent-tests.el ends here
