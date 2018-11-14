;;; indent-tests.el --- tests for indent.c functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-current-indentation-spaces ()
  (insert "     some text")
  (should (equal (current-indentation) 5))
  (insert "\n some text")
  (should (equal (current-indentation) 1)))

(ert-deftest test-current-indentation-tabs ()
  (insert "\tsome more text")
  (should (equal (current-indentation) 8)))

;;; indent-tests.el ends here
