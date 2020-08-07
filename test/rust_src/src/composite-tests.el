;;; composite-tests.el --- Tests for composite.rs

;;; Code:

(require 'ert)

(ert-deftest compose-string-internal ()
  ;; All arguments, used negative to move backwards through index
  (should (compose-string-internal "hello world" 1 -5 "h" (+ 3 3)))
  
  ;; END should be greater than START
  (should-error (compose-string-internal "hello world" 1 0))
  )

(provide 'composite-tests)
;;; composite-tests.el ends here
