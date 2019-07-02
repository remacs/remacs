;;; composite-tests.el --- Tests for composite.rs

;;; Code:

(require 'ert)

(ert-deftest composite-test--compose-string-internal ()
  ;; All arguments, used negative to move backwards through index
  (should (eq
           (compose-string-internal "hello world" 1 -5 "h" (+ 3 3))
           #("hello world" 1 6 (composition ((5 . "h") . 6)))
           ))
  
  ;; END should be greater than START
  (should-error (compose-string-internal "hello world" 1 0))
  )

(provide 'composite-tests)
;;; composite-tests.el ends here
