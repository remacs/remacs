;;; xdisp-tests.el --- Tests for xdisp.rs

;;; Code:

(require 'ert)

(ert-deftest xdisp-tests--trace-redisplay-base ()
  (when (fboundp 'trace-redisplay)
    (should-error (trace-redisplay 'bogus))
    (should-error (trace-redisplay "wrong"))
    (should-error (trace-redisplay t))

    (should (eq nil (trace-redisplay)))
    (should (eq nil (trace-redisplay nil)))
    (should (eq nil (trace-redisplay -1)))
    (should (eq nil (trace-redisplay 0)))
    (should (eq nil (trace-redisplay 1)))))
