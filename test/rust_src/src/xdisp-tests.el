;;; xdisp-tests.el --- Tests for xdisp.rs

;;; Code:

(require 'ert)

(ert-deftest xdisp-tests--trace-redisplay-base ()
  "Check (trace-redisplay) base cases.
   Only run when configured with --enable-checks=glyphs or equivalent."
  (when (fboundp 'trace-redisplay)
    (should-error (trace-redisplay 1 2))
    (should-error (trace-redisplay nil 2))

    (should (eq nil (trace-redisplay)))
    (should (eq nil (trace-redisplay nil)))
    (should (eq nil (trace-redisplay -1)))
    (should (eq nil (trace-redisplay 0)))
    (should (eq nil (trace-redisplay 1)))))
