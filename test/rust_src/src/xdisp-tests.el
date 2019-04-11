;;; xdisp-tests.el --- Tests for xdisp.rs

;;; Code:

(require 'ert)

(ert-deftest xdisp-tests--trace-redisplay-base ()
  "Check (trace-redisplay) base cases.
   Only run when configured with --enable-checks=glyphs or equivalent."
  (when (fboundp 'trace-redisplay)
    (should-error (trace-redisplay 1 2))
    (should-error (trace-redisplay nil 2))

    (should-not (trace-redisplay))
    (should-not (trace-redisplay nil))
    (should-not (trace-redisplay -1))
    (should-not (trace-redisplay 0))
    (should-not (trace-redisplay 1))))
