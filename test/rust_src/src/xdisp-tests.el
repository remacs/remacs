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

(ert-deftest xdisp-test--invisible-p ()
  "Check making text invisible"
  (with-temp-buffer
    (insert "hello world\nsome bunch of text")
    (goto-char (point-min))
    (should-not (invisible-p (point)))
    (forward-word)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible t)
    (should (eq t (invisible-p (point))))
    (let* ((prop-and-overlay (get-char-property-and-overlay (point) 'invisible))
           (overlay (cdr prop-and-overlay)))
      (should (eq t (invisible-p (point)))))
    (setq buffer-invisibility-spec nil)
    (should-not (invisible-p (point)))
    (setq buffer-invisibility-spec '(foo (bar . t)))
    (next-line) (beginning-of-line)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible 'foo)
    (should (eq t (invisible-p (point))))
    (forward-word) (forward-char)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible 'bar)
    (should (= 2 (invisible-p (point))))))
