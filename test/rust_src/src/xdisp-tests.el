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
    ;; if invisible isn't set, invisible-p should be nil
    (goto-char (point-min))
    (should-not (invisible-p (point)))
    ;; should also be able to pass the overlay to invisible-p, instead
    ;; of just the position
    (let* ((prop-and-overlay (get-char-property-and-overlay (point) 'invisible))
           (overlay (cdr prop-and-overlay)))
      (should-not (invisible-p (point))))
    ;; if the property invisible is t, invisible-p should be t
    (forward-word)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible t)
    (should (eq t (invisible-p (point))))
    (let* ((prop-and-overlay (get-char-property-and-overlay (point) 'invisible))
           (overlay (cdr prop-and-overlay)))
      (should (eq t (invisible-p (point)))))
    ;; if buffer-invisiblity-spec is nil, invisible-p should be nil
    (setq buffer-invisibility-spec nil)
    (should-not (invisible-p (point)))
    ;; if buffer-invisibility-spec is a list...
    (setq buffer-invisibility-spec '(foo (bar . t) (baz . nil)))
    ;; ...then when the invisible property is an atom in the spec
    ;; list, invisible-p should be t
    (next-line) (beginning-of-line)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible 'foo)
    (should (eq t (invisible-p (point))))
    ;; ...and when the invisible property is an atom and
    ;; invisibility-spec contains (atom . non-nil), then invisible-p
    ;; should be non-nil and non-t
    (forward-word) (forward-char)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible 'bar)
    (should (invisible-p (point)))
    (should-not (eq t (invisible-p (point))))
    (let* ((prop-and-overlay (get-char-property-and-overlay (point) 'invisible))
           (overlay (cdr prop-and-overlay)))
      (should (invisible-p (point)))
      (should-not (eq t (invisible-p (point)))))
    ;; ...and when (atom . nil), invisible-p should be t
    (forward-word) (forward-char)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible '(baz quux))
    (should (eq t (invisible-p (point))))
    ;; if the invisible property is a non-nil value not in the list,
    ;; invisible-p should be nil
    (forward-word) (forward-char)
    (put-text-property (point) (save-excursion (forward-word) (point))
                       'invisible t)
    (should-not (invisible-p (point)))))
