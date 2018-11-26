(require 'ert)

(ert-deftest window-display-table-nil ()
  (should (eq (window-display-table) nil)))

(ert-deftest window-display-table-not-nil ()
  (with-temp-buffer
    (let ((disptab (make-display-table)))
      ;; populate the display table
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Tables.html
      (dotimes (i 32)
        (or (= i ?\t)
            (= i ?\n)
            (aset disptab i
                  (vector (make-glyph-code ?^ 'escape-glyph)
                          (make-glyph-code (+ i 64) 'escape-glyph)))))
      ;; change the display table for the current window
      (set-window-display-table (get-buffer-window) disptab)
      (should (eq (window-display-table) disptab)))))

(ert-deftest window-dedicated-set-nil()
  ;; set window to non-dedicated
  (should (eq (set-window-dedicated-p (selected-window) nil) nil))
  (should(eq (window-dedicated-p (selected-window)) nil)))

(ert-deftest window-dedicated-set-not-nil()
  ;; set widnow to dedicated
  (should (eq (set-window-dedicated-p (selected-window) 't) 't))
  (should(eq (window-dedicated-p (selected-window)) 't)))

(ert-deftest window-dedicated-p-default-selected-window()
  (should(eq (window-dedicated-p) nil))
  ;; set selected widnow to dedicated
  (should (eq (set-window-dedicated-p (selected-window) 't) 't))
  (should(eq (window-dedicated-p) 't)))

(ert-deftest window-old-point()
  (should (eq (window-old-point) 1))
  (should (eq (window-old-point (selected-window)) 1)))

(ert-deftest window-new-total()
  (should (eq (window-new-total) (window-total-height)))
  (should (eq (window-new-total (selected-window)) (window-total-height))))

(ert-deftest window-new-normal ()
  "Effectively tests both `window-new-normal' (the getter) and
`set-window-new-normal' (the setter)."
  ;; Can we change normal on this window?
  (set-window-new-normal nil 1.0)
  (should (= (window-new-normal) 1.0))
  (set-window-new-normal nil 0.23)
  (should (= (window-new-normal) 0.23))

  ;; Can we correctly get the value of a different window?
  (let ((current-window-expected-normal 1.0)
        (other-window-expected-normal 0.5)
        (current-window (selected-window))
        (other-window (split-window)))
    (set-window-new-normal current-window current-window-expected-normal)
    (set-window-new-normal other-window other-window-expected-normal)
    ;; Normal for current-window should be the same
    (should (= (window-new-normal) current-window-expected-normal))
    (select-window other-window)
    (should (= (window-new-normal) other-window-expected-normal))
    (delete-window other-window)
    ))

(ert-deftest window-use-time ()
  (let ((use-time (window-use-time)))
    (should (eq 'integer (type-of use-time)))
    (should (> use-time 0))))

(ert-deftest get-mru-window ()
  (let ((w1 (selected-window))
        (w2 (split-window)))
    (should (not (eq w1 w2)))
    (should (eq (get-mru-window) w1))
    (select-window w2)
    (should (eq (get-mru-window) w2))
    (delete-window w2)
    (select-window w1)
    (should (eq (get-mru-window) w1))))
