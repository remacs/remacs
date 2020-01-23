(require 'ert)

(ert-deftest window-scroll-bar-width ()
  (let ((w1 (selected-window)))
    (set-window-scroll-bars w1 nil nil nil nil)
    (should (eq 0 (window-scroll-bar-width w1)))
    (set-window-scroll-bars w1 1 'right nil nil)
    (should (eq 1 (window-scroll-bar-width w1)))))

(ert-deftest window-scroll-bar-height ()
  (let ((w1 (selected-window)))
    (set-window-scroll-bars w1 nil nil nil nil)
    (should (eq 0 (window-scroll-bar-height w1)))))

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
  ;; set window to dedicated
  (should (eq (set-window-dedicated-p (selected-window) 't) 't))
  (should(eq (window-dedicated-p (selected-window)) 't)))

(ert-deftest window-dedicated-p-default-selected-window()
  (should(eq (window-dedicated-p) nil))
  ;; set selected window to dedicated
  (should (eq (set-window-dedicated-p (selected-window) 't) 't))
  (should(eq (window-dedicated-p) 't)))

(ert-deftest window-old-point()
  (should (eq (window-old-point) 1))
  (should (eq (window-old-point (selected-window)) 1)))

(ert-deftest window-new-total()
  (should (eq (window-new-total) (window-total-height)))
  (should (eq (window-new-total (selected-window)) (window-total-height))))

(ert-deftest set-window-new-total()
  (set-window-new-total nil 1)
  (should (eq (window-new-total) 1))
  (set-window-new-total nil 1 nil)
  (should (eq (window-new-total) 1))
  (set-window-new-total nil 1 t)
  (should (eq (window-new-total) 2)))

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

(ert-deftest window-parameters ()
  (should (eq nil (window-parameters)))
  (should (eq nil (window-parameters (selected-window))))
  (set-window-parameter (selected-window) 'test 'test)
  (should (consp (window-parameters)))
  (should (consp (window-parameters (selected-window)))))

(ert-deftest window-normal-size ()
  (let ((w1 (selected-window))
        (w2 (split-window)))
  (should (= 0.5 (window-normal-size)))
  (should (= 1.0 (window-normal-size nil 't)))
  (should (= 0.5 (window-normal-size w1)))
  (should (= 1.0 (window-normal-size w1 't)))
  (should (= 0.5 (window-normal-size w2)))
  (should (= 1.0 (window-normal-size w2 't)))))

(ert-deftest window-pixel-top ()
  (let ((w1 (selected-window))
        (w2 (split-window-vertically)))
    (should (= (window-pixel-top) (window-pixel-top w1)))
    (should-not (= (window-pixel-top w1) (window-pixel-top w2)))))

(ert-deftest window-left-column ()
  (let ((w1 (selected-window))
        (w2 (split-window-vertically))
        (w3 (split-window-horizontally (/ (window-width) 2))))
    (should (= 0 (window-left-column w1)))
    (should (= 0 (window-left-column w2)))
    (should-not (= 0 (window-left-column w3)))))

(ert-deftest window-fringes ()
  (skip-unless (display-graphic-p))
  (let ((w1 (selected-window))
        (w2 (split-window)))
    (set-window-fringes w1 4 4 nil)
    (set-window-fringes w2 16 16 t)
    (should (equal `(4 4 nil) (window-fringes w1)))
    (should (equal `(4 4 nil) (window-fringes nil)))
    (should (equal `(16 16 t) (window-fringes w2)))))

(ert-deftest set-window-fringes ()
  (skip-unless (display-graphic-p))
  (let ((w1 (selected-window))
        (w2 (split-window)))
    (set-window-fringes w1 4 4 nil)
    (set-window-fringes w2 16 16 t)
    (should (equal `(4 4 nil) (window-fringes w1)))
    (should (equal `(4 4 nil) (window-fringes nil)))
    (set-window-fringes nil 8 8 nil)
    (should (equal `(8 8 nil) (window-fringes w1)))
    (should (equal `(8 8 nil) (window-fringes nil)))
    (should (equal `(16 16 t) (window-fringes w2)))
    (set-window-fringes w2 15)
    (should (equal `(15 8 nil) (window-fringes w2)))
    (set-window-fringes w2 15 15)
    (should (equal `(15 15 nil) (window-fringes w2)))

    ;; invoking set-window-fringes with the same params would
    ;; not change window fringe properties and should return nil
    (should (set-window-fringes w1 1 1 nil))
    (should-not (set-window-fringes w1 1 1 nil))
    (should (set-window-fringes w1 1 2 nil))))

(ert-deftest window-hscroll ()
  "Effectively tests both `window-hscroll' (the getter) and
  `set-window-hscroll' (the setter)."

  ;; Can we change hscroll on this window?
  (set-window-hscroll nil 42)
  (should (= (window-hscroll) 42))
  (should (= (window-hscroll nil) 42))
  (set-window-hscroll nil 7)
  (should (= (window-hscroll) 7))
  (should (= (window-hscroll nil) 7))

  ;; Can we correctly operate on a different window?
  (let ((initial-window (selected-window))
        (other-window (split-window)))
    (set-window-hscroll initial-window 1337)
    (set-window-hscroll other-window 4711)
    (should (= (window-hscroll) 1337))
    (should (= (window-hscroll initial-window) 1337))
    (should (= (window-hscroll other-window) 4711))
    (select-window other-window)
    (should (= (window-hscroll) 4711))
    (should (= (window-hscroll initial-window) 1337))
    (should (= (window-hscroll other-window) 4711))
    (delete-window other-window)
    ))

;; TODO: make proper tests for this function when we will be able to change 
;; frames inside tests (see https://github.com/remacs/remacs/issues/1429).
(ert-deftest window-pixel-width-before-size-change ()
  (window-pixel-width-before-size-change))

;; TODO: make proper tests for this function when we will be able to change 
;; frames inside tests (see https://github.com/remacs/remacs/issues/1429).
(ert-deftest window-pixel-height-before-size-change ()
  (window-pixel-height-before-size-change))

(ert-deftest run-window-configuration-change-hook ()
  (setq window-conf-change-hook-val 0)
  (add-hook 'window-configuration-change-hook
            (lambda () (setq window-conf-change-hook-val
                        (+ window-conf-change-hook-val 1))))

  (should (eq (run-window-configuration-change-hook) nil))
  (should (eq window-conf-change-hook-val 1))
  (should (eq (run-window-configuration-change-hook (selected-frame)) nil))
  (should (eq window-conf-change-hook-val 2)))
