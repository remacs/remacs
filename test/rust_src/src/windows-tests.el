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
