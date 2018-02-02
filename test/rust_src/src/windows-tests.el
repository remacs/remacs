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
