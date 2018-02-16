(require 'ert)

(ert-deftest keymap-local-tests ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (use-local-map nil)
    (should-not (current-local-map))
    (use-local-map sample-keymap)
    (should (equal (current-local-map) '(keymap (3 keymap (26 . emacs-version)))))))

(ert-deftest keymap-global-tests ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version))))
        (backup-keymap (current-global-map)))
    (should-error (use-global-map nil))
    (use-global-map sample-keymap)
    (should (equal (current-global-map) '(keymap (3 keymap (26 . emacs-version)))))
    (use-global-map backup-keymap)))
