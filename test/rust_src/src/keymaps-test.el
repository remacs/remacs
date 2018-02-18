(require 'ert)

(ert-deftest keymap-prompt-tests ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (should (string= (keymap-prompt (make-keymap "test-prompt")) "test-prompt"))
    (should (eq (keymap-prompt (make-keymap)) nil))
    (should (eq (keymap-prompt sample-keymap)) nil)))

(ert-deftest keymap-make-tests ()
  (should (equal (make-keymap) '(keymap
                                 #^[nil nil keymap nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil])))
  
  (should (equal (make-keymap "menu-name") '(keymap
                                             #^[nil nil keymap nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] "menu-name"))))

(ert-deftest keymap-keymapp-tests ()
  (should (keymapp (make-keymap)))
  (should (keymapp '(keymap)))
  (should-not (keymapp '(test-map)))
  (should-not (keymapp nil)))

(ert-deftest keymap-local-tests ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (should (keymapp sample-keymap))
    
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
    (should (keymapp sample-keymap))
    (should (keymapp backup-keymap))
    
    (should-error (use-global-map nil))
    (use-global-map sample-keymap)
    (should (equal (current-global-map) '(keymap (3 keymap (26 . emacs-version)))))
    (use-global-map backup-keymap)))
