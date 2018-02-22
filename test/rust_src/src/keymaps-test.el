(require 'ert)

(ert-deftest keymap-set-keymap-parent-tests ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version))))
        (map (make-sparse-keymap)))
    
    (should (equal (set-keymap-parent map sample-keymap) sample-keymap))
    (should (equal map '(keymap keymap (3 keymap (26 . emacs-version)))))

    (should-error (set-keymap-parent (set-keymap-parent map sample-keymap) sample-keymap)))

  (let ((map (make-sparse-keymap)))
    (should-not (set-keymap-parent map nil))
    (should (equal map '(keymap)))))

(ert-deftest keymap-parent-tests ()
  (let ((sample-keymap-with-parent '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . run-lisp))
                         (27 keymap
                             ;; C-M-x, treated as <ESC> C-x
                             (24 . lisp-send-defun))
                         ;; This part is inherited from lisp-mode-shared-map.
                         keymap
                         ;; <DEL>
                         (127 . backward-delete-char-untabify)
                         (27 keymap
                             ;; C-M-q, treated as <ESC> C-q
                             (17 . indent-sexp))))
        (sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (should (equal (keymap-parent '(keymap keymap
                                           (17 . indent-sexp))) '(keymap
                                                                  (17 . indent-sexp))))
    (should (equal (keymap-parent sample-keymap-with-parent) '(keymap (127 . backward-delete-char-untabify)
                                                          (27 keymap
                                                              (17 . indent-sexp)))))
    (should-not (keymap-parent '(keymap ())))
    (should-not (keymap-parent sample-keymap))
    (should-error (keymap-parent '()))
    (should-error (keymap-parent nil))
    (should-error (keymap-parent "test"))))

(ert-deftest keymap-prompt-tests ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (should-not (keymap-prompt nil))
    (should (string= (keymap-prompt (make-keymap "test-prompt")) "test-prompt"))
    (should-not (keymap-prompt (make-keymap)))
    (should-not (keymap-prompt sample-keymap))))

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
