(require 'ert)

(ert-deftest keymap-tests--copy-keymap ()
  (let ((sample-keymap '(keymap
                         (27 keymap
                             (24 . lisp-send-defun)
                             (28 . forward-line))
                         (3 keymap
                            (25 . run-lisp))))
        (sample-keymap-long `(keymap
                              (3 keymap
                                 (25 . run-lisp))
                              (27 keymap
                                  (24 . lisp-send-defun))
                              ,(make-char-table 'test 0)
                              [0 0 0 0 0 0]
                              keymap
                              (127 . backward-delete-char-untabify)
                              (26 keymap
                                  (17 . indent-sexp))
                              )))

    ;; Test copying
    (should (equal (copy-keymap sample-keymap) sample-keymap))
    (should (equal (copy-keymap sample-keymap-long) sample-keymap-long))

    ;; Test empty keymap
    (should (equal (copy-keymap '(keymap)) '(keymap)))
    
    ;; Test invalid inputs
    (should-error (copy-keymap nil))
    (should-error (copy-keymap "string"))))

(ert-deftest keymap-tests--map-keymap ()
  (let* ((sample-keymap '(keymap
                          (27 keymap
                              (24 . lisp-send-defun)
                              (28 . forward-line))
                          (3 keymap
                             (25 . run-lisp))))
         (sample-keymap-with-parent '(keymap
                                      (3 keymap
                                         (25 . run-lisp))
                                      (27 keymap
                                          (24 . lisp-send-defun))
                                      keymap
                                      (127 . backward-delete-char-untabify)
                                      (26 keymap
                                          (17 . indent-sexp))))
         (keys nil)
         (values nil)
         (test-function '(lambda (key value)
                           (push key keys)
                           (push value values))))

    ;; Test simple keymap with children
    (map-keymap test-function sample-keymap)
    (should (equal keys '(3 27)))
    (should (equal values '((keymap (25 . run-lisp))
                            (keymap (24 . lisp-send-defun) (28 . forward-line))
                            )))

    ;; Test simple keymap with children, sort_first is t
    (setq keys nil)
    (setq values nil)
    (map-keymap test-function sample-keymap t)
    (should (equal keys '(27 3)))
    (should (equal values '((keymap (24 . lisp-send-defun) (28 . forward-line))
                            (keymap (25 . run-lisp)))))

    ;; Test keymap with parent keymap
    (setq keys nil)
    (setq values nil)
    (map-keymap test-function sample-keymap-with-parent)
    (should (equal keys '(26 127 27 3)))
    (should (equal values '((keymap (17 . indent-sexp))
                            backward-delete-char-untabify
                            (keymap (24 . lisp-send-defun))
                            (keymap (25 . run-lisp)))))

    ;; Test keymap with parent keymap, sort_first is t
    (setq keys nil)
    (setq values nil)
    (map-keymap test-function sample-keymap-with-parent t)
    (should (equal keys '(127 27 26 3)))
    (should (equal values '(backward-delete-char-untabify
                            (keymap (24 . lisp-send-defun))
                            (keymap (17 . indent-sexp))
                            (keymap (25 . run-lisp)))))

    ;; Test invalid inputs
    (should-error (map-keymap nil nil))
    (should-error (map-keymap "test" nil))
    (should-error (map-keymap test-function nil))
    (should-error (map-keymap test-function '(test)))))

(ert-deftest keymap-tests--map-keymap-internal ()
  (let* ((sample-keymap '(keymap
                         (27 keymap
                             (24 . lisp-send-defun)
                             (28 . forward-line))
                         (3 keymap
                            (25 . run-lisp))))
        (sample-keymap-with-parent '(keymap
                                     (3 keymap
                                        (25 . run-lisp))
                                     (27 keymap
                                         (24 . lisp-send-defun))
                                     keymap
                                     (127 . backward-delete-char-untabify)
                                     (26 keymap
                                         (17 . indent-sexp))))
        (keys nil)
        (values nil)
        (test-function '(lambda (key value)
                          (push key keys)
                          (push value values))))

    ;; Test simple keymap with children
    (should-not (map-keymap-internal test-function sample-keymap))
    (should (equal keys '(3 27)))
    (should (equal values '((keymap (25 . run-lisp))
                            (keymap (24 . lisp-send-defun) (28 . forward-line)))))

    ;; Test keymap with parent keymap
    (setq keys nil)
    (setq values nil)
    (should (equal (map-keymap-internal test-function sample-keymap-with-parent) '(keymap
                                                                                   (127 . backward-delete-char-untabify)
                                                                                   (26 keymap
                                                                                       (17 . indent-sexp)))))
    (should (equal keys '(27 3)))
    (should (equal values '((keymap (24 . lisp-send-defun))
                            (keymap (25 . run-lisp)))))

    ;; If one of the elements is a char-table
    (setq keys nil)
    (setq values nil)
    (should-not (map-keymap-internal test-function `(keymap (24 . lisp-send-defun) ,(make-char-table 'test 0))))
    (should (equal keys '((0 . 4194303) 24)))
    (should (equal values '(0 lisp-send-defun)))

    ;; If one of the elements is a vector
    (setq keys nil)
    (setq values nil)
    (should-not (map-keymap-internal test-function '(keymap (24 . lisp-send-defun) [0 0 0 0 0 0])))
    (should (equal keys '(5 4 3 2 1 0 24)))
    (should (equal values '(0 0 0 0 0 0 lisp-send-defun)))

    ;; Test invalid inputs
    (should-error (map-keymap-internal nil nil))
    (should-error (map-keymap-internal "test" nil))
    (should-error (map-keymap-internal  test-function nil))
    (should-error (map-keymap-internal test-function '(test)))))
  
(ert-deftest keymap-tests--set-keymap-parent ()
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

(ert-deftest keymap-tests--keymap-parent ()
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

(ert-deftest keymap-tests--keymap-prompt ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (should-not (keymap-prompt nil))
    (should (string= (keymap-prompt (make-keymap "test-prompt")) "test-prompt"))
    (should-not (keymap-prompt (make-keymap)))
    (should-not (keymap-prompt sample-keymap))))

(ert-deftest keymap-tests--make-keymap ()
  (should (equal (make-keymap) '(keymap
                                 #^[nil nil keymap nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil])))
  
  (should (equal (make-keymap "menu-name") '(keymap
                                             #^[nil nil keymap nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] "menu-name"))))

(ert-deftest keymap-tests--keymapp ()
  (should (keymapp (make-keymap)))
  (should (keymapp '(keymap)))
  (should-not (keymapp '(test-map)))
  (should-not (keymapp nil)))

(ert-deftest keymap-tests--use-local-map ()
  (let ((sample-keymap '(keymap
                         (3 keymap
                            ;; C-c C-z
                            (26 . emacs-version)))))
    (should (keymapp sample-keymap))
    
    (use-local-map nil)
    (should-not (current-local-map))
    (use-local-map sample-keymap)
    (should (equal (current-local-map) '(keymap (3 keymap (26 . emacs-version)))))))

(ert-deftest keymap-tests--use-global-map ()
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
