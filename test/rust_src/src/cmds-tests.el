;;; cmds-tests.el --- Tests for cmds.rs

;;; Code:

(require 'ert)

(ert-deftest cmds-test--delete-char-base ()
  (let ((test-string "hello, world!"))
    ; Can't delete before beginning or after end
    (with-temp-buffer
      (insert test-string)
      (goto-char (point-min))
      (should-error (delete-char -1))
      (goto-char (point-max))
      (should-error (delete-char 1)))
    ; Deleting 0 chars removes nothing
    (with-temp-buffer
      (insert test-string)
      (goto-char (point-min))
      (delete-char 0)
      (should (string= (buffer-string) test-string)))
    ; From beginning deleting n removes first n characters
    (with-temp-buffer
      (insert test-string)
      (goto-char (point-min))
      (delete-char 2)
      (should (string= (buffer-string) "llo, world!")))
    ; Go to 2nd char, delete backwards 1
    (with-temp-buffer
      (insert test-string)
      (goto-char (1+ (point-min)))
      (delete-char -1)
      (should (string= (buffer-string) "ello, world!")))
    ; demonstrate deletion of multiple characters
    (with-temp-buffer
      (insert test-string)
      (goto-char (point-min))
      (delete-char 7)
      (should (string= (buffer-string) "world!")))
    ; Same result with killflag = true
    (with-temp-buffer
      (insert test-string)
      (goto-char (point-min))
      (delete-char 7 't)
      (should (string= (buffer-string) "world!"))))
  )

(provide 'cmds-tests)
;;; cmds-tests.el ends here
