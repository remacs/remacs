;;; textprop-tests.el --- tests for textprops.rs functions

;;; Code:

(require 'ert)

(ert-deftest get-text-property ()
  (let* ((string "foobar"))
    (put-text-property 0 (length string) 'face 'bold string)
    (should (eq (get-text-property 0 'face string) `bold))))


(provide 'textprop-tests)
;; textprop-tests.el ends here.
