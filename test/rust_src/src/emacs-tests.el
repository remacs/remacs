;;; emacs-tests.el --- -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(ert-deftest test-emacs ()
  (should (stringp (invocation-name)))
  (should (stringp (invocation-directory))))

(provide 'emacs-tests)
;;; emacs-tests.el ends here
