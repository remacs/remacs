;;; image-tests.el --- Tests for image.rs

;;; Code:

(require 'ert)

(ert-deftest image-test--init-image-library ()
  (should (eq (init-image-library 'jpeg) t)))

(provide 'image-tests)

;; image-tests.el ends here
