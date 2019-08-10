;;; frame-tests.el ---  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest frame-char-height ()
  (should (equal (frame-char-height) 1)))

(ert-deftest frame-char-width ()
  (should (equal (frame-char-width) 1)))

(provide 'frame-tests)
;;; frame-tests.el ends here
