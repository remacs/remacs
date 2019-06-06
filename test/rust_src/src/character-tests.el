;;; character-tests.el ---  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest char-resolve-modifiers ()
    (should (equal (char-resolve-modifiers -1) 4294967295))
    (should (equal (char-resolve-modifiers 0) 0))
    (should (equal (char-resolve-modifiers 42) 42))
    (should (equal (char-resolve-modifiers 500000) 500000)))

(provide 'character-tests)
;;; character-tests.el ends here
