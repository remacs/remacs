;;; alloc-tests.el ---  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest memory-use-count ()
  (let ((counts (memory-use-counts)))
    (should (= 8 (length counts)))
    (dolist (count counts)
      (should (integerp count)))))

(ert-deftest bool-vector ()
  (should (bool-vector)))

(ert-deftest make-record ()
  (should (recordp (make-record 'foo 1 'A)))
  (should (equal (make-record 'foo 3 'Z) #s(foo Z Z Z))))

(ert-deftest record ()
  (should (recordp (record 'foo)))
  (should (equal (record 'foo 23 [bar baz] "rats") #s(foo 23 [bar baz] "rats")))
  (should-error (record)))

(provide 'alloc-tests)
;;; alloc-tests.el ends here
