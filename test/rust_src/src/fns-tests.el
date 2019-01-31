;;; fns-tests.el -- tests for fns.rs function -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-equal-string ()
  (should (equal "Test 1.2.3." "Test 1.2.3."))
  (should (equal "Rust" "Rust"))
  (should-not (equal "Remacs" "Emacs")))

(ert-deftest test-equal-float ()
  (should (equal 2.0 2.0))
  (should (equal 1.5 1.5)))

(ert-deftest test-equal-cons ()
  (should (equal '(1 . 3) '(1 . 3)))
  (should (equal '(1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 nil)))))))))))
                 '(1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 nil)))))))))))))
  (should-not (equal '(1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 nil)))))))))))
                     '(1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (2 nil))))))))))))))

(ert-deftest test-equal-marker ()
  (should (equal (make-marker) (make-marker)))
  (should (equal (point-marker) (point-marker))))

(ert-deftest test-equal-overlay ()
  (should (equal (make-overlay (buffer-end 0) (buffer-end 1))
                 (make-overlay (buffer-end 0) (buffer-end 1)))))

(ert-deftest test-equal-bool-vector ()
  (should (equal (bool-vector t nil t nil t nil)
                 (bool-vector t nil t nil t nil))))

(ert-deftest test-equal-char-table ()
  (should (equal (make-char-table 'syntax-table '(3))
                 (make-char-table 'syntax-table '(3)))))

(ert-deftest test-equal-vector ()
  (should (equal [1 two '(three) "four" [five]]
                 [1 two '(three) "four" [five]])))

(ert-deftest test-load-average ()
  (should (integerp (car (load-average))))
  (should (integerp (car (load-average nil))))
  (should (floatp (car (load-average t))))
  (should (floatp (car (load-average 42))))
  (should (floatp (car (load-average "asdf"))))
  (should (floatp (car (load-average '(gimme floats))))))
