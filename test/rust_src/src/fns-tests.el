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

(ert-deftest test-copy-alist ()
  (let ((alist '(("foo" . "bar") ("foo" . "bar"))))
    (should (equal alist (copy-alist alist)))))

(ert-deftest test-reverse ()
  (should-error (reverse))
  (should-error (reverse 1))
  (should-error (reverse (make-char-table 'foo)))
  (should (equal [] (reverse [])))
  (should (equal [0] (reverse [0])))
  (should (equal [1 2 3 4] (reverse (reverse [1 2 3 4]))))
  (should (equal '(a b c d) (reverse (reverse '(a b c d)))))
  (should (equal "xyzzy" (reverse (reverse "xyzzy"))))
  (should (equal "こんにちは / ｺﾝﾆﾁﾊ" (reverse (reverse "こんにちは / ｺﾝﾆﾁﾊ")))))

(ert-deftest test-reverse-bool-vector ()
  (let ((A (make-bool-vector 10 nil)))
    (dotimes (i 5) (aset A i t))
    (should (equal [nil nil nil nil nil t t t t t] (vconcat (reverse A))))
    (should (equal A (reverse (reverse A))))))

;; Test handling of cyclic and dotted lists.

(defun cyc1 (a)
  (let ((ls (make-list 10 a)))
    (nconc ls ls)
    ls))

(defun cyc2 (a b)
  (let ((ls1 (make-list 10 a))
        (ls2 (make-list 1000 b)))
    (nconc ls2 ls2)
    (nconc ls1 ls2)
    ls1))

(defun dot1 (a)
  (let ((ls (make-list 10 a)))
    (nconc ls 'tail)
    ls))

(defun dot2 (a b)
  (let ((ls1 (make-list 10 a))
        (ls2 (make-list 10 b)))
    (nconc ls1 ls2)
    (nconc ls2 'tail)
    ls1))

(ert-deftest test-cycle-reverse ()
  "Tests reverse errors on a circular list"
  (should-error (reverse (cyc1 1)) :type 'circular-list)
  (should-error (reverse (cyc2 1 2)) :type 'circular-list)
  (should-error (reverse (dot1 1)) :type 'wrong-type-argument)
  (should-error (reverse (dot2 1 2)) :type 'wrong-type-argument))
