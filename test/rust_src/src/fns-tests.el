;;; fns-tests.el -- tests for fns.rs function -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-load-average ()
  (should (integerp (car (load-average))))
  (should (integerp (car (load-average nil))))
  (should (floatp (car (load-average t))))
  (should (floatp (car (load-average 42))))
  (should (floatp (car (load-average "asdf"))))
  (should (floatp (car (load-average '(gimme floats))))))
