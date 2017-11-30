;;; remacs-helpers-tests -- ERT tests for the helpers
;;; Commentary:

;; Simple examples to demonstrate capabilities and test the code.

;;; Code:

(require 'remacs-helpers)

(ert-deftest transmute-type-test ()
  "Tests known transmutations."
  (should (equal (remacs-helpers/transmute-type '("int" "foo"))
                 '("c_int" "foo")))
  (should (equal (remacs-helpers/transmute-type '("char" "foo"))
                 '("c_char" "foo")))
  (should (equal (remacs-helpers/transmute-type '("bool" "foo"))
                 '("bool" "foo")))
  (should (equal (remacs-helpers/transmute-type '("void"))
                 nil)))

(ert-deftest args-from-C-worker-test ()
  "Tests C args transformations."
  (should (equal (remacs-helpers/make-rust-args-from-C-worker "int foo")
                 "foo: c_int"))
  ;; Ideally this becomes something sensible....
  (should (equal (remacs-helpers/make-rust-args-from-C-worker "void *foo")
                 "foo: *mut void"))
  (should (equal (remacs-helpers/make-rust-args-from-C-worker "void")
                 ""))
  (should (equal (remacs-helpers/make-rust-args-from-C-worker
                  "register int bar, register char c")
                 "bar: c_int, c: c_char"))
  (should (equal (remacs-helpers/make-rust-args-from-C-worker
                  "struct thing foo")
                 "foo: thing")))

;; Enforce order so failures are caught before they cascade
(defvar test-order '(member transmute-type-test
                            args-from-C-worker-test))

(provide 'remacs-helpers-tests)

;;; remacs-helpers-tests.el ends here
