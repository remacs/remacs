;;; Code:

(ert-deftest transmute-type-test ()
       "Tests known transmutations."
       (should (equal (remacs-helpers/transmute-type '("int" "foo")) '("c_int" "foo")))
       (should (equal (remacs-helpers/transmute-type '("char" "foo")) '("c_char" "foo")))
       (should (equal (remacs-helpers/transmute-type '("bool" "foo")) '("bool" "foo")))
       (should (equal (remacs-helpers/transmute-type '("void")) nil)))

(ert-deftest args-from-C-worker-test ()
       "Tests known transmutations."
       (should (equal (remacs-helpers/make-rust-args-from-C-worker "int foo") "foo: c_int"))
       (should (equal (remacs-helpers/make-rust-args-from-C-worker "void") ""))
       (should (equal (remacs-helpers/make-rust-args-from-C-worker "register int bar, register char c") "bar: c_int, c: c_char"))
       (should (equal (remacs-helpers/make-rust-args-from-C-worker "struct thing foo") "foo: thing")))

(provide 'remacs-helpers-tests)

;;; remacs-helpers-tests.el ends here
