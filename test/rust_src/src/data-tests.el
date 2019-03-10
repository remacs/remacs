;;; data-tests.el --- Tests for data.rs

;;; Code:

(require 'ert)

(ert-deftest data-test--aref-base ()
  "Verify (aref) base cases"
  (should-error (aref "abc" -1) :type 'args-out-of-range)
  (should (eq (aref "abcdef" 3)
              ?d))
  (should-error (aref "abc" 3)
                :type 'args-out-of-range)
  (should (eq (aref (bool-vector t nil t nil) 1)
              nil))
  (should-error (aref (bool-vector t nil t nil) 4)
                :type 'args-out-of-range)
  (let ((tbl (make-char-table 'foo 0)))
    (should (eq (aref tbl 0)
                0)))
  (let* ((x 1)
         (y 2)
         (z 3)
         (v [x y z]))
    (should (eq (aref v 1)
                'y))
    (should-error (aref v 5)
                  :type 'args-out-of-range))
  (should (eq (aref (kbd "<f1> SPC") 1)
              32))
  (let ((r (make-record 'foo 9 'Z)))
    (should (eq (aref r 1)
                'Z))
    (should-error (aref r 10)
                  :type 'args-out-of-range))
  (should-error (aref 100 1)
                :type 'wrong-type-argument))

(ert-deftest data-test--aset-base ()
  "Verify (aset) base cases"
  (should-error (aset "abc" -1 ?d) :type 'args-out-of-range)
  (should-error (aset 100 1 1)
                :type 'wrong-type-argument)
  (let ((object "abcdef"))
    (should (eq (aset object 3 ?w)
                ?w))
    (should (eq (aref object 3)
                ?w)))
  (should-error (aset "abc" 3 ?w)
                :type 'args-out-of-range)
  (let ((bv (bool-vector t nil t nil)))
    (should-error (aset bv 4 t)
                  :type 'args-out-of-range)
    (should (eq (aset bv 1 t)
                t))
    (should (eq (aref bv 1)
                t)))
  (let ((tbl (make-char-table 'foo 0)))
    (should (eq (aset tbl 0 1)
                1))
    (should (eq (aref tbl 0)
                1)))
  (let* ((x 1)
         (y 2)
         (z 3)
         (a 10)
         (v [x y z]))
    (should-error (aset v 5 a)
                  :type 'args-out-of-range)
    (should (eq (aset v 1 a)
                10))
    (should (eq (aref v 1)
                10)))
  (let ((object (kbd "<f1> SPC")))
    (should (eq (aset object 1 33)
                33))
    (should (eq (aref object 1)
                33)))
  (let ((r (make-record 'foo 9 'Z)))
    (should-error (aref r 10)
                  :type 'args-out-of-range)
    (should (eq (aset r 1 'A)
                'A))
    (should (eq (aref r 1)
                'A)))
  )

(ert-deftest data-test--byteorder ()
  (should (member (byteorder) '(66 108))))

(ert-deftest data-test--subr-arity ()
  (should-error (subr-arity 'insert))
  (should (equal '(0 . many) (subr-arity (symbol-function 'insert))))
  (should (equal '(2 . 2) (subr-arity (symbol-function 'equal)))))

(ert-deftest data-test--subr-name ()
  (should-error (subr-name 'insert))
  (should (equal "insert" (subr-name (symbol-function 'insert))))
  (should (equal "equal" (subr-name (symbol-function 'equal)))))

(ert-deftest data-test--subr-lang ()
  (should-error (subr-lang 'insert))
  (should (equal "Rust" (subr-lang (symbol-function 'eval))))
  ;; If this C function is ported to Rust, replace it with another C
  ;; function. If there are no more C functions, then delete these
  ;; tests, delete `subr-lang', and celebrate :)
  (should (equal "C" (subr-lang (symbol-function 'menu-bar-menu-at-x-y)))))

(ert-deftest data-test--subr-lang-fail ()
  ;; `rename-buffer' is a primitive function that is advised by
  ;; default (by `uniquify'), confusing `subr-lang'.
  :expected-result :failed
  (should (equal "C" (subr-lang (symbol-function 'rename-buffer)))))

(ert-deftest data-test--describe-function ()
  ;; `describe-function' relies on `subr-lang' in its implementation,
  ;; so run it here to make sure that it works.
  (describe-function 'car)
  (describe-function 'rename-buffer))

(ert-deftest data-test--get-variable-documentation ()
  ;; Defined in Lisp
  (should (stringp (get 'split-width-threshold 'variable-documentation)))
  ;; Defined in C
  (should (integerp (get 'gc-cons-threshold 'variable-documentation)))
  ;; Defined in Rust
  (should (integerp (get 'post-self-insert-hook 'variable-documentation)))
  (should (integerp (get 'last-command 'variable-documentation)))
  (should (integerp (get 'header-line-format 'variable-documentation))))

(ert-deftest data-test--find-definition-noselect ()
  ;; Defined in Lisp
  (should (consp (find-definition-noselect 'split-width-threshold 'defvar)))
  ;; Defined in C
  (should (consp (find-definition-noselect 'gc-cons-threshold 'defvar)))
  ;; Defined in Rust
  (should (consp (find-definition-noselect 'post-self-insert-hook 'defvar))))

(ert-deftest test-string-to-number ()
  (should (= (string-to-number "aaa1") 0))
  (should (= (string-to-number "1aaa1") 1))
  (should (= (string-to-number "1") 1))
  (should (= (string-to-number "-1") -1))
  (should (= (string-to-number "0.1") 0.1))
  (should (= (string-to-number "-0.1") -0.1))
  (should (= (string-to-number "1111" 2) 15))
  (should (= (string-to-number "FF" 16) 255)))

(provide 'data-tests)
;;; data-tests.el ends here
