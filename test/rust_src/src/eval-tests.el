;;; eval-tests.el --- unit tests for src/eval.c      -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for src/eval.c.

;;; Code:

(require 'ert)

(ert-deftest eval-tests--or-base ()
  "Check (or) base cases"
  (should (eq (or) nil))
  (should (eq (or t) t))
  (should (eq (or nil) nil))
  (should (eq (or nil t) t))
  (should (eq (or t nil) t))
  (should (eq (or 1 2) 1))
  (should (eq (or 1 2 (error "Not evaluated!")) 1)))

(ert-deftest eval-tests--and-base ()
  "Check (and) base cases"
  (should (eq (and) t))
  (should (eq (and t) t))
  (should (eq (and nil) nil))
  (should (eq (and nil t) nil))
  (should (eq (and t nil) nil))
  (should (eq (and t t) t))
  (should (eq (and 1 2) 2))
  (should (eq (and 1 nil (error "Not evaluated!")) nil)))

(ert-deftest eval-tests--if-base ()
  "Check (if) base cases"
  (should-error (eval '(if)) :type 'wrong-number-of-arguments)
  (should (eq (if t 'a) 'a))
  (should (eq (if t 'a 'b) 'a))
  (should (eq (if nil 'a) nil))
  (should (eq (if nil 'a 'b) 'b))
  (should (eq (if t 'a (error "Not evaluated!")) 'a))
  (should (eq (if nil (error "Not evaluated!") 'a) 'a)))

(ert-deftest eval-tests--if-dot-string ()
  "Check that Emacs rejects (if . \"string\")."
  (should-error (eval '(if . "abc")) :type 'wrong-type-argument)
  (let ((if-tail (list '(setcdr if-tail "abc") t)))
    (should-error (eval (cons 'if if-tail))))
  (let ((if-tail (list '(progn (setcdr if-tail "abc") nil) t)))
    (should-error (eval (cons 'if if-tail)))))

(ert-deftest eval-tests--cond-base ()
  "Check (cond) base cases"
  (should (eq (cond) nil))
  (should (eq (cond (t)) t))
  (should (eq (cond (1 2 3)) 3))
  (should (eq (cond (nil)
                    (t))
              t))
  (should (eq (cond (t)
                    (nil))
              t))
  (should (eq (cond (nil (error "Not evaluated!"))
                    (t 1))
              1)))

(ert-deftest eval-tests--mutating-cond ()
  "Check that Emacs doesn't crash on a cond clause that mutates during eval."
  (let ((clauses (list '((progn (setcdr clauses "ouch") nil)))))
    (should-error (eval (cons 'cond clauses)))))

(ert-deftest eval-tests--progn-base ()
  "Check (progn) base cases"
  (should (eq (progn) nil))
  (should (eq (progn t) t))
  (should (eq (progn nil t) t))
  (should (eq (progn t nil) nil)))

(ert-deftest eval-tests--prog1-base ()
  "Check (prog1) base cases"
  (should (eq (prog1 1 2) 1))
  (should (eq (prog1 nil 2) nil))
  (should-error (eval '(prog1 . (1 (error "Must be evaluated")))) :type 'error))

(ert-deftest eval-tests--prog2-base ()
  "Check (prog2) base cases"
  (should (eq (prog2 1 2 3) 2))
  (should (eq (prog2 nil 2 3) 2))
  (should-error (eval '(prog2 . (1 2 (error "Must be evaluated")))) :type 'error))

(ert-deftest eval-tests--setq-base ()
  "Check (setq) base cases"
  (should-error (eval '(setq . (a))) :type 'wrong-number-of-arguments)
  (should-error (eval '(setq . ((a 1 b)))) :type 'wrong-number-of-arguments)
  (should (eq (setq a 1) 1))
  (should (eq (setq a 1
                    b 2)
                2))
  (eval '(lambda ()  ;; Validate lexical bindings
    (should (eq ((let ((c t))
                   (setq c nil)))
                nil)))))

(ert-deftest eval-tests--function-base ()
  "Check (function) base cases"
  (should-error (function 1 2) :type 'wrong-number-of-arguments)
  (should (eq (function a) (quote a)))
  (should (equal (function (1 2 3))
                 (quote (1 2 3))))
  (should (equal (function (lambda (a) "Add 1 to A" (+ 1 a)))
                 (lambda (a) "Add 1 to A" (+ 1 a))))
  ;; First, ensure lexical bindings are active
  (should (equal (let ((x nil)
                       (f (let ((x t)) (lambda () x))))
                   (funcall f))
                 t))
  ;; Now test (:documentation) form for dynamic docs.
  (let ((val 1))
    (should (equal (function (lambda (a) (:documentation (format "Add %d to A" val))
                               (+ val a)))
                   (function (lambda (a) "Add 1 to A" (+ val a)))))))

(ert-deftest eval-tests--special-variable ()
  "Check support for special variables."
  (defvar eval-tests-var1 nil)
  (should (eq (special-variable-p eval-tests-var1) t))
  (internal-make-var-non-special 'eval-tests-var1)
  (should (eq (special-variable-p 'eval-tests-var1) nil))

  (let ((eval-tests-var2 nil))
    (should (eq (special-variable-p 'eval-tests-var2) nil))))

(dolist (form '(let let*))
  (dolist (arg '(1 "a" [a]))
    (eval
     `(ert-deftest ,(intern (format "eval-tests--%s--%s" form (type-of arg))) ()
        ,(format "Check that the first argument of `%s' cannot be a %s"
                 form (type-of arg))
        (should-error (,form ,arg) :type 'wrong-type-argument))
     t)))

(ert-deftest eval-tests--let-with-circular-defs ()
  "Check that Emacs reports an error for (let VARS ...) when VARS is circular."
  (let ((vars (list 'v)))
    (setcdr vars vars)
    (dolist (let-sym '(let let*))
      (should-error (eval (list let-sym vars))))))

;;; eval-tests.el ends here
