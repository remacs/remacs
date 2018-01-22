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

;;; eval-tests.el ends here
