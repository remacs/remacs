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
  (should (eq (or t nil) t)))

(ert-deftest eval-tests--and-base ()
  "Check (and) base cases"
  (should (eq (and) t))
  (should (eq (and t) t))
  (should (eq (and nil) nil))
  (should (eq (and nil t) nil))
  (should (eq (and t nil) nil))
  (should (eq (and t t) t)))

(ert-deftest eval-tests--if-base ()
  "Check (if) base cases"
  (should-error (eval '(if)) :type 'wrong-number-of-arguments)
  (should (eq (if t 'a) 'a))
  (should (eq (if t 'a 'b) 'a))
  (should (eq (if nil 'a) nil))
  (should (eq (if nil 'a 'b) 'b)))

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
  (should (eq (cond (nil)
                    (t))
              t))
  (should (eq (cond (t)
                    (nil))
              t)))

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

;;; eval-tests.el ends here
