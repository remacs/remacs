(ert-deftest obarray-tests-intern ()
  ;; Calling `intern' should return us a symbol.
  (should (symbolp (intern "foo")))
  ;; If we insert a symbol in an obarray, we should be able to
  ;; retrieve it. See `(elisp)Creating Symbols'.
  (let ((my-obarray (make-vector 20 0)))
    (intern "bar" my-obarray)
    (should (intern-soft "bar" my-obarray)))
  ;; If the second argument is not an obarray, we should error.
  (should-error
   (intern "foo" 123)
   :type 'wrong-type-argument)
  ;; `intern' should only accept strings.
  (should-error
   (intern 'symbol)
   :type 'wrong-type-argument))

(ert-deftest obarray-tests-mapatoms ()
  ;; We should have `let' in our global obarray.
  (let ((found-let-sym nil))
    (mapatoms
     (lambda (s)
       (when (eq s 'let)
         (setq found-let-sym t))))
    (should found-let-sym))
  ;; We should be able to iterate over our own obarray.
  (let ((my-obarray (make-vector 20 0)))
    (mapatoms (lambda (s)) my-obarray))

  ;; If the second argument is not an obarray, we should error.
  (should-error
   (mapatoms (lambda (s)) 123)
   :type 'wrong-type-argument))
