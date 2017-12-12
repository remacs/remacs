(ert-deftest vector-tests-make ()
  (let ((v (make-vector 10 "asdfghjklqwertyuiopzxcvbnm")))
    (should (= 10 (length v)))))
