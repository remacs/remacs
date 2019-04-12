;;; filelock-tests.el --- Tests for filelock.rs

;;; Code:

(require 'ert)

(ert-deftest filelock-tests--lock-buffer-base ()
  "Check lock-buffer base cases"
  (should (eq nil (lock-buffer)))
  (should (eq nil (lock-buffer nil)))
  (should-error (eval '(lock-buffer "foo" "bar")) :type 'wrong-number-of-arguments)
  (should-error (eval '(lock-buffer 1)) :type 'wrong-type-argument)
  (should-error (eval '(lock-buffer '("foo"))) :type 'wrong-type-argument)
  (should-error (eval '(lock-buffer 'bogus)) :type 'wrong-type-argument)
  (should-error (eval '(lock-buffer t)) :type 'wrong-type-argument))

(ert-deftest filelock-tests--unlock-buffer-base ()
  "Check unlock-buffer base cases"
  (should (eq nil (unlock-buffer)))
  (should-error (eval '(unlock-buffer "foo")) :type 'wrong-number-of-arguments)
  (should-error (eval '(unlock-buffer nil)) :type 'wrong-number-of-arguments))

(ert-deftest filelock-tests--lock-buffer-current ()
  "Check locking of current buffer"
  (let ((file (make-temp-file "filelock-tests--current-" nil ".txt" "test")))
    (find-file-existing file)
    (insert "modification")
    (lock-buffer)
    (should (eq t (file-locked-p file)))
    (unlock-buffer)
    (should (eq nil (file-locked-p file)))))

(ert-deftest filelock-tests--lock-buffer-other ()
  "Check locking of other file"
  (let ((file (make-temp-file "filelock-tests--other-" nil ".txt" "test"))
        (other (make-temp-file "filelock-tests--other-to-lock-" nil ".txt" "to-lock")))
    (find-file-existing file)
    (insert "modification")
    (lock-buffer other)
    ; I don’t understand this but it replicates GNU Emacs behavior
    (should (eq t (file-locked-p file)))
    (should (eq t (file-locked-p other)))
    (unlock-buffer)
    (should (eq nil (file-locked-p file)))
    (should (eq t (file-locked-p other)))))

