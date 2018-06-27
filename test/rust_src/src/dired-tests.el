;;; dired-tests.el --- tests for dired.rs functions

;;; Code:

(require 'ert)

(ert-deftest test-file-attributes-lessp ()
  (should-error (eval '(file-attributes-lessp '("rms" t)))
                :type 'wrong-number-of-arguments)
  (should-error (eval
                 '(file-attributes-lessp "rms" "wilfred"))
                :type 'wrong-type-argument)
  (should-error (eval
                 '(file-attributes-lessp '("rms" t) "wilfred"))
                :type 'wrong-type-argument)
  (should (not (file-attributes-lessp '() '())))
  (should (not (file-attributes-lessp '() '(""))))
  (should (file-attributes-lessp '("") '()))
  (should (file-attributes-lessp '("rms" t) '("wilfred" t)))
  (should (not (file-attributes-lessp '("wilfred" t) '("rms" t)))))

(ert-deftest test-system-users ()
  (should-error (eval '(system-users 'rms)) :type 'wrong-number-of-arguments)
  ;; The result should be a list of >= 1 user name(s) on all Unix and GNU systems.
  ;; Windows should be a list of only one user name.
  (should (eq 'cons (type-of (system-users))))
  (if (memq system-type '(windows-nt))
      (progn
        (should (= (length (system-users)) 1)))
    (progn
      (should (>= (length (system-users)) 1)))))
