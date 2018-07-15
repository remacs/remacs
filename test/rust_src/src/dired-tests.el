;;; dired-tests.el --- tests for dired.rs functions

;;; Code:

(require 'ert)

(ert-deftest test-file-attributes ()
  (let ((tdir "/")
        (reslen 12)
        (uidnth 2)
        (gidnth 3)
        (permsnth 8))
    (should-error (eval '(file-attributes tdir 'String 'bar))
                  :type 'wrong-number-of-arguments)
    (should (= reslen
               (length (file-attributes tdir))))
    (should (eq 'integer
                (type-of (nth uidnth (file-attributes tdir)))))
    (should (eq 'integer
                (type-of (nth gidnth (file-attributes tdir)))))
    (should (eq 'string
                (type-of (nth uidnth (file-attributes tdir 'String)))))
    (should (eq 'string
                (type-of (nth gidnth (file-attributes tdir 'String)))))
    (should (eq 'string
                (type-of (nth permsnth (file-attributes tdir))))))
)

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
