;;; dired-tests.el --- tests for dired.rs functions

;;; Code:

(require 'ert)

(ert-deftest test-file-attributes-lessp ()
  (let ((rstr "rms")
	(wstr "wilfred")
	(lnull '())
	(snull '("")))
    (should-error (eval '(file-attributes-lessp '(rstr t)))
		  :type 'wrong-number-of-arguments)
    (should-error (eval '(file-attributes-lessp '(rstr t) '(wstr t) snull))
		  :type 'wrong-number-of-arguments)
    (should-error (eval
		   '(file-attributes-lessp rstr wstr))
		  :type 'wrong-type-argument)
    (should-error (eval
		   '(file-attributes-lessp '(rstr t) wstr))
		  :type 'wrong-type-argument)
    (should (not (file-attributes-lessp lnull lnull)))
    (should (not (file-attributes-lessp lnull snull)))
    (should (file-attributes-lessp snull lnull))
    (should (file-attributes-lessp '(rstr t) '(wstr t)))
    (should (not (file-attributes-lessp '(wstr t) '(rstr t))))))

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

(ert-deftest test-system-groups ()
  (should-error (system-groups 'rms) :type 'wrong-number-of-arguments)
  ;; The result should be a list of >= 1 group name(s) on all Unix and GNU systems.
  ;; Windows should be a nil
  (let ((system-groups (system-groups)))
    (if (eq system-type 'windows-nt)
        (should-not system-groups)
      (should system-groups)
      (should (listp system-groups))
      (should (< 0 (length system-groups))))))
