;;; fileio-tests.el --- tests for fileio.rs functions

;;; Code:

(require 'ert)

(ert-deftest test-car-less-than-car ()
  (should (car-less-than-car '(1 2 3) '(3 2 1)))
  (should-not (car-less-than-car '(3 2 1) '(1 2 3)))
  (should-not (car-less-than-car '(1 2 3) '(1 2 3)))
  (should-error (car-less-than-car '() '(1)))
  (should-error (car-less-than-car '(1) '()))
  (should-error (car-less-than-car '("a") '("b"))))

(ert-deftest test-directory-name-p ()
  (should-error (eval '(directory-name-p)) :type 'wrong-number-of-arguments)
  (should-error (eval '(directory-name-p 3 4)) :type 'wrong-number-of-arguments)
  (should-error (eval '(directory-name-p 3)) :type 'wrong-type-argument)
  ;; / is the directory separator on all Unix and GNU systems except
  ;; Windows and MS-DOS
  (if (memq system-type '(ms-dos windows-nt))
      (progn
        (should (directory-name-p ".\\"))
        (should-not (directory-name-p ""))
        (should-not (directory-name-p "./")))
    (progn
      (should (directory-name-p "./"))
      (should-not (directory-name-p ""))
      (should-not (directory-name-p ".\\")))))

(ert-deftest test-file-predicates ()
  (let ((file (make-temp-file "file")))
    (when (memq system-type '(gnu/linux darwin))
      (should (file-name-absolute-p "/"))
      (should (file-name-absolute-p file))
      (should (file-name-absolute-p "~"))
      (should-not (file-name-absolute-p (file-name-base file))))
    (when (eq system-type 'gnu/linux)
      (should-not (file-name-case-insensitive-p file)))
    (when (eq system-type 'darwin)
      (should (file-name-case-insensitive-p file)))))
