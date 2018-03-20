;;; process-tests.el --- Tests for process.rs

;;; Code:

(require 'ert)

(ert-deftest process-tests--process-inherit-coding-system-flag ()
  (let ((proc (start-process "test" nil "ls")))
    (should (not (process-inherit-coding-system-flag proc)))
    (set-process-inherit-coding-system-flag proc t)
    (should (equal t (process-inherit-coding-system-flag proc)))))
