;;; process-tests.el --- Tests for process.rs

;;; Code:

(require 'ert)

(ert-deftest process-tests--process-inherit-coding-system-flag ()
  (let* ((cmd (if (eq system-type 'windows-nt) "dir" "ls"))
         (proc (start-process "test" nil cmd)))
    (should (not (process-inherit-coding-system-flag proc)))
    (set-process-inherit-coding-system-flag proc t)
    (should (equal t (process-inherit-coding-system-flag proc)))))
