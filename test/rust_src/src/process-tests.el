;;; process-tests.el --- Tests for process.rs

;;; Code:

(require 'ert)

(ert-deftest process-tests--process-inherit-coding-system-flag ()
  (let* ((cmd (if (eq system-type 'windows-nt) "dir" "ls"))
         (proc (start-process "test" nil cmd)))
    (should (not (process-inherit-coding-system-flag proc)))
    (set-process-inherit-coding-system-flag proc t)
    (should (equal t (process-inherit-coding-system-flag proc)))))

(ert-deftest process-tests--process-type ()
  (let* ((cmd (if (eq system-type 'windows-nt) "dir" "ls"))
         (real-proc (start-process "test-real" nil cmd))
         (pipe-proc (make-pipe-process :name "test-pipe" :command '(cmd)))
         (network-proc (make-network-process :name "test-network" :host 'local :service 10613 :server t))
         (buffer-proc (start-process "test-buffer" "*test*" cmd)))
    (should (equal 'real (process-type real-proc)))
    (should (equal 'network (process-type network-proc)))
    (should (equal 'pipe (process-type pipe-proc)))
    (should (equal 'real (process-type "test-real")))
    (should (equal 'real (process-type "*test*")))
    (delete-process real-proc)
    (delete-process network-proc)
    (delete-process pipe-proc)
    (delete-process buffer-proc)))
