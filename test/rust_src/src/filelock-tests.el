;;; filelock-tests.el --- Tests for filelock.rs

;;; Code:

(require 'ert)

(ert-deftest filelock-tests--lock-buffer-base ()
  "Check lock-buffer base cases."
  (should (eq nil (lock-buffer)))
  (should (eq nil (lock-buffer nil)))
  (should-error (eval '(lock-buffer "foo" "bar")) :type 'wrong-number-of-arguments)
  (should-error (eval '(lock-buffer 1)) :type 'wrong-type-argument)
  (should-error (eval '(lock-buffer '("foo"))) :type 'wrong-type-argument)
  (should-error (eval '(lock-buffer 'bogus)) :type 'wrong-type-argument)
  (should-error (eval '(lock-buffer t)) :type 'wrong-type-argument))

(ert-deftest filelock-tests--unlock-buffer-base ()
  "Check unlock-buffer base cases."
  (should (eq nil (unlock-buffer)))
  (should-error (eval '(unlock-buffer "foo")) :type 'wrong-number-of-arguments)
  (should-error (eval '(unlock-buffer nil)) :type 'wrong-number-of-arguments))

(ert-deftest filelock-tests--lock-buffer-current ()
  "Check locking of current buffer."
  (let ((file (make-temp-file "filelock-tests--current-" nil ".txt" "test")))
    (unwind-protect
        (progn
          (find-file-existing file)
          (insert "modification")
          (lock-buffer)
          (should (file-locked-p file))
          (unlock-buffer)
          (should-not (file-locked-p file)))
      (delete-file file nil))))

(ert-deftest filelock-tests--lock-buffer-other ()
  "Check locking of a file that is not the current buffer’s file."
  (let ((file (make-temp-file "filelock-tests--other-" nil ".txt" "test"))
        (other (make-temp-file "filelock-tests--other-to-lock-" nil ".txt" "to-lock")))
    (unwind-protect
        (progn
          (find-file-existing file)
          (insert "modification")
          (lock-buffer other)
          ; I don’t understand this but it replicates GNU Emacs behavior
          (should (file-locked-p file))
          (should (file-locked-p other))
          (unlock-buffer)
          (should-not (file-locked-p file))
          (should (file-locked-p other)))
      (find-file-existing other)
      (unlock-buffer)
      (delete-file file nil)
      (delete-file other nil))))

(ert-deftest filelock-tests--unlock-buffer-deletes ()
  "Check that unlock-buffer deletes the lock file."
  (let* ((file (make-temp-file "filelock-tests--unlock-buffer-deletes-" nil ".txt" "test"))
         (lock-file (concat (file-name-directory file) (concat ".#" (file-name-nondirectory file)))))
    (unwind-protect
        (progn
          (find-file-existing file)
          (insert "modification")
          (lock-buffer)
          (should (or (file-symlink-p lock-file) (file-exists-p lock-file)))
          (unlock-buffer)
          (should-not (and (file-symlink-p lock-file) (file-exists-p lock-file))))
      (delete-file file nil))))

(ert-deftest filelock-tests--file-locked-p-base ()
  "Check file-locked-p base cases."
  (should-error (file-locked-p))
  (should-error (file-locked-p "foo" "bar"))
  (should-error (file-locked-p '("foo")))
  (should-error (file-locked-p nil))
  (should-error (file-locked-p t))
  (should-error (file-locked-p 'bogus))
  (should-error (file-locked-p 1))

  (should-not (file-locked-p "/this/file/should/not/exist"))
  (should-not (file-locked-p "~/this/file/should/not/exist")))

(ert-deftest filelock-tests--file-locked-p-unicode ()
  "Check that file locking works with files containing unicode characters."
  (let ((file (make-temp-file "filelock-tests--unicode-🤔-" nil ".txt" "test")))
    (unwind-protect
        (progn
          (find-file-existing file)
          (insert "modification")
          (lock-buffer)
          (should (file-locked-p file)))
      (unlock-buffer)
      (delete-file file nil))))

(ert-deftest filelock-tests--file-locked-p-file-name-coding ()
  "Check that file-locked-p respects encoding (#35171)."
  ; w32-unicode-filenames overrides file-name-coding-system on (modern) Windows
  ; so no need to test this in that case.
  (unless (eq system-type 'windows-nt)
    (let ((file-name-coding-system 'latin-1-unix)
          (file (make-temp-file "filelock-tests--encoding-éö-" nil ".txt" "test")))
      (unwind-protect
          (progn
            (find-file-existing file)
            (insert "modification")
            (lock-buffer)
            (should (file-locked-p file)))
        (unlock-buffer)
        (delete-file file nil)))))

(ert-deftest filelock-tests--locked-by-other-user ()
  "Check file-locked-p for (non-symlink) lock owned by some other user."
  (let* ((key (random most-positive-fixnum))
         (user (format "some-%d-user" key))
         (file (make-temp-file "filelock-tests--locked-by-other-user-" nil ".txt" "test"))
         (lock-file (concat (file-name-directory file) (concat ".#" (file-name-nondirectory file)))))
    (unwind-protect
        (progn
          (with-temp-file lock-file
            (insert (format "%s@some-%d-host.123" user key)))
          (should (equal user (file-locked-p file))))
      (delete-file file nil)
      (delete-file lock-file nil))))

(ert-deftest filelock-tests--locked-by-us ()
  "Check file-locked-p for (non-symlink) lock owned by current user."
  (let* ((file (make-temp-file "filelock-tests--locked-by-us-" nil ".txt" "test"))
         (user (user-login-name))
         (lock-file (concat (file-name-directory file) (concat ".#" (file-name-nondirectory file)))))
    (unwind-protect
        (progn
          (with-temp-file lock-file
            (insert (format "%s@%s.%d" user (system-name) (emacs-pid))))
          (should (file-locked-p file))))
    (delete-file file nil)
    (delete-file lock-file nil)))

(ert-deftest filelock-tests--locked-by-other-process ()
  "Check file-locked-p for (non-symlink) lock owned by another current user’s process."
  (let* ((file (make-temp-file "filelock-tests--locked-by-other-process-" nil ".txt" "test"))
         (user (user-login-name))
         ; PID 1 on *nix and 4 on Windows should always exist?
         (pid (if (eq system-type 'windows-nt) 4 1))
         (lock-file (concat (file-name-directory file) (concat ".#" (file-name-nondirectory file)))))
    (unwind-protect
        (progn
          (with-temp-file lock-file
            (insert (format "%s@%s.%d" user (system-name) pid)))
          (should (equal user (file-locked-p file))))
      (delete-file file nil)
      (delete-file lock-file nil))))
