;;; auto-revert-tests.el --- Tests of auto-revert

;; Copyright (C) 2015-2019 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Commentary:

;; Some of the tests require access to a remote host files.  Since
;; this could be problematic, a mock-up connection method "mock" is
;; used.  Emulating a remote connection, it simply calls "sh -i".
;; Tramp's file name handlers still run, so this test is sufficient
;; except for connection establishing.

;; If you want to test a real Tramp connection, set
;; $REMOTE_TEMPORARY_FILE_DIRECTORY to a suitable value in order to
;; overwrite the default value.  If you want to skip tests accessing a
;; remote host, set this environment variable to "/dev/null" or
;; whatever is appropriate on your system.

;; For the remote file-notify library, Tramp checks for the existence
;; of a respective command.  The first command found is used.  In
;; order to use a dedicated one, the environment variable
;; $REMOTE_FILE_NOTIFY_LIBRARY shall be set, possible values are
;; "inotifywait", "gio-monitor" and "gvfs-monitor-dir".

;; Local file-notify libraries are auto-detected during Emacs
;; configuration.  This can be changed with a respective configuration
;; argument, like
;;
;;   --with-file-notification=inotify
;;   --with-file-notification=kqueue
;;   --with-file-notification=gfile
;;   --with-file-notification=w32

;; A whole test run can be performed calling the command `auto-revert-test-all'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'autorevert)
(require 'tramp)

(setq auto-revert-debug nil
      auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-stop-on-user-input nil
      file-notify-debug nil
      tramp-verbose 0
      tramp-message-show-message nil)

(defconst auto-revert--timeout 10
  "Time to wait for a message.")

(defvar auto-revert--messages nil
  "Used to collect messages issued during a section of a test.")

;; There is no default value on w32 systems, which could work out of the box.
(defconst auto-revert-test-remote-temporary-file-directory
  (cond
   ((getenv "REMOTE_TEMPORARY_FILE_DIRECTORY"))
   ((eq system-type 'windows-nt) null-device)
   (t (add-to-list
       'tramp-methods
       '("mock"
	 (tramp-login-program        "sh")
	 (tramp-login-args           (("-i")))
	 (tramp-remote-shell         "/bin/sh")
	 (tramp-remote-shell-args    ("-c"))
	 (tramp-connection-timeout   10)))
      (add-to-list
       'tramp-default-host-alist
       `("\\`mock\\'" nil ,(system-name)))
      ;; Emacs' Makefile sets $HOME to a nonexistent value.  Needed in
      ;; batch mode only, therefore.  `temporary-file-directory' might
      ;; be quoted, so we unquote it just in case.
      (unless (and (null noninteractive) (file-directory-p "~/"))
        (setenv "HOME" (file-name-unquote temporary-file-directory)))
      (format "/mock::%s" temporary-file-directory)))
  "Temporary directory for Tramp tests.")

;; Filter suppressed remote file-notify libraries.
(when (stringp (getenv "REMOTE_FILE_NOTIFY_LIBRARY"))
  (dolist (lib '("inotifywait" "gio-monitor" "gvfs-monitor-dir"))
    (unless (string-equal (getenv "REMOTE_FILE_NOTIFY_LIBRARY") lib)
      (add-to-list 'tramp-connection-properties `(nil ,lib nil)))))

(defvar auto-revert--test-enabled-remote-checked nil
  "Cached result of `auto-revert--test-enabled-remote'.
If the function did run, the value is a cons cell, the `cdr'
being the result.")

(defun auto-revert--test-enabled-remote ()
  "Whether remote file access is enabled."
  (unless (consp auto-revert--test-enabled-remote-checked)
    (setq
     auto-revert--test-enabled-remote-checked
     (cons
      t (ignore-errors
	  (and
	   (file-remote-p auto-revert-test-remote-temporary-file-directory)
	   (file-directory-p auto-revert-test-remote-temporary-file-directory)
	   (file-writable-p
            auto-revert-test-remote-temporary-file-directory))))))
  ;; Return result.
  (cdr auto-revert--test-enabled-remote-checked))

(defun auto-revert--wait-for-revert (buffer)
  "Wait until a message reports reversion of BUFFER.
This expects `auto-revert--messages' to be bound by
`ert-with-message-capture' before calling."
  ;; Remote files do not cooperate well with timers.  So we count ourselves.
  (let ((ct (current-time)))
    (while (and (< (float-time (time-subtract (current-time) ct))
                   auto-revert--timeout)
                (null (string-match
                       (format-message
                        "Reverting buffer `%s'\\." (buffer-name buffer))
                       auto-revert--messages)))
      (if (with-current-buffer buffer auto-revert-use-notify)
          (read-event nil nil 0.1)
        (sleep-for 0.1)))))

(defmacro auto-revert--deftest-remote (test docstring)
  "Define ert `TEST-remote' for remote files."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat (symbol-name test) "-remote")) ()
     ,docstring
     :tags '(:expensive-test)
     (let ((temporary-file-directory
	    auto-revert-test-remote-temporary-file-directory)
           (auto-revert-remote-files t)
	   (ert-test (ert-get-test ',test))
           vc-handled-backends)
       (skip-unless (auto-revert--test-enabled-remote))
       (tramp-cleanup-connection
	(tramp-dissect-file-name temporary-file-directory) nil 'keep-password)
       (condition-case err
           (funcall (ert-test-body ert-test))
         (error (message "%s" err) (signal (car err) (cdr err)))))))

(ert-deftest auto-revert-test00-auto-revert-mode ()
  "Check autorevert for a file."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
	(progn
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
          (with-current-buffer buf
            (ert-with-message-capture auto-revert--messages
              (should (string-equal (buffer-string) "any text"))
              ;; `buffer-stale--default-function' checks for
              ;; `verify-visited-file-modtime'.  We must ensure that it
              ;; returns nil.
              (sleep-for 1)
              (auto-revert-mode 1)
              (should auto-revert-mode)

              ;; Modify file.  We wait for a second, in order to have
              ;; another timestamp.
              (sleep-for 1)
              (write-region "another text" nil tmpfile nil 'no-message)

              ;; Check, that the buffer has been reverted.
              (auto-revert--wait-for-revert buf))
            (should (string-match "another text" (buffer-string)))

            ;; When the buffer is modified, it shall not be reverted.
            (ert-with-message-capture auto-revert--messages
              (set-buffer-modified-p t)
              (sleep-for 1)
              (write-region "any text" nil tmpfile nil 'no-message)

              ;; Check, that the buffer hasn't been reverted.
              (auto-revert--wait-for-revert buf))
            (should-not (string-match "any text" (buffer-string)))))

      ;; Exit.
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(auto-revert--deftest-remote auto-revert-test00-auto-revert-mode
  "Check autorevert for a remote file.")

;; This is inspired by Bug#21841.
(ert-deftest auto-revert-test01-auto-revert-several-files ()
  "Check autorevert for several files at once."
  :tags '(:expensive-test)
  (skip-unless (executable-find "cp" (file-remote-p temporary-file-directory)))

  (let* ((cp (executable-find "cp" (file-remote-p temporary-file-directory)))
         (tmpdir1 (make-temp-file "auto-revert-test" 'dir))
         (tmpdir2 (make-temp-file "auto-revert-test" 'dir))
         (tmpfile1
          (make-temp-file (expand-file-name "auto-revert-test" tmpdir1)))
         (tmpfile2
          (make-temp-file (expand-file-name "auto-revert-test" tmpdir1)))
         buf1 buf2)
    (unwind-protect
        (ert-with-message-capture auto-revert--messages
          (write-region "any text" nil tmpfile1 nil 'no-message)
          (setq buf1 (find-file-noselect tmpfile1))
          (write-region "any text" nil tmpfile2 nil 'no-message)
          (setq buf2 (find-file-noselect tmpfile2))

          (dolist (buf (list buf1 buf2))
            (with-current-buffer buf
              (should (string-equal (buffer-string) "any text"))
              ;; `buffer-stale--default-function' checks for
              ;; `verify-visited-file-modtime'.  We must ensure that
              ;; it returns nil.
              (sleep-for 1)
              (auto-revert-mode 1)
              (should auto-revert-mode)))

          ;; Modify files.  We wait for a second, in order to have
          ;; another timestamp.
          (sleep-for 1)
          (write-region
           "another text" nil
           (expand-file-name (file-name-nondirectory tmpfile1) tmpdir2)
           nil 'no-message)
          (write-region
           "another text" nil
           (expand-file-name (file-name-nondirectory tmpfile2) tmpdir2)
           nil 'no-message)
          ;;(copy-directory tmpdir2 tmpdir1 nil 'copy-contents)
          ;; Strange, that `copy-directory' does not work as expected.
          ;; The following shell command is not portable on all
          ;; platforms, unfortunately.
          (shell-command
           (format "%s -f %s/* %s"
                   cp (file-local-name tmpdir2) (file-local-name tmpdir1)))

          ;; Check, that the buffers have been reverted.
          (dolist (buf (list buf1 buf2))
            (with-current-buffer buf
              (auto-revert--wait-for-revert buf)
              (should (string-match "another text" (buffer-string))))))

      ;; Exit.
      (ignore-errors
        (dolist (buf (list buf1 buf2))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))
      (ignore-errors (delete-directory tmpdir1 'recursive))
      (ignore-errors (delete-directory tmpdir2 'recursive)))))

(auto-revert--deftest-remote auto-revert-test01-auto-revert-several-files
  "Check autorevert for several remote files at once.")

;; This is inspired by Bug#23276.
(ert-deftest auto-revert-test02-auto-revert-deleted-file ()
  "Check autorevert for a deleted file."
  :tags '(:expensive-test)
  ;; Repeated unpredictable failures, bug#32645.
  ;; Unlikely to be hydra-specific?
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))

  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf desc)
    (unwind-protect
	(progn
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
	  (with-current-buffer buf
            (should-not auto-revert-notify-watch-descriptor)
            (should (string-equal (buffer-string) "any text"))
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that
            ;; it returns nil.
            (sleep-for 1)
            (auto-revert-mode 1)
            (should auto-revert-mode)
            (setq desc auto-revert-notify-watch-descriptor)

            ;; Remove file while reverting.  We simulate this by
            ;; modifying `before-revert-hook'.
            (add-hook
             'before-revert-hook
             (lambda ()
               (when auto-revert-debug
                 (message "%s deleted" buffer-file-name))
               (delete-file buffer-file-name))
             nil t)

            (ert-with-message-capture auto-revert--messages
              (sleep-for 1)
              (write-region "another text" nil tmpfile nil 'no-message)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer hasn't been reverted.  File
            ;; notification should be disabled, falling back to
            ;; polling.
            (should (string-match "any text" (buffer-string)))
            ;; With w32notify, and on emba, the `stopped' events are not sent.
            (or (eq file-notify--library 'w32notify)
                (getenv "EMACS_EMBA_CI")
                (should-not auto-revert-notify-watch-descriptor))

            ;; Once the file has been recreated, the buffer shall be
            ;; reverted.
            (kill-local-variable 'before-revert-hook)
            (ert-with-message-capture auto-revert--messages
              (sleep-for 1)
              (write-region "another text" nil tmpfile nil 'no-message)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should (string-match "another text" (buffer-string)))
            ;; When file notification is used, it must be reenabled
            ;; after recreation of the file.  We cannot expect that
            ;; the descriptor is the same, so we just check the
            ;; existence.
            (should (eq (null desc) (null auto-revert-notify-watch-descriptor)))

            ;; An empty file shall still be reverted.
            (ert-with-message-capture auto-revert--messages
              (sleep-for 1)
              (write-region "" nil tmpfile nil 'no-message)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should (string-equal "" (buffer-string)))))

      ;; Exit.
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(auto-revert--deftest-remote auto-revert-test02-auto-revert-deleted-file
  "Check autorevert for a deleted remote file.")

(ert-deftest auto-revert-test03-auto-revert-tail-mode ()
  "Check autorevert tail mode."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
        (ert-with-message-capture auto-revert--messages
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
	  (with-current-buffer buf
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
	    (auto-revert-tail-mode 1)
	    (should auto-revert-tail-mode)
            (erase-buffer)
            (insert "modified text\n")
            (set-buffer-modified-p nil)

	    ;; Modify file.  We wait for a second, in order to have
	    ;; another timestamp.
	    (sleep-for 1)
            (write-region "another text" nil tmpfile 'append 'no-message)

	    ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert buf)
            (should
             (string-match "modified text\nanother text" (buffer-string)))))

      ;; Exit.
      (ignore-errors (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(auto-revert--deftest-remote auto-revert-test03-auto-revert-tail-mode
  "Check remote autorevert tail mode.")

(ert-deftest auto-revert-test04-auto-revert-mode-dired ()
  "Check autorevert for dired."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let* ((tmpfile (make-temp-file "auto-revert-test"))
         (name (file-name-nondirectory tmpfile))
         buf)
    (unwind-protect
	(progn
	  (setq buf (dired-noselect temporary-file-directory))
	  (with-current-buffer buf
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
            (auto-revert-mode 1)
            (should auto-revert-mode)
	    (should
             (string-match name (substring-no-properties (buffer-string))))

            (ert-with-message-capture auto-revert--messages
              ;; Delete file.  We wait for a second, in order to have
              ;; another timestamp.
              (sleep-for 1)
              (delete-file tmpfile)
              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should-not
             (string-match name (substring-no-properties (buffer-string))))

            (ert-with-message-capture auto-revert--messages
              ;; Make dired buffer modified.  Check, that the buffer has
              ;; been still reverted.
              (set-buffer-modified-p t)
              (sleep-for 1)
              (write-region "any text" nil tmpfile nil 'no-message)

              (auto-revert--wait-for-revert buf))
            ;; Check, that the buffer has been reverted.
            (should
             (string-match name (substring-no-properties (buffer-string))))))

      ;; Exit.
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(auto-revert--deftest-remote auto-revert-test04-auto-revert-mode-dired
  "Check remote autorevert for dired.")

(defun auto-revert-test-all (&optional interactive)
  "Run all tests for \\[auto-revert]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^auto-revert-")
    (ert-run-tests-batch "^auto-revert-")))

(provide 'auto-revert-tests)
;;; auto-revert-tests.el ends here
