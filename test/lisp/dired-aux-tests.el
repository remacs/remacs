;;; dired-aux-tests.el --- Test suite for dired-aux. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'dired-aux)


(ert-deftest dired-test-bug27496 ()
  "Test for http://debbugs.gnu.org/27496 ."
  (skip-unless (executable-find shell-file-name))
  (let* ((foo (make-temp-file "foo"))
         (files (list foo)))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) 'error))
          (dired temporary-file-directory)
          (dired-goto-file foo)
          ;; `dired-do-shell-command' returns nil on success.
          (should-error (dired-do-shell-command "ls ? ./?" nil files))
          (should-error (dired-do-shell-command "ls ./? ?" nil files))
          (should-not (dired-do-shell-command "ls ? ?" nil files))
          (should-error (dired-do-shell-command "ls * ./*" nil files))
          (should-not (dired-do-shell-command "ls * *" nil files))
          (should-not (dired-do-shell-command "ls ? ./`?`" nil files)))
      (delete-file foo))))

(provide 'dired-aux-tests)
;; dired-aux-tests.el ends here
