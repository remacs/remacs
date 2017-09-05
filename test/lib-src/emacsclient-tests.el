;;; emacsclient-tests.el --- Test emacsclient

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

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

;;; Commentary:

;;

;;; Code:

(require 'ert)

(defconst emacsclient-test-emacs
  (if installation-directory
      (expand-file-name "lib-src/emacsclient" installation-directory)
    "emacsclient")
  "The emacsclient binary to test.")

(defun emacsclient-test-call-emacsclient ()
  "Run emacsclient."
  (call-process emacsclient-test-emacs nil nil nil
                "--server-file" (expand-file-name "non-existent-file" invocation-directory)
                "foo"))

(ert-deftest emacsclient-test-alternate-editor-allows-arguments ()
  (let ((process-environment process-environment))
    (setenv "ALTERNATE_EDITOR" (concat
                                (expand-file-name invocation-name invocation-directory)
                                " --batch"))
    (should (eq 0 (emacsclient-test-call-emacsclient)))))

(ert-deftest emacsclient-test-alternate-editor-allows-quotes ()
  (let ((process-environment process-environment))
    (setenv "ALTERNATE_EDITOR" (concat
                                "\""
                                (expand-file-name invocation-name invocation-directory)
                                "\"" " --batch"))
    (should (eq 0 (emacsclient-test-call-emacsclient)))))

(provide 'emacsclient-tests)
;;; emacsclient-tests.el ends here
