;;; custom-tests.el --- tests for custom.el  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(ert-deftest custom-theme--load-path ()
  "Test `custom-theme--load-path' behavior."
  (let ((tmpdir (file-name-as-directory (make-temp-file "custom-tests-" t))))
    (unwind-protect
        ;; Create all temporary files under the same deletable parent.
        (let ((temporary-file-directory tmpdir))
          ;; Path is empty.
          (let ((custom-theme-load-path ()))
            (should (null (custom-theme--load-path))))

          ;; Path comprises non-existent file.
          (let* ((name (make-temp-name tmpdir))
                 (custom-theme-load-path (list name)))
            (should (not (file-exists-p name)))
            (should (null (custom-theme--load-path))))

          ;; Path comprises existing file.
          (let* ((file (make-temp-file "file"))
                 (custom-theme-load-path (list file)))
            (should (file-exists-p file))
            (should (not (file-directory-p file)))
            (should (null (custom-theme--load-path))))

          ;; Path comprises existing directory.
          (let* ((dir (make-temp-file "dir" t))
                 (custom-theme-load-path (list dir)))
            (should (file-directory-p dir))
            (should (equal (custom-theme--load-path) custom-theme-load-path)))

          ;; Expand `custom-theme-directory' path element.
          (let ((custom-theme-load-path '(custom-theme-directory)))
            (let ((custom-theme-directory (make-temp-name tmpdir)))
              (should (not (file-exists-p custom-theme-directory)))
              (should (null (custom-theme--load-path))))
            (let ((custom-theme-directory (make-temp-file "file")))
              (should (file-exists-p custom-theme-directory))
              (should (not (file-directory-p custom-theme-directory)))
              (should (null (custom-theme--load-path))))
            (let ((custom-theme-directory (make-temp-file "dir" t)))
              (should (file-directory-p custom-theme-directory))
              (should (equal (custom-theme--load-path)
                             (list custom-theme-directory)))))

          ;; Expand t path element.
          (let ((custom-theme-load-path '(t)))
            (let ((data-directory (make-temp-name tmpdir)))
              (should (not (file-exists-p data-directory)))
              (should (null (custom-theme--load-path))))
            (let ((data-directory tmpdir)
                  (themedir (expand-file-name "themes" tmpdir)))
              (should (not (file-exists-p themedir)))
              (should (null (custom-theme--load-path)))
              (with-temp-file themedir)
              (should (file-exists-p themedir))
              (should (not (file-directory-p themedir)))
              (should (null (custom-theme--load-path)))
              (delete-file themedir)
              (make-directory themedir)
              (should (file-directory-p themedir))
              (should (equal (custom-theme--load-path) (list themedir))))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

;;; custom-tests.el ends here
