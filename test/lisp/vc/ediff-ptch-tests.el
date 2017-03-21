;;; ediff-ptch-tests.el --- Tests for ediff-ptch.el

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'ediff-ptch)

(ert-deftest ediff-ptch-test-bug25010 ()
  "Test for http://debbugs.gnu.org/25010 ."
  (with-temp-buffer
    (insert "diff --git a/lisp/vc/ediff-ptch.el b/lisp/vc/ediff-ptch.el
index 6a07f80..6e8e947 100644
--- a/lisp/vc/ediff-ptch.el
+++ b/lisp/vc/ediff-ptch.el
@@ -120,11 +120,12 @@ ediff-patch-default-directory
")
    (goto-char (point-min))
    (let ((filename
           (progn
             (re-search-forward ediff-context-diff-label-regexp nil t)
             (match-string 1))))
      (should-not (string-suffix-p "@@" filename)))))


(ert-deftest ediff-ptch-test-bug26084 ()
  "Test for http://debbugs.gnu.org/26084 ."
  (let* ((tmpdir temporary-file-directory)
         (foo (expand-file-name "foo" tmpdir))
         (patch (expand-file-name "foo.diff" tmpdir))
         (qux (expand-file-name "qux.txt" foo))
         (bar (expand-file-name "bar.txt" foo))
         (cmd "
mkdir -p foo
cd foo
echo 'qux here' > qux.txt
echo 'bar here' > bar.txt
git init
git add . && git commit -m 'Test repository.'
echo 'foo here' > qux.txt
echo 'foo here' > bar.txt
git diff > ../foo.diff
git reset --hard HEAD
"))
    (setq default-directory tmpdir)
    (call-process-shell-command cmd)
    (find-file patch)
    (unwind-protect
        (let* ((info
                (progn (ediff-map-patch-buffer (current-buffer)) ediff-patch-map))
               (patch1
                (buffer-substring-no-properties
                 (car (nth 3 (car info)))
                 (car (nth 4 (car info)))))
               (patch2
                (buffer-substring-no-properties
                 (car (nth 3 (cadr info)))
                 (car (nth 4 (cadr info))))))
          ;; Apply both patches.
          (dolist (x (list (cons patch1 bar) (cons patch2 qux)))
            (with-temp-buffer
              (insert (car x))
              (call-shell-region (point-min)
                                 (point-max)
                                 (format "%s %s %s %s"
                                         ediff-patch-program
                                         ediff-patch-options
                                         ediff-backup-specs
                                         (cdr x)))))
          ;; Check backup files were saved correctly.
          (dolist (x (list qux bar))
            (should-not (string= (with-temp-buffer
                                   (insert-file-contents x)
                                   (buffer-string))
                                 (with-temp-buffer
                                   (insert-file-contents (concat x ediff-backup-extension))
                                   (buffer-string))))))
      (delete-directory foo 'recursive)
      (delete-file patch))))


(provide 'ediff-ptch-tests)
;;; ediff-ptch-tests.el ends here
