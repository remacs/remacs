;;; ibuffer-tests.el --- Test suite. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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
(require 'ibuffer)
(eval-when-compile
  (require 'ibuf-macs))

(ert-deftest ibuffer-autoload ()
  "Tests to see whether reftex-auc has been autoloaded"
  (should
   (fboundp 'ibuffer-mark-unsaved-buffers))
  (should
   (autoloadp
    (symbol-function
     'ibuffer-mark-unsaved-buffers))))

(ert-deftest ibuffer-test-Bug25000 ()
  "Test for http://debbugs.gnu.org/25000 ."
  :expected-result :failed
  (let ((case-fold-search t)
        (buf1 (generate-new-buffer "ibuffer-test-Bug25000-buf1"))
        (buf2 (generate-new-buffer "ibuffer-test-Bug25000-buf2")))
    (ibuffer)
    (unwind-protect
        (ibuffer-save-marks
          (ibuffer-unmark-all-marks)
          (ibuffer-mark-by-name-regexp (buffer-name buf1))
          (ibuffer-change-marks ibuffer-marked-char ?L)
          (ibuffer-mark-by-name-regexp (buffer-name buf2))
          (ibuffer-change-marks ibuffer-marked-char ?l)
          (should-not (cdr (ibuffer-buffer-names-with-mark ?l))))
      (mapc (lambda (buf) (when (buffer-live-p buf)
                            (kill-buffer buf))) (list buf1 buf2)))))

(provide 'ibuffer-tests)
;; ibuffer-tests.el ends here
