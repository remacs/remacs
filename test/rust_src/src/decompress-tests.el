;;; decompress-tests.el --- Test suite for decompress.

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(defvar zlib-tests-data-directory
  (expand-file-name "data/decompress" (getenv "EMACS_TEST_DIRECTORY"))
  "Directory containing zlib test data.")

(ert-deftest zlib--decompress ()
  "Test decompressing a gzipped file."
  (when (and (fboundp 'zlib-available-p)
	     (zlib-available-p))
    (should (string=
	     (with-temp-buffer
	       (set-buffer-multibyte nil)
	       (insert-file-contents-literally
		(expand-file-name "foo.gz" zlib-tests-data-directory))
	       (zlib-decompress-region (point-min) (point-max))
	       (buffer-string))
	     "foo\n"))))

(ert-deftest zlib--decompress-large-file ()
  "Test decompressing a large gzipped file."
  (when (and (fboundp 'zlib-available-p)
	     (zlib-available-p))
    (should (string=
	     (with-temp-buffer
	       (set-buffer-multibyte nil)
	       (insert-file-contents-literally
		(expand-file-name "32k-a.gz" zlib-tests-data-directory))
	       (zlib-decompress-region (point-min) (point-max))
	       (buffer-string))
             ;; 32kb 'a' repeat string
             (make-string (* 32 1024) ?a)))))

(ert-deftest zlib--decompress-empty-buffer ()
  "Test decompressing an empty buffer."
  (when (and (fboundp 'zlib-available-p)
	     (zlib-available-p))
    (should (not (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (zlib-decompress-region (point-min) (point-max)))))))

(provide 'decompress-tests)

;;; decompress-tests.el ends here.
