;;; timefns-tests.el -- tests for timefns.c

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'ert)

;;; Check format-time-string with various TZ settings.
;;; Use only POSIX-compatible TZ values, since the tests should work
;;; even if tzdb is not in use.
(ert-deftest format-time-string-with-zone ()
  ;; Don’t use (0 0 0 0) as the test case, as there are too many bugs
  ;; in MS-Windows (and presumably other) C libraries when formatting
  ;; time stamps near the Epoch of 1970-01-01 00:00:00 UTC, and this
  ;; test is for GNU Emacs, not for C runtimes.  Instead, look before
  ;; you leap: "look" is the timestamp just before the first leap
  ;; second on 1972-06-30 23:59:60 UTC, so it should format to the
  ;; same string regardless of whether the underlying C library
  ;; ignores leap seconds, while avoiding circa-1970 glitches.
  ;;
  ;; Similarly, stick to the limited set of time zones that are
  ;; supported by both POSIX and MS-Windows: exactly 3 ASCII letters
  ;; in the abbreviation, and no DST.
  (let ((look '(1202 22527 999999 999999))
        (format "%Y-%m-%d %H:%M:%S.%3N %z (%Z)"))
    ;; UTC.
    (should (string-equal
             (format-time-string "%Y-%m-%d %H:%M:%S.%3N %z" look t)
             "1972-06-30 23:59:59.999 +0000"))
    ;; "UTC0".
    (should (string-equal
             (format-time-string format look "UTC0")
             "1972-06-30 23:59:59.999 +0000 (UTC)"))
    ;; Negative UTC offset, as a Lisp list.
    (should (string-equal
             (format-time-string format look '(-28800 "PST"))
             "1972-06-30 15:59:59.999 -0800 (PST)"))
    ;; Negative UTC offset, as a Lisp integer.
    (should (string-equal
             (format-time-string format look -28800)
             ;; MS-Windows build replaces unrecognizable TZ values,
             ;; such as "-08", with "ZZZ".
             (if (eq system-type 'windows-nt)
                 "1972-06-30 15:59:59.999 -0800 (ZZZ)"
               "1972-06-30 15:59:59.999 -0800 (-08)")))
    ;; Positive UTC offset that is not an hour multiple, as a string.
    (should (string-equal
             (format-time-string format look "IST-5:30")
             "1972-07-01 05:29:59.999 +0530 (IST)"))))

;;; This should not dump core.
(ert-deftest format-time-string-with-outlandish-zone ()
  (should (stringp
           (format-time-string "%Y-%m-%d %H:%M:%S.%3N %z" nil
                               (concat (make-string 2048 ?X) "0")))))

(defun timefns-tests--have-leap-seconds ()
  (string-equal (format-time-string "%Y-%m-%d %H:%M:%S" 78796800 t)
                "1972-06-30 23:59:60"))

(ert-deftest format-time-string-with-bignum-on-32-bit ()
  (should (or (string-equal
               (format-time-string "%Y-%m-%d %H:%M:%S" (- (ash 1 31) 3600) t)
               "2038-01-19 02:14:08")
              (timefns-tests--have-leap-seconds))))

(ert-deftest time-equal-p-nil-nil ()
  (should (time-equal-p nil nil)))
