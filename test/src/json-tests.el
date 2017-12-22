;;; json-tests.el --- unit tests for json.c          -*- lexical-binding: t; -*-

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for src/json.c.

;;; Code:

(require 'cl-lib)
(require 'map)

(ert-deftest json-serialize/roundtrip ()
  (skip-unless (fboundp 'json-serialize))
  ;; The noncharacter U+FFFF should be passed through,
  ;; cf. https://www.unicode.org/faq/private_use.html#noncharacters.
  (let ((lisp [:null :false t 0 123 -456 3.75 "abc\uFFFFαβγ𝔸𝐁𝖢\"\\"])
        (json "[null,false,true,0,123,-456,3.75,\"abc\uFFFFαβγ𝔸𝐁𝖢\\\"\\\\\"]"))
    (should (equal (json-serialize lisp) json))
    (with-temp-buffer
      (json-insert lisp)
      (should (equal (buffer-string) json))
      (should (eobp)))
    (should (equal (json-parse-string json) lisp))
    (with-temp-buffer
      (insert json)
      (goto-char 1)
      (should (equal (json-parse-buffer) lisp))
      (should (eobp)))))

(ert-deftest json-serialize/object ()
  (skip-unless (fboundp 'json-serialize))
  (let ((table (make-hash-table :test #'equal)))
    (puthash "abc" [1 2 t] table)
    (puthash "def" :null table)
    (should (equal (json-serialize table)
                   "{\"abc\":[1,2,true],\"def\":null}"))))

(ert-deftest json-parse-string/object ()
  (skip-unless (fboundp 'json-parse-string))
  (let ((input
         "{ \"abc\" : [1, 2, true], \"def\" : null, \"abc\" : [9, false] }\n"))
    (let ((actual (json-parse-string input)))
      (should (hash-table-p actual))
      (should (equal (hash-table-count actual) 2))
      (should (equal (cl-sort (map-pairs actual) #'string< :key #'car)
                     '(("abc" . [9 :false]) ("def" . :null)))))
    (should (equal (json-parse-string input :object-type 'alist)
                   '((abc . [9 :false]) (def . :null))))))

(ert-deftest json-parse-string/string ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "[\"formfeed\f\"]") :type 'json-parse-error)
  (should (equal (json-parse-string "[\"foo \\\"bar\\\"\"]") ["foo \"bar\""]))
  (should (equal (json-parse-string "[\"abcαβγ\"]") ["abcαβγ"]))
  (should (equal (json-parse-string "[\"\\nasd\\u0444\\u044b\\u0432fgh\\t\"]")
                 ["\nasdфывfgh\t"]))
  (should (equal (json-parse-string "[\"\\uD834\\uDD1E\"]") ["\U0001D11E"]))
  (should-error (json-parse-string "foo") :type 'json-parse-error))

(ert-deftest json-serialize/string ()
  (skip-unless (fboundp 'json-serialize))
  (should (equal (json-serialize ["foo"]) "[\"foo\"]"))
  (should (equal (json-serialize ["a\n\fb"]) "[\"a\\n\\fb\"]"))
  (should (equal (json-serialize ["\nasdфыв\u001f\u007ffgh\t"])
                 "[\"\\nasdфыв\\u001F\u007ffgh\\t\"]"))
  (should (equal (json-serialize ["a\0b"]) "[\"a\\u0000b\"]")))

(ert-deftest json-serialize/invalid-unicode ()
  (skip-unless (fboundp 'json-serialize))
  ;; FIXME: "out of memory" is the wrong error signal, but we don't
  ;; currently distinguish between error types when serializing.
  (should-error (json-serialize ["a\uDBBBb"]) :type 'json-out-of-memory)
  (should-error (json-serialize ["u\x110000v"]) :type 'json-out-of-memory)
  (should-error (json-serialize ["u\xCCv"]) :type 'json-out-of-memory))

(ert-deftest json-parse-string/null ()
  (skip-unless (fboundp 'json-parse-string))
  ;; FIXME: Reconsider whether this is the right behavior.
  (should-error (json-parse-string "[a\\u0000b]") :type 'json-parse-error))

(ert-deftest json-parse-string/incomplete ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "[123") :type 'json-end-of-file))

(ert-deftest json-parse-string/trailing ()
  (skip-unless (fboundp 'json-parse-string))
  (should-error (json-parse-string "[123] [456]") :type 'json-trailing-content))

(ert-deftest json-parse-buffer/incomplete ()
  (skip-unless (fboundp 'json-parse-buffer))
  (with-temp-buffer
    (insert "[123")
    (goto-char 1)
    (should-error (json-parse-buffer) :type 'json-end-of-file)
    (should (bobp))))

(ert-deftest json-parse-buffer/trailing ()
  (skip-unless (fboundp 'json-parse-buffer))
  (with-temp-buffer
    (insert "[123] [456]")
    (goto-char 1)
    (should (equal (json-parse-buffer) [123]))
    (should-not (bobp))
    (should (looking-at-p (rx " [456]" eos)))))

(provide 'json-tests)
;;; json-tests.el ends here
