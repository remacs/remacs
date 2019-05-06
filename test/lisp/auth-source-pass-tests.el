;;; auth-source-pass-tests.el --- Tests for auth-source-pass.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2017-2019 Free Software Foundation, Inc.

;; Author: Damien Cassou <damien.cassou@gmail.com>

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

;; Tests for auth-source-pass.el

;;; Code:

(require 'ert)

(require 'auth-source-pass)

(eval-when-compile (require 'cl-lib))

(ert-deftest auth-source-pass-parse-simple ()
  (let ((content "pass\nkey1:val1\nkey2:val2\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(ert-deftest auth-source-pass-parse-with-dash-line ()
  (let ((content "pass\n--\nkey1:val1\nkey2:val2\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(ert-deftest auth-source-pass-parse-with-trailing-spaces ()
  (let ((content "pass\n--\nkey1 :val1   \nkey2:   val2\n\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(defvar auth-source-pass--debug-log nil
  "Contains a list of all messages passed to `auth-source-do-debug`.")

(defun auth-source-pass--should-have-message-containing (regexp)
  "Assert that at least one `auth-source-do-debug` matched REGEXP."
  (should (seq-find (lambda (message)
                      (string-match regexp message))
                    auth-source-pass--debug-log)))

(defun auth-source-pass--debug (&rest msg)
  "Format MSG and add that to `auth-source-pass--debug-log`.
This function is intended to be set to `auth-source-debug`."
  (add-to-list 'auth-source-pass--debug-log (apply #'format msg) t))

(defvar auth-source-pass--parse-log nil)

(defmacro auth-source-pass--with-store (store &rest body)
  "Use STORE as password-store while executing BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'auth-source-pass-parse-entry)
              (lambda (entry)
                (add-to-list 'auth-source-pass--parse-log entry)
                (cdr (cl-find entry ,store :key #'car :test #'string=))))
             ((symbol-function 'auth-source-pass-entries) (lambda () (mapcar #'car ,store))))
     (let ((auth-source-debug #'auth-source-pass--debug)
           (auth-source-pass--debug-log nil)
           (auth-source-pass--parse-log nil))
       ,@body)))

(ert-deftest auth-source-pass-any-host ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host t))))

(ert-deftest auth-source-pass-undefined-host ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host nil))))

(ert-deftest auth-source-pass-not-found ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host "baz"))))

(ert-deftest auth-source-pass-find-match-minimal-parsing ()
  (let ((store-contents
         '(("baz" ("secret" . "baz password"))
           ("baz:123" ("secret" . "baz:123 password"))
           ("baz/foo" ("secret" . "baz/foo password"))
           ("foo@baz" ("secret" . "foo@baz password"))
           ("baz:123/foo" ("secret" . "baz:123/foo password"))
           ("foo@baz:123" ("secret" . "foo@baz:123 password"))
           ("bar.baz" ("secret" . "bar.baz password"))
           ("bar.baz:123" ("secret" . "bar.baz:123 password"))
           ("bar.baz/foo" ("secret" . "bar.baz/foo password"))
           ("foo@bar.baz" ("secret" . "foo@bar.baz password"))
           ("bar.baz:123/foo" ("secret" . "bar.baz:123/foo password"))
           ("foo@bar.baz:123" ("secret" . "foo@bar.baz:123 password")))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" "foo" "123")
      (should (equal auth-source-pass--parse-log '("foo@bar.baz:123"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" "foo" nil)
      (should (equal auth-source-pass--parse-log '("foo@bar.baz"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" nil "123")
      (should (equal auth-source-pass--parse-log '("bar.baz:123"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" nil nil)
      (should (equal auth-source-pass--parse-log '("bar.baz"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "baz" nil nil)
      (should (equal auth-source-pass--parse-log '("baz"))))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--find-match "foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "foo password")))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-part ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--find-match "https://foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "foo password")))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-ignoring-user ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--find-match "https://SomeUser@foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "foo password")))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-with-user ()
  (auth-source-pass--with-store
      '(("SomeUser@foo" ("secret" . "SomeUser@foo password")))
    (let ((result (auth-source-pass--find-match "https://SomeUser@foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "SomeUser@foo password")))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-prefer-full ()
  (auth-source-pass--with-store
      '(("SomeUser@foo" ("secret" . "SomeUser@foo password"))
        ("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--find-match "https://SomeUser@foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "SomeUser@foo password")))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-prefer-full-reversed ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password"))
        ("SomeUser@foo" ("secret" . "SomeUser@foo password")))
    (let ((result (auth-source-pass--find-match "https://SomeUser@foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "SomeUser@foo password")))))

(ert-deftest auth-source-pass-matching-entries-name-without-subdomain ()
  (auth-source-pass--with-store '(("bar.com"))
    (should (equal (auth-source-pass--matching-entries "foo.bar.com" nil nil)
                   '(nil ("bar.com") nil)))))

(ert-deftest auth-source-pass-matching-entries-name-without-subdomain-with-user ()
  (auth-source-pass--with-store '(("someone@bar.com"))
    (should (equal (auth-source-pass--matching-entries "foo.bar.com" "someone" nil)
                   '(nil nil nil ("someone@bar.com") nil nil nil nil nil)))))

(ert-deftest auth-source-pass-matching-entries-name-without-subdomain-with-bad-user ()
  (auth-source-pass--with-store '(("someoneelse@bar.com"))
    (should (equal (auth-source-pass--matching-entries "foo.bar.com" "someone" nil)
                   '(nil nil nil nil nil nil nil nil nil)))))

(ert-deftest auth-source-pass-matching-entries-name-without-subdomain-prefer-full ()
  (auth-source-pass--with-store '(("bar.com") ("foo.bar.com"))
    (should (equal (auth-source-pass--matching-entries "foo.bar.com" nil nil)
                   '(("foo.bar.com") ("bar.com") nil)))))

(ert-deftest auth-source-pass-dont-match-at-folder-name ()
  (auth-source-pass--with-store '(("foo.bar.com/foo"))
    (should (equal (auth-source-pass--matching-entries "foo.bar.com" nil nil)
                   '(nil nil nil)))))

(ert-deftest auth-source-pass-matching-entries-host-port-and-subdir-user ()
  (auth-source-pass--with-store '(("bar.com:443/someone"))
    (should (equal (auth-source-pass--matching-entries "bar.com" "someone" "443")
                   '(nil ("bar.com:443/someone") nil nil nil nil
                         nil nil nil nil nil nil)))))

(ert-deftest auth-source-pass-matching-entries-host-port-and-subdir-user-with-custom-separator ()
  (let ((auth-source-pass-port-separator "#"))
    (auth-source-pass--with-store '(("bar.com#443/someone"))
      (should (equal (auth-source-pass--matching-entries "bar.com" "someone" "443")
                     '(nil ("bar.com#443/someone") nil nil nil nil
                           nil nil nil nil nil nil))))))

(ert-deftest auth-source-pass-matching-entries-extracting-user-from-host ()
  (auth-source-pass--with-store
      '(("foo.com/bar" ("secret" . "foo.com/bar password")))
    (let ((result (auth-source-pass--find-match "https://bar@foo.com" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "foo.com/bar password")))))

(ert-deftest auth-source-pass-matching-entries-with-user-first ()
  (auth-source-pass--with-store '(("foo") ("user@foo"))
    (should (equal (auth-source-pass--matching-entries "foo" "user" nil)
                   '(("user@foo") nil ("foo"))))
    (auth-source-pass--should-have-message-containing "found: (\"user@foo\" \"foo\"")))

(ert-deftest auth-source-pass-give-priority-to-desired-user ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password"))
        ("subdir/foo" ("secret" . "subdir/foo password") ("user" . "someone")))
    (let ((result (auth-source-pass--find-match "foo" "someone" nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "subdir/foo password"))
      (should (equal (auth-source-pass--get-attr "user" result)
                     "someone")))
    (auth-source-pass--should-have-message-containing "found: (\"foo\" \"subdir/foo\"")))

(ert-deftest auth-source-pass-give-priority-to-desired-user-reversed ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password") ("user" . "someone"))
        ("subdir/foo" ("secret" . "subdir/foo password")))
    (let ((result (auth-source-pass--find-match "foo" "someone" nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "foo password")))
    (auth-source-pass--should-have-message-containing "found: (\"foo\" \"subdir/foo\"")))

(ert-deftest auth-source-pass-return-first-when-several-matches ()
  (auth-source-pass--with-store
      '(("foo" ("secret" . "foo password"))
        ("subdir/foo" ("secret" . "subdir/foo password")))
    (let ((result (auth-source-pass--find-match "foo" nil nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "foo password")))
    (auth-source-pass--should-have-message-containing "found: (\"foo\" \"subdir/foo\"")))

(ert-deftest auth-source-pass-matching-entries-make-divansantana-happy ()
  (auth-source-pass--with-store '(("host.com"))
    (should (equal (auth-source-pass--matching-entries "smtp.host.com" "myusername@host.co.za" nil)
                   '(nil nil nil nil nil ("host.com") nil nil nil)))))

(ert-deftest auth-source-pass-find-host-without-port ()
  (auth-source-pass--with-store
      '(("host.com" ("secret" . "host.com password")))
    (let ((result (auth-source-pass--find-match "host.com:8888" "someuser" nil)))
      (should (equal (auth-source-pass--get-attr "secret" result)
                     "host.com password")))))

(ert-deftest auth-source-pass-matching-entries-host-with-port ()
  (auth-source-pass--with-store '(("host.com:443"))
    (should (equal (auth-source-pass--matching-entries "host.com" "someuser" "443")
                   '(nil nil nil nil ("host.com:443") nil
                         nil nil nil nil nil nil)))))

(ert-deftest auth-source-pass-matching-entries-with-custom-port-separator ()
  (let ((auth-source-pass-port-separator "#"))
    (auth-source-pass--with-store '(("host.com#443"))
      (should (equal (auth-source-pass--matching-entries "host.com" "someuser" "443")
                     '(nil nil nil nil ("host.com#443") nil
                           nil nil nil nil nil nil))))))

(defmacro auth-source-pass--with-store-find-foo (store &rest body)
  "Use STORE while executing BODY.  \"foo\" is the matched entry."
  (declare (indent 1))
  `(auth-source-pass--with-store ,store
     (cl-letf (((symbol-function 'auth-source-pass-find-match)
                (lambda (_host _user)
                  "foo")))
       ,@body)))

(ert-deftest auth-source-pass-build-result-return-parameters ()
  (auth-source-pass--with-store-find-foo
      '(("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--build-result "foo" 512 "user")))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "user")))))

(ert-deftest auth-source-pass-build-result-return-entry-values ()
  (auth-source-pass--with-store-find-foo '(("foo" ("port" . 512) ("user" . "anuser")))
    (let ((result (auth-source-pass--build-result "foo" nil nil)))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "anuser")))))

(ert-deftest auth-source-pass-build-result-entry-takes-precedence ()
  (auth-source-pass--with-store-find-foo '(("foo" ("port" . 512) ("user" . "anuser")))
    (let ((result (auth-source-pass--build-result "foo" 1024 "anotheruser")))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "anuser")))))

(ert-deftest auth-source-pass-build-result-passes-full-host-to-find-match ()
  (let (passed-host)
    (cl-letf (((symbol-function 'auth-source-pass--find-match)
               (lambda (host _user _port)
                 (setq passed-host host)
                 nil)))
      (auth-source-pass--build-result "https://user@host.com:123" nil nil)
      (should (equal passed-host "https://user@host.com:123"))
      (auth-source-pass--build-result "https://user@host.com" nil nil)
      (should (equal passed-host "https://user@host.com"))
      (auth-source-pass--build-result "user@host.com" nil nil)
      (should (equal passed-host "user@host.com"))
      (auth-source-pass--build-result "user@host.com:443" nil nil)
      (should (equal passed-host "user@host.com:443")))))

(ert-deftest auth-source-pass-only-return-entries-that-can-be-open ()
  (auth-source-pass--with-store
      '(("foo.site.com" ("secret" . "foo.site.com password"))
        ("bar.site.com") ; An entry name with no data is invalid
        ("mail/baz.site.com/scott" ("secret" . "mail/baz.site.com/scott password")))
    (should (equal (auth-source-pass--find-match "foo.site.com" "someuser" nil)
                   '(("secret" . "foo.site.com password"))))
    (should (equal (auth-source-pass--find-match "bar.site.com" "someuser" nil)
                   nil))
    (should (equal (auth-source-pass--find-match "baz.site.com" "scott" nil)
                   '(("secret" . "mail/baz.site.com/scott password"))))))

(ert-deftest auth-source-pass-can-start-from-auth-source-search ()
  (auth-source-pass--with-store '(("gitlab.com" ("user" . "someone")))
    (auth-source-pass-enable)
    (let ((result (car (auth-source-search :host "gitlab.com"))))
      (should (equal (plist-get result :user) "someone"))
      (should (equal (plist-get result :host) "gitlab.com")))))

(provide 'auth-source-pass-tests)

;;; auth-source-pass-tests.el ends here
