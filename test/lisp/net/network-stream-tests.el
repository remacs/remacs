;;; network-stream-tests.el --- tests for network processes       -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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


;;; Code:

(ert-deftest make-local-unix-server ()
  (let* ((file (make-temp-name "/tmp/server-test"))
         (server
          (make-network-process
           :name "server"
           :server t
           :buffer (get-buffer-create "*server*")
           :noquery t
           :family 'local
           :service file)))
    (should (equal (process-contact server :local) file))
    (delete-file (process-contact server :local))))

(ert-deftest make-local-tcp-server-with-unspecified-port ()
  (let ((server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :host 'local)))
    (should (and (arrayp (process-contact server :local))
                 (numberp (aref (process-contact server :local) 4))
                 (> (aref (process-contact server :local) 4) 0)))
    (delete-process server)))

(ert-deftest make-local-tcp-server-with-specified-port ()
  (let ((server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service 57869
          :host 'local)))
    (should (and (arrayp (process-contact server :local))
                 (= (aref (process-contact server :local) 4) 57869)))
    (delete-process server)))

(defun make-server (host)
  (make-network-process
   :name "server"
   :server t
   :noquery t
   :family 'ipv4
   :coding 'raw-text-unix
   :buffer (get-buffer-create "*server*")
   :service t
   :sentinel 'server-sentinel
   :filter 'server-process-filter
   :host host))

(defun server-sentinel (proc msg)
  )

(defun server-process-filter (proc string)
  (message "Received %s" string)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (if (and (not (string-match "\n" string))
           (> (length string) 0))
      (process-put proc 'previous-string string))
  (let ((command (split-string string)))
    (cond
     ((equal (car command) "echo")
      (process-send-string proc (concat (cadr command) "\n")))
     (t
      ))))

(ert-deftest echo-server-with-dns ()
  (let* ((server (make-server "mouse"))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host (system-name)
                                     :service port)))
    (with-current-buffer "*foo*"
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))))

(ert-deftest echo-server-with-localhost ()
  (let* ((server (make-server 'local))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host "localhost"
                                     :service port)))
    (with-current-buffer "*foo*"
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))))

(ert-deftest echo-server-with-ip ()
  (let* ((server (make-server 'local))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host "127.0.0.1"
                                     :service port)))
    (with-current-buffer "*foo*"
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))))

;;; network-stream-tests.el ends here
