;;; gravatar.el --- Get Gravatars

;; Copyright (C) 2010-2019 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: news

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

;;; Code:

(require 'url)
(require 'url-cache)
(require 'image)

(defgroup gravatar nil
  "Gravatar."
  :version "24.1"
  :group 'comm)

(defcustom gravatar-automatic-caching t
  "Whether to cache retrieved gravatars."
  :type 'boolean
  :group 'gravatar)

;; FIXME a time value is not the nicest format for a custom variable.
(defcustom gravatar-cache-ttl (days-to-time 30)
  "Time to live for gravatar cache entries.
If a requested gravatar has been cached for longer than this, it
is retrieved anew."
  :type '(repeat integer)
  :group 'gravatar)

(defcustom gravatar-rating "g"
  "Most explicit Gravatar rating level to allow.
Some gravatars are rated according to how suitable they are for
different audiences.  The supported rating levels are, in order
of increasing explicitness, the following:

\"g\"  - Suitable for any audience.
\"pg\" - May contain rude gestures, provocatively dressed
       individuals, mild profanity, or mild violence.
\"r\"  - May contain harsh profanity, intense violence, nudity,
       or hard drug use.
\"x\"  - May contain hardcore sexual imagery or extremely
       disturbing violence.

Each level covers itself as well as all less explicit levels.
For example, setting this variable to \"pg\" will allow gravatars
rated either \"g\" or \"pg\"."
  :type 'string
  :group 'gravatar)

(defcustom gravatar-size 32
  "Gravatar size in pixels to request.
Valid sizes range from 1 to 2048 inclusive."
  :type 'integer
  :group 'gravatar)

(defconst gravatar-base-url
  "http://www.gravatar.com/avatar"
  "Base URL for getting gravatars.")

(defun gravatar-hash (mail-address)
  "Create a hash from MAIL-ADDRESS."
  (md5 (downcase mail-address)))

(defun gravatar-build-url (mail-address)
  "Return a URL to retrieve MAIL-ADDRESS gravatar."
  (format "%s/%s?d=404&r=%s&s=%d"
          gravatar-base-url
          (gravatar-hash mail-address)
          gravatar-rating
          gravatar-size))

(defun gravatar-cache-expired (url)
  "Check if URL is cached for more than `gravatar-cache-ttl'."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   gravatar-cache-ttl)
                  (current-time))
               t)))))

(defun gravatar-get-data ()
  "Get data from current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/.+ 200 OK$" nil (line-end-position))
      (when (search-forward "\n\n" nil t)
        (buffer-substring (point) (point-max))))))

(defun gravatar-data->image ()
  "Get data of current buffer and return an image.
If no image available, return 'error."
  (let ((data (gravatar-get-data)))
    (if data
	(create-image data nil t)
      'error)))

(autoload 'help-function-arglist "help-fns")

;;;###autoload
(defun gravatar-retrieve (mail-address cb &optional cbargs)
  "Asynchronously retrieve a gravatar for MAIL-ADDRESS.
When finished, call CB as (apply CB GRAVATAR CBARGS),
where GRAVATAR is either an image descriptor, or the symbol
`error' if the retrieval failed."
  (let ((url (gravatar-build-url mail-address)))
    (if (gravatar-cache-expired url)
	(let ((args (list url
			  'gravatar-retrieved
			  (list cb (when cbargs cbargs)))))
	  (when (> (length (if (featurep 'xemacs)
			       (cdr (split-string (function-arglist 'url-retrieve)))
			     (help-function-arglist 'url-retrieve)))
		   4)
	    (setq args (nconc args (list t))))
	  (apply #'url-retrieve args))
      (apply cb
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (url-cache-extract (url-cache-create-filename url))
                 (gravatar-data->image))
               cbargs))))

;;;###autoload
(defun gravatar-retrieve-synchronously (mail-address)
  "Synchronously retrieve a gravatar for MAIL-ADDRESS.
Value is either an image descriptor, or the symbol `error' if the
retrieval failed."
  (let ((url (gravatar-build-url mail-address)))
    (if (gravatar-cache-expired url)
        (with-current-buffer (url-retrieve-synchronously url)
	  (when gravatar-automatic-caching
            (url-store-in-cache (current-buffer)))
          (let ((data (gravatar-data->image)))
            (kill-buffer (current-buffer))
            data))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (url-cache-extract (url-cache-create-filename url))
        (gravatar-data->image)))))


(defun gravatar-retrieved (status cb &optional cbargs)
  "Callback function used by `gravatar-retrieve'."
  ;; Store gravatar?
  (when gravatar-automatic-caching
    (url-store-in-cache (current-buffer)))
  (if (plist-get status :error)
      ;; Error happened.
      (apply cb 'error cbargs)
    (apply cb (gravatar-data->image) cbargs))
  (kill-buffer (current-buffer)))

(provide 'gravatar)

;;; gravatar.el ends here
