;;; xdg.el --- XDG specification and standard support -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Created: 27 January 2017
;; Keywords: files, data

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Library providing some convenience functions for the following XDG
;; standards and specifications
;;
;; - XDG Base Directory Specification
;; - Thumbnail Managing Standard
;; - xdg-user-dirs configuration
;; - Desktop Entry Specification

;;; Code:

(eval-when-compile
  (require 'subr-x))


;; XDG Base Directory Specification
;; https://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html

(defmacro xdg--dir-home (environ default-path)
  (declare (debug (stringp stringp)))
  (let ((env (make-symbol "env")))
    `(let ((,env (getenv ,environ)))
       (if (or (null ,env) (not (file-name-absolute-p ,env)))
           (expand-file-name ,default-path)
         ,env))))

(defun xdg-config-home ()
  "Return the base directory for user specific configuration files."
  (xdg--dir-home "XDG_CONFIG_HOME" "~/.config"))

(defun xdg-cache-home ()
  "Return the base directory for user specific cache files."
  (xdg--dir-home "XDG_CACHE_HOME" "~/.cache"))

(defun xdg-data-home ()
  "Return the base directory for user specific data files."
  (xdg--dir-home "XDG_DATA_HOME" "~/.local/share"))

(defun xdg-runtime-dir ()
  "Return the value of $XDG_RUNTIME_DIR."
  (getenv "XDG_RUNTIME_DIR"))

(defun xdg-config-dirs ()
  "Return the config directory search path as a list."
  (let ((env (getenv "XDG_CONFIG_DIRS")))
    (if (or (null env) (string= env ""))
        '("/etc/xdg")
      (parse-colon-path env))))

(defun xdg-data-dirs ()
  "Return the data directory search path as a list."
  (let ((env (getenv "XDG_DATA_DIRS")))
    (if (or (null env) (string= env ""))
        '("/usr/local/share/" "/usr/share/")
      (parse-colon-path env))))


;; Thumbnail Managing Standard
;; https://specifications.freedesktop.org/thumbnail-spec/thumbnail-spec-latest.html

(defun xdg-thumb-uri (filename)
  "Return the canonical URI for FILENAME.
If FILENAME has absolute path /foo/bar.jpg, its canonical URI is
file:///foo/bar.jpg"
  (concat "file://" (expand-file-name filename)))

(defun xdg-thumb-name (filename)
  "Return the appropriate thumbnail filename for FILENAME."
  (concat (md5 (xdg-thumb-uri filename)) ".png"))

(defun xdg-thumb-mtime (filename)
  "Return modification time of FILENAME as integral seconds from the epoch."
  (floor (float-time (nth 5 (file-attributes filename)))))


;; XDG User Directories
;; https://www.freedesktop.org/wiki/Software/xdg-user-dirs/

(defconst xdg-line-regexp
  (eval-when-compile
    (rx "XDG_"
        (group-n 1 (or "DESKTOP" "DOWNLOAD" "TEMPLATES" "PUBLICSHARE"
                       "DOCUMENTS" "MUSIC" "PICTURES" "VIDEOS"))
        "_DIR=\""
        (group-n 2 (or "/" "$HOME/") (*? (or (not (any "\"")) "\\\"")))
        "\""))
  "Regexp matching non-comment lines in xdg-user-dirs config files.")

(defvar xdg-user-dirs nil
  "Alist of directory keys and values.")

(defun xdg--substitute-home-env (str)
  (if (file-name-absolute-p str) str
    (save-match-data
      (and (string-match "^$HOME/" str)
           (replace-match "~/" t nil str 0)))))

(defun xdg--user-dirs-parse-line ()
  "Return pair of user-dirs key to directory value in LINE, otherwise nil.
This should be called at the beginning of a line."
  (skip-chars-forward "[:blank:]")
  (when (and (/= (following-char) ?#)
             (looking-at xdg-line-regexp))
    (let ((k (match-string 1))
          (v (match-string 2)))
      (when (and k v) (cons k (xdg--substitute-home-env v))))))

(defun xdg--user-dirs-parse-file (filename)
  "Return alist of xdg-user-dirs from FILENAME."
  (let (elt res)
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (while (not (eobp))
          (setq elt (xdg--user-dirs-parse-line))
          (when (consp elt) (push elt res))
          (forward-line))))
    res))

(defun xdg-user-dir (name)
  "Return the directory referred to by NAME."
  (when (null xdg-user-dirs)
    (setq xdg-user-dirs
          (xdg--user-dirs-parse-file
           (expand-file-name "user-dirs.dirs" (xdg-config-home)))))
  (let ((dir (cdr (assoc name xdg-user-dirs))))
    (when dir (expand-file-name dir))))


;; Desktop Entry Specification
;; https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.1.html

(defconst xdg-desktop-group-regexp
  (rx "[" (group-n 1 (+? (in " -Z\\^-~"))) "]")
  "Regexp matching desktop file group header names.")

;; TODO Localized strings left out intentionally, as Emacs has no
;; notion of l10n/i18n
(defconst xdg-desktop-entry-regexp
  (rx (group-n 1 (+ (in "A-Za-z0-9-")))
      ;; (? "[" (group-n 3 (+ nonl)) "]")
      (* blank) "=" (* blank)
      (group-n 2 (* nonl)))
  "Regexp matching desktop file entry key-value pairs.")

(defun xdg-desktop-read-group ()
  "Return hash table of group of desktop entries in the current buffer."
  (let ((res (make-hash-table :test #'equal)))
    (while (not (or (eobp) (looking-at xdg-desktop-group-regexp)))
      (skip-chars-forward "[:blank:]")
      (cond
       ((eolp))
       ((= (following-char) ?#))
       ((looking-at xdg-desktop-entry-regexp)
        (puthash (match-string 1) (match-string 2) res))
       ;; Filter localized strings
       ((looking-at (rx (group-n 1 (+ (in alnum "-"))) (* blank) "[")))
       (t (error "Malformed line: %s"
                 (buffer-substring (point) (point-at-eol)))))
      (forward-line))
    res))

(defun xdg-desktop-read-file (filename &optional group)
  "Return group contents of desktop file FILENAME as a hash table.
Optional argument GROUP defaults to the string \"Desktop Entry\"."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (while (and (skip-chars-forward "[:blank:]" (line-end-position))
                (or (eolp) (= (following-char) ?#)))
      (forward-line))
    (unless (looking-at xdg-desktop-group-regexp)
      (error "Expected group name!  Instead saw: %s"
             (buffer-substring (point) (point-at-eol))))
    (unless (equal (match-string 1) "Desktop Entry")
      (error "Wrong first group: %s" (match-string 1)))
    (when group
      (while (and (re-search-forward xdg-desktop-group-regexp nil t)
                  (not (equal (match-string 1) group)))))
    (forward-line)
    (xdg-desktop-read-group)))

(defun xdg-desktop-strings (value)
  "Partition VALUE into elements delimited by unescaped semicolons."
  (let (res)
    (setq value (string-trim-left value))
    (dolist (x (split-string (replace-regexp-in-string "\\\\;" "\0" value) ";"))
      (push (replace-regexp-in-string "\0" ";" x) res))
    (when (null (string-match-p "[^[:blank:]]" (car res))) (pop res))
    (nreverse res)))

(provide 'xdg)

;;; xdg.el ends here
