;;; tab-bar.el --- frame-local tab bar with window configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: frames tabs
;; Maintainer: emacs-devel@gnu.org

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

;; Provides `tab-bar-mode' to control display of the tab-bar and
;; bindings for the global tab bar.

;; The normal global binding for [tab-bar] (below) uses the value of
;; `tab-bar-map' as the actual keymap to define the tab bar.  Modes
;; may either bind items under the [tab-bar] prefix key of the local
;; map to add to the global bar or may set `tab-bar-map'
;; buffer-locally to override it.

;;; Code:


(defgroup tab-bar nil
  "Frame-local tab bar."
  :group 'convenience
  :version "27.1")

(defgroup tab-bar-faces nil
  "Faces used in the tab bar."
  :group 'tab-bar
  :group 'faces
  :version "27.1")

(defface tab-bar
  '((default
     :box (:line-width 1 :style released-button)
     :foreground "black")
    (((type x w32 ns) (class color))
     :background "grey75")
    (((type x) (class mono))
     :background "grey"))
  "Tab bar face."
  :version "27.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab
  '((default
      :inherit tab-bar-tab-inactive)
    (t
     :background "grey75"))
  "Tab bar face for selected tab."
  :version "27.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab-inactive
  '((((class color) (min-colors 88))
     :box (:line-width -15 :style pressed-button)
     :background "grey60")
    (t
     :inherit highlight))
  "Tab bar face for non-selected tab."
  :version "27.1"
  :group 'tab-bar-faces)


(define-minor-mode tab-bar-mode
  "Toggle the tab bar in all graphical frames (Tab Bar mode)."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable tab-bar-mode
  (let ((val (if tab-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'tab-bar-lines val))
    ;; If the user has given `default-frame-alist' a `tab-bar-lines'
    ;; parameter, replace it.
    (if (assq 'tab-bar-lines default-frame-alist)
        (setq default-frame-alist
              (cons (cons 'tab-bar-lines val)
                    (assq-delete-all 'tab-bar-lines
                                     default-frame-alist)))))
  (when tab-bar-mode
    (global-set-key [(control shift iso-lefttab)] 'tab-bar-switch-to-prev-tab)
    (global-set-key [(control tab)]               'tab-bar-switch-to-next-tab)))

;;;###autoload
;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tab-bar-mode-from-frame (&optional arg)
  "Toggle tab bar on or off, based on the status of the current frame.
See `tab-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tab-bar-mode (if (> (frame-parameter nil 'tab-bar-lines) 0) 0 1))
    (tab-bar-mode arg)))

(defvar tab-bar-map (make-sparse-keymap)
  "Keymap for the tab bar.
Define this locally to override the global tab bar.")

(global-set-key [tab-bar]
                `(menu-item ,(purecopy "tab bar") ignore
                            :filter tab-bar-make-keymap))

(defconst tab-bar-keymap-cache (make-hash-table :weakness t :test 'equal))

(defun tab-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to show tabs in the tab bar."
  (if (= 1 (length tab-bar-map))
      (tab-bar-make-keymap-1)
    (let ((key (cons (frame-terminal) tab-bar-map)))
      (or (gethash key tab-bar-keymap-cache)
          (puthash key tab-bar-map tab-bar-keymap-cache)))))


(defvar tab-bar-separator " ")
(defvar tab-bar-tab-name-add nil)
(defvar tab-bar-tab-name-close nil)

(defun tab-bar-tab-name ()
  "Generate tab name in the context of the selected frame."
  (mapconcat
   (lambda (w) (buffer-name (window-buffer w)))
   (window-list)
   ", "))

(defun tab-bar-tabs ()
  "Return a list of tabs belonging to the selected frame.
Ensure the frame parameter `tabs' is pre-populated.
Return its existing value or a new value."
  (let ((tabs (frame-parameter nil 'tabs)))
    (unless tabs
      (setq tabs `((current-tab (name . ,(tab-bar-tab-name)))))
      (set-frame-parameter nil 'tabs tabs))
    tabs))

(defun tab-bar-make-keymap-1 ()
  "Generate an actual keymap from `tab-bar-map', without caching."
  (let ((i 0))
    (append
     '(keymap)
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (list (cond
               ((eq (car tab) 'current-tab)
                `(current-tab
                  menu-item
                  ,(propertize "Current tab" 'face 'tab-bar-tab)
                  ignore
                  :help "Current tab"))
               (t
                `(,(intern (format "tab-%i" i))
                  menu-item
                  ,(propertize (cdr (assq 'name tab)) 'face 'tab-bar-tab-inactive)
                  ,(lambda ()
                     (interactive)
                     (tab-bar-select-tab tab))
                  :help "Click to visit tab")))
              `(,(intern (format "close-tab-%i" i))
                menu-item
                ,(concat (propertize (or tab-bar-tab-name-close
                                         (if (char-displayable-p ?⮿) "⮿" "[x]"))
                                     'face (if (eq (car tab) 'current-tab)
                                               'tab-bar-tab
                                             'tab-bar-tab-inactive))
                         tab-bar-separator)
                ,(lambda ()
                   (interactive)
                   (tab-bar-close-tab tab))
                :help "Click to close tab")))
      (tab-bar-tabs))
     `((add-tab menu-item
                ,(propertize (or tab-bar-tab-name-add
                                 (if (char-displayable-p ?➕) "➕" "[+]"))
                             'face 'tab-bar-tab-inactive)
                tab-bar-add-tab
                :help "Click to add tab")))))


(defun tab-bar-read-tab-name (prompt)
  (let* ((tabs (tab-bar-tabs))
         (tab-name
          (completing-read prompt
                           (or (delq nil (mapcar (lambda (tab)
                                                   (cdr (assq 'name tab)))
                                                 tabs))
                               '("")))))
    (catch 'done
      (dolist (tab tabs)
        (when (equal (cdr (assq 'name tab)) tab-name)
          (throw 'done tab))))))

(defun tab-bar-new-tab ()
  (let ((tab `(tab
               (name . ,(tab-bar-tab-name))
               (wc . ,(current-window-configuration))
               (ws . ,(window-state-get
                       (frame-root-window (selected-frame)) 'writable)))))
    tab))

(defun tab-bar-find-prev-tab (&optional tabs)
  (unless tabs
    (setq tabs (tab-bar-tabs)))
  (unless (eq (car (car tabs)) 'current-tab)
    (while (and tabs (not (eq (car (car (cdr tabs))) 'current-tab)))
      (setq tabs (cdr tabs)))
    tabs))


(defun tab-bar-select-tab (tab)
  "Switch to the specified TAB."
  (interactive (list (tab-bar-read-tab-name "Select tab by name: ")))
  (when (and tab (not (eq (car tab) 'current-tab)))
    (let* ((tabs (tab-bar-tabs))
           (new-tab (tab-bar-new-tab))
           (wc (cdr (assq 'wc tab))))
      ;; During the same session, use window-configuration to switch
      ;; tabs, because window-configurations are more reliable
      ;; (they keep references to live buffers) than window-states.
      ;; But after restoring tabs from a previously saved session,
      ;; its value of window-configuration is unreadable,
      ;; so restore its saved window-state.
      (if (window-configuration-p wc)
          (set-window-configuration wc)
        (window-state-put (cdr (assq 'ws tab))
                          (frame-root-window (selected-frame)) 'safe))
      (while tabs
        (cond
         ((eq (car tabs) tab)
          (setcar tabs `(current-tab (name . ,(tab-bar-tab-name)))))
         ((eq (car (car tabs)) 'current-tab)
          (setcar tabs new-tab)))
        (setq tabs (cdr tabs)))
      (force-window-update))))

(defun tab-bar-switch-to-prev-tab ()
  "Switch to the previous tab."
  (interactive)
  (let ((prev-tab (tab-bar-find-prev-tab)))
    (when prev-tab
      (tab-bar-select-tab (car prev-tab)))))

(defun tab-bar-switch-to-next-tab ()
  "Switch to the next tab."
  (interactive)
  (let* ((tabs (tab-bar-tabs))
         (prev-tab (tab-bar-find-prev-tab tabs)))
    (if prev-tab
        (tab-bar-select-tab (car (cdr (cdr prev-tab))))
      (tab-bar-select-tab (car (cdr tabs))))))


(defcustom tab-bar-add-tab-to 'right
  "Defines where to create a new tab.
If `leftmost', create as the first tab.
If `left', create to the left from the current tab.
If `right', create to the right from the current tab.
If `rightmost', create as the last tab."
  :type '(choice (const :tag "First tab" leftmost)
                 (const :tag "To the left" left)
                 (const :tag "To the right" right)
                 (const :tag "Last tab" rightmost))
  :version "27.1")

(defun tab-bar-add-tab ()
  "Clone the current tab to the position specified by `tab-bar-add-tab-to'."
  (interactive)
  (unless tab-bar-mode
    (tab-bar-mode 1))
  (let* ((tabs (tab-bar-tabs))
         ;; (i-tab (- (length tabs) (length (memq tab tabs))))
         (new-tab (tab-bar-new-tab)))
    (cond
     ((eq tab-bar-add-tab-to 'leftmost)
      (setq tabs (cons new-tab tabs)))
     ((eq tab-bar-add-tab-to 'rightmost)
      (setq tabs (append tabs (list new-tab))))
     (t
      (let ((prev-tab (tab-bar-find-prev-tab tabs)))
        (cond
         ((eq tab-bar-add-tab-to 'left)
          (if prev-tab
              (setcdr prev-tab (cons new-tab (cdr prev-tab)))
            (setq tabs (cons new-tab tabs))))
         ((eq tab-bar-add-tab-to 'right)
          (if prev-tab
              (setq prev-tab (cdr prev-tab))
            (setq prev-tab tabs))
          (setcdr prev-tab (cons new-tab (cdr prev-tab))))))))
    (set-frame-parameter nil 'tabs tabs)
    (tab-bar-select-tab new-tab)))


(defcustom tab-bar-close-tab-select 'right
  "Defines what tab to select after closing the specified tab.
If `left', select the adjacent left tab.
If `right', select the adjacent right tab."
  :type '(choice (const :tag "Select left tab" left)
                 (const :tag "Select right tab" right))
  :version "27.1")

(defun tab-bar-close-current-tab (tab)
  "Close the current TAB.
After closing the current tab switch to the tab
specified by `tab-bar-close-tab-select'."
  (interactive
   (list
    (let* ((tabs (tab-bar-tabs))
           (prev-tab (tab-bar-find-prev-tab tabs)))
      (if prev-tab
          (tab-bar-select-tab (car prev-tab))
        (car tabs)))))
  (let* ((tabs (tab-bar-tabs))
         (i-tab (- (length tabs) (length (memq tab tabs))))
         (i-select
          (cond
           ((eq tab-bar-close-tab-select 'left)
            (1- i-tab))
           ((eq tab-bar-close-tab-select 'right)
            ;; Do nothing: the next tab will take
            ;; the index of the closed tab
            i-tab)
           (t 0)))
         (tabs (delq tab tabs))
         (i-select (max 0 (min (1- (length tabs)) i-select)))
         (select-tab (nth i-select tabs)))
    (set-frame-parameter nil 'tabs tabs)
    (tab-bar-select-tab select-tab)))

(defun tab-bar-close-tab (tab)
  "Close the specified TAB.
After closing the current tab switch to the tab
specified by `tab-bar-close-tab-select'."
  (interactive (list (tab-bar-read-tab-name "Close tab by name: ")))
  (when tab
    (if (eq (car tab) 'current-tab)
        (tab-bar-close-current-tab tab)
      ;; Close non-current tab, no need to switch to another tab
      (set-frame-parameter nil 'tabs (delq tab (tab-bar-tabs)))
      (force-window-update))))


(defvar ctl-x-6-map (make-sparse-keymap)
  "Keymap for tab commands.")
(defalias 'ctl-x-6-prefix ctl-x-6-map)
(define-key ctl-x-map "6" 'ctl-x-6-prefix)

(defun switch-to-buffer-other-tab (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another tab.
Like \\[switch-to-buffer-other-frame] (which see), but creates a new tab."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other tab: ")))
  (tab-bar-add-tab)
  (delete-other-windows)
  (switch-to-buffer buffer-or-name norecord))

(defun find-file-other-tab (filename &optional wildcards)
  "Edit file FILENAME, in another tab.
Like \\[find-file-other-frame] (which see), but creates a new tab."
  (interactive
   (find-file-read-args "Find file in other tab: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(progn
	  (setq value (nreverse value))
	  (switch-to-buffer-other-tab (car value))
	  (mapc 'switch-to-buffer (cdr value))
	  value)
      (switch-to-buffer-other-tab value))))

(define-key ctl-x-6-map "2" 'tab-bar-add-tab)
(define-key ctl-x-6-map "b" 'switch-to-buffer-other-tab)
(define-key ctl-x-6-map "f" 'find-file-other-tab)
(define-key ctl-x-6-map "\C-f" 'find-file-other-tab)


(provide 'tab-bar)

;;; tab-bar.el ends here
