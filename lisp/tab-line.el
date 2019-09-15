;;; tab-line.el --- window-local tab line with window buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: windows tabs
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

;; To enable this mode, run `M-x global-tab-line-mode'.

;;; Code:

(require 'seq) ; tab-line.el is not pre-loaded so it's safe to use it here


(defgroup tab-line nil
  "Window-local tab line."
  :group 'convenience
  :version "27.1")

(defgroup tab-line-faces nil
  "Faces used in the tab line."
  :group 'tab-line
  :group 'faces
  :version "27.1")

(defface tab-line
  '((((type x w32 ns) (class color))
     :background "grey85"
     :foreground "black")
    (((type x) (class mono))
     :background "grey")
    (t
     :inverse-video t))
  "Tab line face."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-tab
  '((((class color) (min-colors 88))
     :box (:line-width 1 :style released-button)
     :background "grey85")
    (t
     :inverse-video nil))
  "Tab line face for selected tab."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-tab-inactive
  '((default
      :inherit tab-line-tab)
    (((class color) (min-colors 88))
     :background "grey75")
    (t
     :inverse-video t))
  "Tab line face for non-selected tabs."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-highlight
  '((default :inherit tab-line-tab))
  "Tab line face for highlighting."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-close-highlight
  '((t :foreground "red"))
  "Tab line face for highlighting."
  :version "27.1"
  :group 'tab-line-faces)


(defvar tab-line-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1] 'tab-line-select-tab)
    (define-key map [tab-line mouse-2] 'tab-line-select-tab)
    (define-key map [tab-line mouse-4] 'tab-line-switch-to-prev-tab)
    (define-key map [tab-line mouse-5] 'tab-line-switch-to-next-tab)
    (define-key map "\C-m" 'tab-line-select-tab)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for `tab-line-mode' window tabs.")

(defvar tab-line-add-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1] 'tab-line-add-tab)
    (define-key map [tab-line mouse-2] 'tab-line-add-tab)
    (define-key map "\C-m" 'tab-line-add-tab)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap to add `tab-line-mode' window tabs.")

(defvar tab-line-tab-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1] 'tab-line-close-tab)
    (define-key map [tab-line mouse-2] 'tab-line-close-tab)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap to close `tab-line-mode' window tabs.")


(defvar tab-line-separator " ")

(defvar tab-line-tab-name-ellipsis
  (if (char-displayable-p ?…) "…" "..."))

(defvar tab-line-button-new
  (propertize " + "
              'display `(image :type xpm
                               :file ,(expand-file-name
                                       "images/tabs/new.xpm"
                                       data-directory)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-add-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to add tab")
  "Button for creating a new tab.")

(defvar tab-line-button-close
  (propertize "x"
              'display `(image :type xpm
                               :file ,(expand-file-name
                                       "images/tabs/close.xpm"
                                       data-directory)
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-tab-close-map
              'mouse-face 'tab-line-close-highlight
              'help-echo "Click to close tab")
  "Button for closing the clicked tab.")


(defun tab-line-tab-name (buffer &optional buffers)
  "Generate tab name from BUFFER.
Reduce tab width proportionally to space taken by other tabs."
  (let ((tab-name (buffer-name buffer))
        (limit (when buffers
                 (max 1 (- (/ (window-width) (length buffers)) 3)))))
    (if (or (not limit) (< (length tab-name) limit))
        tab-name
      (propertize (truncate-string-to-width tab-name limit nil nil
                                            tab-line-tab-name-ellipsis)
                  'help-echo tab-name))))

(defvar tab-line-tabs-limit 15
  "Maximum number of buffer tabs displayed in the window tab-line.")

(defun tab-line-tabs (&optional window)
  (let* ((buffer (window-buffer window))
         (next-buffers (seq-remove (lambda (b) (eq b buffer))
                                   (window-next-buffers window)))
         (next-buffers (seq-filter #'buffer-live-p next-buffers))
         (prev-buffers (seq-remove (lambda (b) (eq b buffer))
                                   (mapcar #'car (window-prev-buffers window))))
         (prev-buffers (seq-filter #'buffer-live-p prev-buffers))
         ;; Remove next-buffers from prev-buffers
         (prev-buffers (seq-difference prev-buffers next-buffers))
         (half-limit (/ tab-line-tabs-limit 2))
         (prev-buffers-limit
          (if (> (length prev-buffers) half-limit)
              (if (> (length next-buffers) half-limit)
                  half-limit
                (+ half-limit (- half-limit (length next-buffers))))
            (length prev-buffers)))
         (next-buffers-limit
          (- tab-line-tabs-limit prev-buffers-limit))
         (buffer-tabs
          (append (reverse (seq-take prev-buffers prev-buffers-limit))
                  (list buffer)
                  (seq-take next-buffers next-buffers-limit))))
    buffer-tabs))

(defun tab-line-format ()
  "Template for displaying tab line for selected window."
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (buffer-tabs (tab-line-tabs window)))
    (append
     (mapcar
      (lambda (b)
        (concat
         (or tab-line-separator "")
         (apply 'propertize (concat (propertize
                                     (tab-line-tab-name b buffer-tabs)
                                     'keymap tab-line-tab-map)
                                    tab-line-button-close)
                `(
                  buffer ,b
                  face ,(if (eq b buffer)
                            'tab-line-tab
                          'tab-line-tab-inactive)
                  mouse-face tab-line-highlight))))
      buffer-tabs)
     (list (concat tab-line-separator tab-line-button-new)))))


(defun tab-line-add-tab (&optional e)
  (interactive "e")
  (if window-system ; (display-popup-menus-p)
      (mouse-buffer-menu e) ; like (buffer-menu-open)
    ;; tty menu doesn't support mouse clicks, so use tmm
    (tmm-prompt (mouse-buffer-menu-keymap))))

(defun tab-line-select-tab (&optional e)
  "Switch to the selected tab.
This command maintains the original order of prev/next buffers.
So for example, switching to a previous tab is equivalent to
using the `previous-buffer' command."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (buffer (get-pos-property 1 'buffer (car (posn-string posnp))))
         (window-buffer (window-buffer window))
         (next-buffers (seq-remove (lambda (b) (eq b window-buffer))
                                   (window-next-buffers window)))
         (prev-buffers (seq-remove (lambda (b) (eq b window-buffer))
                                   (mapcar #'car (window-prev-buffers window))))
         ;; Remove next-buffers from prev-buffers
         (prev-buffers (seq-difference prev-buffers next-buffers)))
    (cond
     ((memq buffer next-buffers)
      (dotimes (_ (1+ (seq-position next-buffers buffer)))
        (switch-to-next-buffer window)))
     ((memq buffer prev-buffers)
      (dotimes (_ (1+ (seq-position prev-buffers buffer)))
        (switch-to-prev-buffer window)))
     (t
      (switch-to-buffer buffer)))))

(defun tab-line-switch-to-prev-tab (&optional e)
  "Switch to the previous tab."
  (interactive "e")
  (switch-to-prev-buffer (posn-window (event-start e))))

(defun tab-line-switch-to-next-tab (&optional e)
  "Switch to the next tab."
  (interactive "e")
  (switch-to-next-buffer (posn-window (event-start e))))

(defun tab-line-close-tab (&optional e)
  "Close the selected tab."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (buffer (get-pos-property 1 'buffer (car (posn-string posnp)))))
    (with-selected-window window
      (if (eq buffer (current-buffer))
          (bury-buffer)
        (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
        (set-window-next-buffers nil (delq buffer (window-next-buffers))))
      (force-mode-line-update))))


;;;###autoload
(define-minor-mode global-tab-line-mode
  "Display window-local tab line."
  :group 'tab-line
  :type 'boolean
  :global t
  :init-value nil
  (setq-default tab-line-format (when global-tab-line-mode
                                  '(:eval (tab-line-format)))))


(provide 'tab-line)
;;; tab-line.el ends here
