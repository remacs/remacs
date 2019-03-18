;;; display-fill-column-indicator.el --- interface for display-fill-column-indicator -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience

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

;; Provides a minor mode interface for `display-fill-column-indicator'.
;;
;; Toggle display of line numbers with M-x
;; display-fill-column-indicator-mode.  To enable line numbering in
;; all buffers, use M-x global-display-fill-column-indicator-mode.  To
;; change the default line column


;; NOTE: Customization variables for
;; `display-fill-column-indicator-column' and
;; `display-fill-column-indicator-char' itself are defined in
;; cus-start.el.

;;; Code:

(defgroup display-fill-column-indicator nil
  "Display line numbers in the buffer."
  :group 'convenience
  :group 'display)


;;;###autoload
(define-minor-mode display-fill-column-indicator-mode
  "Toggle display fill column indicator.
This uses `display-fill-column-indicator' internally.

To change the position of the line displayed by default,
customize `display-fill-column-indicator-column' you can change the
character for the line setting `display-fill-column-indicator-character'."
  :lighter nil
  (if display-fill-column-indicator-mode
      (progn
        (setq display-fill-column-indicator t)
        (unless display-fill-column-indicator-character
          (if (char-displayable-p ?\u2502)
              (setq display-fill-column-indicator-character ?\u2502)
            (setq display-fill-column-indicator-character ?|))))
    (setq display-fill-column-indicator nil)))

(defun display-fill-column-indicator--turn-on ()
  "Turn on `display-fill-column-indicator-mode'."
  (unless (or (minibufferp)
              (and (daemonp) (null (frame-parameter nil 'client))))
    (display-fill-column-indicator-mode)))

;;;###autoload
(define-globalized-minor-mode global-display-fill-column-indicator-mode
  display-fill-column-indicator-mode display-fill-column-indicator--turn-on)

(provide 'display-fill-column-indicator)

;;; display-fill-column-indicator.el ends here
