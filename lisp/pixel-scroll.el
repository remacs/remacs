;;; pixel-scroll.el --- Scroll a line smoothly

;; Copyright (C) 2017 Free Software Foundation, Inc.
;; Author: Tak Kunihiro <tkk@misasa.okayama-u.ac.jp>
;; Keywords: mouse
;; Package: emacs

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

;; Usage:
;;
;; To interactively toggle the mode:
;;
;;   M-x pixel-scroll-mode RET
;;
;; To make the mode permanent, put these in your init file:
;;
;;   (require 'pixel-scroll)
;;   (pixel-scroll-mode 1)

;;; Commentary:

;; This package offers a global minor mode which makes mouse-wheel
;; scroll a line smoothly.
;;
;; Scrolling a line up by `set-window-vscroll' and that by `scroll-up'
;; give similar display as shown below.
;;
;;  A: (scroll-up 1)
;;  B: (set-window-vscroll nil (frame-char-height) t)
;;
;; Also scrolling a pixel up by `set-window-vscroll' and that by
;; `scroll-up' give similar display, when vscroll is the last pixel of
;; the line, as shown below.
;;
;;  A: (scroll-up 1)
;;  B: (set-window-vscroll nil (1- (frame-char-height) t)) (scroll-up 1)
;;
;; When point reaches to the top of a window on scroll by
;; `set-window-vscroll', vscroll is set to zero.  To scroll a line
;; smoothly and continuously, this package scrolls a line by following
;; sequences.
;;
;;  (vertical-motion 1)
;;  (dolist (vs (number-sequence 1 (1- (frame-char-height))))
;;    (set-window-vscroll nil vs t) (sit-for 0))
;;  (scroll-up 1)

;;; Todo:
;;
;; Allowing pixel-level scrolling in Emacs requires a thorough review
;; of the related functionalities, to make sure none of them zeroes
;; out vscroll where users won't want that.

;;; Code:

(require 'mwheel)

(defvar pixel-wait 0
  "Idle time on each step of pixel scroll specified in second.
More wait will result in slow and gentle scroll.")

(defvar pixel-resolution-fine-flag nil
  "Set scrolling resolution to a pixel instead of a line.
After a pixel scroll, typing C-n or C-p scrolls the window to
make it fully visible, and undoes the effect of the pixel-level
scroll.")

;;;###autoload
(define-minor-mode pixel-scroll-mode
  "A minor mode to scroll text pixel-by-pixel.
With a prefix argument ARG, enable Pixel Scroll mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable Pixel Scroll mode
if ARG is omitted or nil."
  :init-value nil
  :group 'scrolling
  :global t
  :version "26.1"

  (if pixel-scroll-mode
      (setq mwheel-scroll-up-function 'pixel-scroll-up
            mwheel-scroll-down-function 'pixel-scroll-down)
    (setq mwheel-scroll-up-function 'scroll-up
          mwheel-scroll-down-function 'scroll-down)))

(defun pixel-scroll-up (&optional arg)
  "Scroll text of selected window up ARG lines.
This is an alternative of `scroll-up'.  Scope moves downward."
  (interactive)
  (or arg (setq arg 1))
  (dotimes (ii arg) ; move scope downward
    (if (pixel-eob-at-top-p) ; when end-of-the-buffer is close
        (scroll-up 1) ; relay on robust method
      (when (pixel-point-at-top-p) ; prevent too late
        (vertical-motion 1)) ; move point downward
      (pixel-scroll-pixel-up (if pixel-resolution-fine-flag
                                 1
                               (pixel-line-height)))))) ; move scope downward

(defun pixel-scroll-down (&optional arg)
  "Scroll text of selected window down ARG lines.
This is and alternative of `scroll-down'.  Scope moves upward."
  (interactive)
  (or arg (setq arg 1))
  (dotimes (ii arg)
    (if (or (pixel-bob-at-top-p) ; when beginning-of-the-buffer is seen
            (pixel-eob-at-top-p)) ; for file with a long line
        (scroll-down 1) ; relay on robust method
      (while (pixel-point-at-bottom-p) ; prevent too late (multi tries)
        (vertical-motion -1))
      (pixel-scroll-pixel-down (if pixel-resolution-fine-flag
                                   1
                                 (pixel-line-height -1))))))

(defun pixel-bob-at-top-p ()
  "Return non-nil if beginning of buffer is at top of window."
  (equal (window-start) (point-min)))

(defun pixel-eob-at-top-p ()
  "Return non-nil if end of buffer is at top of window."
  (<= (count-lines (window-start) (window-end)) 2)) ; count-screen-lines

(defun pixel-posn-y-at-point ()
  "Return y coordinates of point in pixels of current window."
  (let ((hscroll0 (window-hscroll))
        (y (cdr (posn-x-y (posn-at-point)))))
    ;; when point is out of scope by hscroll
    (unless y
      (save-excursion
        (set-window-hscroll nil (current-column))
        (setq y (cdr (posn-x-y (posn-at-point))))
        (set-window-hscroll nil hscroll0)))
    y))

(defun pixel-point-at-top-p ()
  "Return if point is located at top of a window."
  (let* ((y (pixel-posn-y-at-point))
         (top-margin y))
    (< top-margin (pixel-line-height))))

(defun pixel-point-at-bottom-p ()
  "Return if point is located at bottom of a window."
  (let* ((y (pixel-posn-y-at-point))
         (edges (window-inside-pixel-edges))
         (height (- (nth 3 edges) (nth 1 edges))) ; (- bottom top)
         (bottom-margin (- height (+ y (line-pixel-height))))) ; bottom margin
    (< bottom-margin (pixel-line-height -1)))) ; coming unseen line

(defun pixel-scroll-pixel-up (amt)
  "Scroll text of selected windows up AMT pixels.
Scope moves downward."
  (while (>= (+ (window-vscroll nil t) amt)
             (pixel-line-height))
    (setq amt (- amt (pixel--whistlestop-line-up)))) ; major scroll
  (pixel--whistlestop-pixel-up amt)) ; minor scroll

(defun pixel-scroll-pixel-down (amt)
  "Scroll text of selected windows down AMT pixels.
Scope moves upward."
  (while (> amt 0)
    (let ((vs (window-vscroll nil t)))
      (if (equal vs 0)
          (pixel-scroll-down-and-set-window-vscroll
           (1- (pixel-line-height -1)))
        (set-window-vscroll nil (1- vs) t))
      (setq amt (1- amt))
      (sit-for pixel-wait))))

(defun pixel--whistlestop-line-up ()
  "Scroll text upward a line with each pixel whistlestopped.
When `vscroll' is non-zero, complete scrolling a line.  When
`vscroll' is larger than height of multiple lines, for example
88, this flushes multiple lines.  At the end, `vscroll' will be
zero.  This assumes that the lines are with the same height.
Scope moves downward.  This function returns number of pixels
that was scrolled."
  (let* ((src (window-vscroll nil t))  ; EXAMPLE (initial)      @0   @8  @88
         (height (pixel-line-height))  ;                        25   25   23
         (line (1+ (/ src height)))    ; catch up + one line    Ä1   Ä1   Ä4
         (dst (* line height))         ; goal                  @25  @25  @92
         (delta (- dst src)))          ; pixels to be scrolled  25   17    4
    (pixel--whistlestop-pixel-up (1- delta)) ; until one less  @24  @24  @91
    (scroll-up line) (sit-for pixel-wait) ; scroll 1 pixel      @0   @0   @0
    delta))

(defun pixel--whistlestop-pixel-up (n)
  "Scroll text upward by N pixels with each pixel whistlestopped.
Scope moves downward."
  (when (> n 0)
    (let ((vs0 (window-vscroll nil t)))
      (dolist (vs (number-sequence (1+ vs0) (+ vs0 n)))
        (set-window-vscroll nil vs t) (sit-for pixel-wait)))))

(defun pixel-line-height (&optional pos)
  "Return height in pixels of text line at POS in the selected window.
When POS is nil or negative, height of the first line or the coming
unseen line above the first line, respectively, is provided."
  (or pos (setq pos (window-start)))
  (when (< pos 0)
    (setq pos (pixel-point-at-unseen-line)))
  (save-excursion
    (goto-char pos)
    (line-pixel-height))) ; frame-char-height

(defun pixel-point-at-unseen-line ()
  "Return the character position of line above the selected window.
The returned value is the position of the first character on the
unseen line just above the scope of current window."
  (let* ((pos0 (window-start))
         (vscroll0 (window-vscroll nil t))
         (pos
          (save-excursion
            (goto-char pos0)
            (if (bobp)
                (point-min)
              ;; When there's an overlay string at window-start,
              ;; (beginning-of-visual-line 0) stays put.
              (let ((ppos (point))
                    (tem (beginning-of-visual-line 0)))
                (if (eq tem ppos)
                    (vertical-motion -1))
                (point))))))
    ;; restore initial position
    (set-window-start nil pos0 t)
    (set-window-vscroll nil vscroll0 t)
    pos))

(defun pixel-scroll-down-and-set-window-vscroll (vscroll)
  "Scroll down a line and set VSCROLL in pixels.
It is important to call `set-window-start' to force the display
engine use that particular position as the window-start point.
Otherwise, redisplay will reset the window's vscroll."
  (set-window-start nil (pixel-point-at-unseen-line) t)
  (set-window-vscroll nil vscroll t))

(provide 'pixel-scroll)
;;; pixel-scroll.el ends here
