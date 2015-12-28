;;; idna.el --- translate non-ASCII domain names to ASCII

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail, net

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Written by looking at
;; http://stackoverflow.com/questions/183485/can-anyone-recommend-a-good-free-javascript-for-punycode-to-unicode-conversion

;;; Code:

(defun idna-encode-string (string)
  (cl-destructuring-bind (ascii complex)
      (cl-loop for i from 0
               for char across string
               when (< char 128)
               collect char into ascii
               else
               collect (cons i char) into complex
               finally (return (list ascii complex)))
    (concat (mapconcat 'string ascii "")
            "-"
            (idna-encode-complex (length ascii)
                                 (sort complex
                                       (lambda (e1 e2)
                                         (< (cdr e1) (cdr e2))))
                                 string))))

(defconst idna-initial-n 128)
(defconst idna-initial-bias 72)
(defconst idna-base 36)
(defconst idna-damp 700)
(defconst idna-tmin 1)
(defconst idna-tmax 26)
(defconst idna-skew 28)

(defun idna-decode-digit (cp)
  (if (< (- cp 48) 10)
      (- cp 22)
    (if (< (- cp 65) 26)
        (- cp 65)
      (if (< (- cp 97) 26)
          (- cp 97)
        idna-base))))

;; 0-25  a-z
;; 26-36 0-9
(defun idna-encode-digit (d)
  (if (< d 26)
      (+ ?a d)
    (+ ?0 (- d 26))))

(defun idna-adapt (delta num-points first-time)
  (let ((delta (if first-time
                   (/ delta idna-damp)
                 (/ delta 2)))
        (k 0))
    (setq delta (+ delta (/ delta num-points)))
    (cl-loop while (> delta (/ (* (- idna-base idna-tmin)
                                  idna-tmax)
                               2))
             do (setq delta (/ delta (- idna-base idna-tmin))
                      k (+ k idna-base)))
    (+ k (/ (* (1+ (- idna-base idna-tmin)) delta)
            (+ delta idna-skew)))))

(defun idna-encode-complex (insertion-points complex string)
  (let ((n idna-initial-n)
        (delta 0)
        (bias idna-initial-bias)
        (h insertion-points)
        result m)
    (while (< h (length string))
      (setq ijv (cl-loop for char across string
                         when (>= char n)
                         minimize char))
      (setq m ijv)
      (setq delta (+ delta (* (- m n) (+ h 1)))
            n m)
      (cl-loop for char across string
               when (< char n)
               do (cl-incf delta)
               when (= char ijv)
               do (progn
                    (setq q delta)
                    (cl-loop with k = idna-base
                             for t1 = (if (<= k bias)
                                          idna-tmin
                                        (if (>= k (+ bias idna-tmax))
                                            idna-tmax
                                          (- k bias)))
                             while (>= q t1)
                             do (push (idna-encode-digit
                                       (+ t1 (mod (- q t1)
                                                  (- idna-base t1))))
                                      result)
                             do (setq q (/ (- q t1) (- idna-base t1))
                                      k (+ k idna-base)))
                    (push (idna-encode-digit q) result)
                    (setq bias (idna-adapt delta (+ h 1) (= h insertion-points))
                          delta 0
                          h (1+ h))))
      (cl-incf delta)
      (cl-incf n))
    (nreverse result)))

(provide 'idna)

;;; shr.el ends here
