;;; cl-print.el --- CL-style generic printer facilies  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

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

;; Customizable print facility.
;;
;; The heart of it is the generic function `cl-print-object' to which you
;; can add any method you like.
;;
;; The main entry point is `cl-prin1'.

;;; Code:

(defvar cl-print-readably nil
  "If non-nil, try and make sure the result can be `read'.")

(defvar cl-print--number-table nil)

;;;###autoload
(cl-defgeneric cl-print-object (object stream)
  "Dispatcher to print OBJECT on STREAM according to its type.
You can add methods to it to customize the output.
But if you just want to print something, don't call this directly:
call other entry points instead, such as `cl-prin1'."
  ;; This delegates to the C printer.  The C printer will not call us back, so
  ;; we should only use it for objects which don't have nesting.
  (prin1 object stream))

(cl-defmethod cl-print-object ((object cons) stream)
  (let ((car (pop object)))
    (if (and (memq car '(\, quote \` \,@ \,.))
             (consp object)
             (null (cdr object)))
        (progn
          (princ (if (eq car 'quote) '\' car) stream)
          (cl-print-object (car object) stream))
      (princ "(" stream)
      (cl-print-object car stream)
      (while (and (consp object)
                  (not (and cl-print--number-table
                            (numberp (gethash object cl-print--number-table)))))
        (princ " " stream)
        (cl-print-object (pop object) stream))
      (when object
        (princ " . " stream) (cl-print-object object stream))
      (princ ")" stream))))

(cl-defmethod cl-print-object ((object vector) stream)
  (princ "[" stream)
  (dotimes (i (length object))
    (unless (zerop i) (princ " " stream))
    (cl-print-object (aref object i) stream))
  (princ "]" stream))

(cl-defmethod cl-print-object ((object compiled-function) stream)
  (princ "#<compiled-function " stream)
  (prin1 (help-function-arglist object 'preserve-names) stream)
  (princ " #<bytecode> >" stream))

;; This belongs in nadvice.el, of course, but some load-ordering issues make it
;; complicated: cl-generic uses macros from cl-macs and cl-macs uses advice-add
;; from nadvice, so nadvice needs to be loaded before cl-generic and hence
;; can't use cl-defmethod.
(cl-defmethod cl-print-object :extra "nadvice"
              ((object compiled-function) stream)
  (if (not (advice--p object))
      (cl-call-next-method)
    (princ "#<advice-wrapper " stream)
    (when (fboundp 'advice--where)
      (princ (advice--where object) stream)
      (princ " " stream))
    (cl-print-object (advice--cdr object) stream)
    (princ " " stream)
    (cl-print-object (advice--car object) stream)
    (let ((props (advice--props object)))
      (when props
        (princ " " stream)
        (cl-print-object props stream)))
    (princ ">" stream)))

(cl-defmethod cl-print-object ((object cl-structure-object) stream)
  (princ "#s(" stream)
  (let* ((class (symbol-value (aref object 0)))
         (slots (cl--struct-class-slots class)))
    (princ (cl--struct-class-name class) stream)
    (dotimes (i (length slots))
      (let ((slot (aref slots i)))
        (princ " :" stream)
        (princ (cl--slot-descriptor-name slot) stream)
        (princ " " stream)
        (cl-print-object (aref object (1+ i)) stream))))
  (princ ")" stream))

;;; Circularity and sharing.

;; I don't try to support the `print-continuous-numbering', because
;; I think it's ill defined anyway: if an object appears only once in each call
;; its sharing can't be properly preserved!

(cl-defmethod cl-print-object :around (object stream)
  ;; FIXME: Only put such an :around method on types where it's relevant.
  (let ((n (if cl-print--number-table (gethash object cl-print--number-table))))
    (if (not (numberp n))
        (cl-call-next-method)
      (if (> n 0)
          ;; Already printed.  Just print a reference.
          (progn (princ "#" stream) (princ n stream) (princ "#" stream))
        (puthash object (- n) cl-print--number-table)
        (princ "#" stream) (princ (- n) stream) (princ "=" stream)
        (cl-call-next-method)))))

(defvar cl-print--number-index nil)

(defun cl-print--find-sharing (object table)
  ;; Avoid recursion: not only because it's too easy to bump into
  ;; `max-lisp-eval-depth', but also because function calls are fairly slow.
  ;; At first, I thought using a list for our stack would cause too much
  ;; garbage to generated, but I didn't notice any such problem in practice.
  ;; I experimented with using an array instead, but the result was slightly
  ;; slower and the reduction in GC activity was less than 1% on my test.
  (let ((stack (list object)))
    (while stack
      (let ((object (pop stack)))
        (unless
            ;; Skip objects which don't have identity!
            (or (floatp object) (numberp object)
                (null object) (if (symbolp object) (intern-soft object)))
          (let ((n (gethash object table)))
            (cond
             ((numberp n))                   ;All done.
             (n                              ;Already seen, but only once.
              (let ((n (1+ cl-print--number-index)))
                (setq cl-print--number-index n)
                (puthash object (- n) table)))
             (t
              (puthash object t table)
              (pcase object
                (`(,car . ,cdr)
                 (push cdr stack)
                 (push car stack))
                ((pred stringp)
                 ;; We presumably won't print its text-properties.
                 nil)
                ((or (pred arrayp) (pred byte-code-function-p))
                 ;; FIXME: Inefficient for char-tables!
                 (dotimes (i (length object))
                   (push (aref object i) stack))))))))))))

(defun cl-print--preprocess (object)
  (let ((print-number-table (make-hash-table :test 'eq :rehash-size 2.0)))
    (if (fboundp 'print--preprocess)
        ;; Use the predefined C version if available.
        (print--preprocess object)           ;Fill print-number-table!
      (let ((cl-print--number-index 0))
        (cl-print--find-sharing object print-number-table)))
    print-number-table))

;;;###autoload
(defun cl-prin1 (object &optional stream)
  (cond
   (cl-print-readably (prin1 object stream))
   ((not print-circle) (cl-print-object object stream))
   (t
    (let ((cl-print--number-table (cl-print--preprocess object)))
      (cl-print-object object stream)))))

;;;###autoload
(defun cl-prin1-to-string (object)
  (with-temp-buffer
    (cl-prin1 object (current-buffer))
    (buffer-string)))

(provide 'cl-print)
;;; cl-print.el ends here
