;;; cl-preloaded.el --- Preloaded part of the CL library  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; The expectation is that structs defined with cl-defstruct do not
;; need cl-lib at run-time, but we'd like to hide the details of the
;; cl-struct metadata behind the cl-struct-define function, so we put
;; it in this pre-loaded file.

;;; Code:

(defun cl-struct-define (name docstring parent type named slots children-sym
                              tag print-auto)
  (if (boundp children-sym)
      (add-to-list children-sym tag)
    (set children-sym (list tag)))
  ;; If the cl-generic support, we need to be able to check
  ;; if a vector is a cl-struct object, without knowing its particular type.
  ;; So we use the (otherwise) unused function slots of the tag symbol
  ;; to put a special witness value, to make the check easy and reliable.
  (unless named (fset tag :quick-object-witness-check))
  (put name 'cl-struct-slots slots)
  (put name 'cl-struct-type (list type named))
  (if parent (put name 'cl-struct-include parent))
  (if print-auto (put name 'cl-struct-print print-auto))
  (if docstring (put name 'structure-documentation docstring)))

(provide 'cl-preloaded)
;;; cl-preloaded.el ends here
