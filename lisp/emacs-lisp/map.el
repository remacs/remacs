;;; map.el --- Map manipulation functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, map, hash-table, alist, array
;; Version: 1.0
;; Package: map

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; map.el provides map-manipulation functions that work on alists,
;; hash-table and arrays.  All functions are prefixed with "map-".
;;
;; Functions taking a predicate or iterating over a map using a
;; function take the function as their first argument.  All other
;; functions take the map as their first argument.

;; TODO:
;; - Add support for char-tables
;; - Maybe add support for gv?
;; - See if we can integrate text-properties
;; - A macro similar to let-alist but working on any type of map could
;;   be really useful

;;; Code:

(require 'seq)

(defun map-elt (map key &optional default)
  "Perform a lookup in MAP of KEY and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil.

If MAP is a list, `equal' is used to lookup KEY."
  (map--dispatch map
    :list (map--elt-list map key default)
    :hash-table (gethash key map default)
    :array (map--elt-array map key default)))

(defmacro map-put (map key value)
  "In MAP, associate KEY with VALUE and return MAP.
If KEY is already present in MAP, replace the associated value
with VALUE."
  (declare (debug t))
  `(progn
     (map--dispatch (m ,map m)
       :list (setq ,map (cons (cons ,key ,value) m))
       :hash-table (puthash ,key ,value m)
       :array (aset m ,key ,value))))

(defmacro map-delete (map key)
  "In MAP, delete the key KEY if present and return MAP.
If MAP is an array, store nil at the index KEY."
  (declare (debug t))
  `(progn
     (map--dispatch (m ,map m)
       :list (setq ,map (map--delete-alist m ,key))
       :hash-table (remhash ,key m)
       :array (map--delete-array m ,key))))

(defun map-nested-elt (map keys &optional default)
  "Travserse MAP using KEYS and return the looked up value or DEFAULT if nil.
Map can be a nested map composed of alists, hash-tables and arrays."
  (or (seq-reduce (lambda (acc key)
                    (when (map-p acc)
                      (map-elt acc key)))
                  keys
                  map)
      default))

(defun map-keys (map)
  "Return the list of keys in MAP."
  (map-apply (lambda (key _) key) map))

(defun map-values (map)
  "Return the list of values in MAP."
  (map-apply (lambda (_ value) value) map))

(defun map-pairs (map)
  "Return the elements of MAP as key/value association lists."
  (map-apply (lambda (key value)
               (cons key value))
             map))

(defun map-length (map)
  "Return the length of MAP."
  (length (map-keys map)))

(defun map-copy (map)
  "Return a copy of MAP."
  (map--dispatch map
    :list (seq-copy map)
    :hash-table (copy-hash-table map)
    :array (seq-copy map)))

(defun map-apply (function map)
  "Return the result of applying FUNCTION to each element of MAP.
FUNCTION is called with two arguments, the key and the value."
  (funcall (map--dispatch map
             :list #'map--apply-alist
             :hash-table #'map--apply-hash-table
             :array #'map--apply-array)
           function
           map))

(defun map-keys-apply (function map)
  "Return the result of applying FUNCTION to each key of MAP."
  (map-apply (lambda (key _)
               (funcall function key))
             map))

(defun map-values-apply (function map)
  "Return the result of applying FUNCTION to each value of MAP."
  (map-apply (lambda (_ val)
               (funcall function val))
             map))

(defun map-filter (pred map)
  "Return an alist of the key/val pairs of which (PRED key val) is non-nil in MAP."
  (delq nil (map-apply (lambda (key val)
                         (if (funcall pred key val)
                             (cons key val)
                           nil))
                       map)))

(defun map-remove (pred map)
  "Return an alist of the key/val pairs of which (PRED key val) is nil in MAP."
  (map-filter (lambda (key val) (not (funcall pred key val)))
              map))

(defun map-p (map)
  "Return non-nil if MAP is a map (list, hash-table or array)."
  (or (listp map)
      (hash-table-p map)
      (arrayp map)))

(defun map-empty-p (map)
  "Return non-nil is MAP is empty.
MAP can be a list, hash-table or array."
  (null (map-keys map)))

(defun map-contains-key-p (map key &optional testfn)
  "Return non-nil if MAP contain the key KEY, nil otherwise.
Equality is defined by TESTFN if non-nil or by `equal' if nil.
MAP can be a list, hash-table or array."
  (seq-contains-p (map-keys map) key testfn))

(defun map-some-p (pred map)
  "Return any key/value pair for which (PRED key val) is non-nil is MAP."
  (catch 'map--break
    (map-apply (lambda (key value)
                 (when (funcall pred key value)
                   (throw 'map--break (cons key value))))
               map)
    nil))

(defun map-every-p (pred map)
  "Return non-nil if (PRED key val) is non-nil for all elements of the map MAP."
  (catch 'map--break
    (map-apply (lambda (key value)
                 (or (funcall pred key value)
                     (throw 'map--break nil)))
               map)
    t))

(defun map-merge (type &rest maps)
  "Merge into a map of type TYPE all the key/value pairs in the maps MAPS."
  (let (result)
    (while maps
      (map-apply (lambda (key value)
                   (map-put result key value))
                 (pop maps)))
    (map-into result type)))

(defun map-into (map type)
  "Convert the map MAP into a map of type TYPE.
TYPE can be one of the following symbols: list or hash-table."
  (pcase type
    (`list (map-pairs map))
    (`hash-table (map--into-hash-table map))
    (t (error "Not a map type name: %S" type))))

(defmacro map--dispatch (spec &rest args)
  "Evaluate one of the provided forms depending on the type of MAP.

SPEC can be a map or a list of the form (VAR MAP [RESULT]).
ARGS should have the form [TYPE FORM]...

The following keyword types are meaningful: `:list',
`:hash-table' and `array'.

An error is thrown if MAP is neither a list, hash-table nor array.

Return RESULT if non-nil or the result of evaluation of the
form.

\(fn (VAR MAP [RESULT]) &rest ARGS)"
  (declare (debug t) (indent 1))
  (unless (listp spec)
    (setq spec `(,spec ,spec)))
  (let ((map-var (car spec))
        (result-var (make-symbol "result")))
    `(let ((,map-var ,(cadr spec))
           ,result-var)
       (setq ,result-var
             (cond ((listp ,map-var) ,(plist-get args :list))
                   ((hash-table-p ,map-var) ,(plist-get args :hash-table))
                   ((arrayp ,map-var) ,(plist-get args :array))
                   (t (error "Unsupported map: %s" ,map-var))))
       ,@(when (cddr spec)
          `((setq ,result-var ,@(cddr spec))))
       ,result-var)))

(defun map--apply-alist (function map)
  "Private function used to apply FUNCTION over MAP, MAP being an alist."
  (seq-map (lambda (pair)
             (funcall function
                      (car pair)
                      (cdr pair)))
           map))

(defun map--apply-hash-table (function map)
  "Private function used to apply FUNCTION over MAP, MAP being a hash-table."
  (let (result)
    (maphash (lambda (key value)
               (push (funcall function key value) result))
             map)
    (nreverse result)))

(defun map--apply-array (function map)
  "Private function used to apply FUNCTION over MAP, MAP being an array."
   (let ((index 0))
     (seq-map (lambda (elt)
                (prog1
                    (funcall function index elt)
                  (setq index (1+ index))))
              map)))

(defun map--elt-list (map key &optional default)
  "Lookup, in the list MAP, the value associated with KEY and return it.
If KEY is not found, return DEFAULT which defaults to nil."
  (let ((pair (assoc key map)))
    (if pair
        (cdr pair)
      default)))

(defun map--elt-array (map key &optional default)
  "Return the element of the array MAP at the index KEY.
If KEY is not found, return DEFAULT which defaults to nil."
  (let ((len (seq-length map)))
    (or (and (>= key 0)
             (<= key len)
             (seq-elt map key))
        default)))

(defun map--delete-alist (map key)
  "Return MAP with KEY removed."
  (seq-remove (lambda (pair)
                (equal key (car pair)))
              map))

(defun map--delete-array (map key)
  "Set nil in the array MAP at the index KEY if present and return MAP."
  (let ((len (seq-length map)))
    (and (>= key 0)
         (<= key len)
         (aset m key nil)))
  map)

(defun map--into-hash-table (map)
  "Convert MAP into a hash-table."
  (let ((ht (make-hash-table :size (map-length map)
                             :test 'equal)))
    (map-apply (lambda (key value)
                 (map-put ht key value))
               map)
    ht))

(provide 'map)
;;; map.el ends here
