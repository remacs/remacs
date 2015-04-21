;;; map-tests.el --- Tests for map.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for map.el

;;; Code:

(require 'ert)
(require 'map)

(defmacro with-maps-do (var &rest body)
  "Successively bind VAR to an alist, vector and hash-table.
Each map is built from the following alist data:
'((0 . 3) (1 . 4) (2 . 5)).
Evaluate BODY for each created map.

\(fn (var map) body)"
  (declare (indent 1) (debug t))
  (let ((alist (make-symbol "alist"))
        (vec (make-symbol "vec"))
        (ht (make-symbol "ht")))
   `(let ((,alist '((0 . 3)
                    (1 . 4)
                    (2 . 5)))
          (,vec (make-vector 3 nil))
          (,ht (make-hash-table)))
      (aset ,vec 0 '3)
      (aset ,vec 1 '4)
      (aset ,vec 2 '5)
      (puthash '0 3 ,ht)
      (puthash '1 4 ,ht)
      (puthash '2 5 ,ht)
      (dolist (,var (list ,alist ,vec ,ht))
        ,@body))))

(ert-deftest test-map-elt ()
  (with-maps-do map
    (assert (= 3 (map-elt map 0)))
    (assert (= 4 (map-elt map 1)))
    (assert (= 5 (map-elt map 2)))
    (assert (null (map-elt map 4)))))

(ert-deftest test-map-elt-default ()
  (with-maps-do map
    (assert (= 5 (map-elt map 7 5)))))

(ert-deftest test-map-put ()
  (with-maps-do map
    (map-put map 2 'hello)
    (assert (eq (map-elt map 2) 'hello)))
  (let ((ht (make-hash-table)))
    (map-put ht 2 'a)
    (assert (eq (map-elt ht 2)
                'a)))
  (let ((alist '((0 . a) (1 . b) (2 . c))))
    (map-put alist 2 'a)
    (assert (eq (map-elt alist 2)
                'a)))
  (let ((vec [3 4 5]))
   (should-error (map-put vec 3 6))))

(ert-deftest test-map-put-literal ()
  (assert (= (map-elt (map-put [1 2 3] 1 4) 1)
             4))
  (assert (= (map-elt (map-put (make-hash-table) 'a 2) 'a)
             2))
  (should-error (map-put '((a . 1)) 'b 2))
  (should-error (map-put '() 'a 1)))

(ert-deftest test-map-put-return-value ()
  (let ((ht (make-hash-table)))
    (assert (eq (map-put ht 'a 'hello) ht))))

(ert-deftest test-map-delete ()
  (with-maps-do map
    (map-delete map 1)
    (assert (null (map-elt map 1)))))

(ert-deftest test-map-delete-return-value ()
  (let ((ht (make-hash-table)))
    (assert (eq (map-delete ht 'a) ht))))

(ert-deftest test-map-nested-elt ()
  (let ((vec [a b [c d [e f]]]))
    (assert (eq (map-nested-elt vec '(2 2 0)) 'e)))
  (let ((alist '((a . 1)
                 (b . ((c . 2)
                       (d . 3)
                       (e . ((f . 4)
                             (g . 5))))))))
    (assert (eq (map-nested-elt alist '(b e f))
                4)))
  (let ((ht (make-hash-table)))
    (map-put ht 'a 1)
    (map-put ht 'b (make-hash-table))
    (map-put (map-elt ht 'b) 'c 2)
    (assert (eq (map-nested-elt ht '(b c))
                2))))

(ert-deftest test-map-nested-elt-default ()
  (let ((vec [a b [c d]]))
    (assert (null (map-nested-elt vec '(2 3))))
    (assert (null (map-nested-elt vec '(2 1 1))))
    (assert (= 4 (map-nested-elt vec '(2 1 1) 4)))))

(ert-deftest test-map-p ()
  (assert (map-p nil))
  (assert (map-p '((a . b) (c . d))))
  (assert (map-p '(a b c d)))
  (assert (map-p []))
  (assert (map-p [1 2 3]))
  (assert (map-p (make-hash-table)))
  (assert (map-p "hello"))
  (assert (not (map-p 1)))
  (assert (not (map-p 'hello))))

(ert-deftest test-map-keys ()
  (with-maps-do map
    (assert (equal (map-keys map) '(0 1 2))))
  (assert (null (map-keys nil)))
  (assert (null (map-keys []))))

(ert-deftest test-map-values ()
  (with-maps-do map
    (assert (equal (map-values map) '(3 4 5)))))

(ert-deftest test-map-pairs ()
  (with-maps-do map
    (assert (equal (map-pairs map) '((0 . 3)
                                     (1 . 4)
                                     (2 . 5))))))

(ert-deftest test-map-length ()
  (let ((ht (make-hash-table)))
    (puthash 'a 1 ht)
    (puthash 'b 2 ht)
    (puthash 'c 3 ht)
    (puthash 'd 4 ht)
    (assert (= 0 (map-length nil)))
    (assert (= 0 (map-length [])))
    (assert (= 0 (map-length (make-hash-table))))
    (assert (= 5 (map-length [0 1 2 3 4])))
    (assert (= 2 (map-length '((a . 1) (b . 2)))))
    (assert (= 4 (map-length ht)))))

(ert-deftest test-map-copy ()
  (with-maps-do map
    (let ((copy (map-copy map)))
      (assert (equal (map-keys map) (map-keys copy)))
      (assert (equal (map-values map) (map-values copy)))
      (assert (not (eq map copy))))))

(ert-deftest test-map-apply ()
  (with-maps-do map
    (assert (equal (map-apply (lambda (k v) (cons (int-to-string k) v))
                              map)
                   '(("0" . 3) ("1" . 4) ("2" . 5)))))
  (let ((vec [a b c]))
    (assert (equal (map-apply (lambda (k v) (cons (1+ k) v))
                              vec)
                   '((1 . a)
                     (2 . b)
                     (3 . c))))))

(ert-deftest test-map-keys-apply ()
  (with-maps-do map
    (assert (equal (map-keys-apply (lambda (k) (int-to-string k))
                                   map)
                   '("0" "1" "2"))))
  (let ((vec [a b c]))
    (assert (equal (map-keys-apply (lambda (k) (1+ k))
                                   vec)
                   '(1 2 3)))))

(ert-deftest test-map-values-apply ()
  (with-maps-do map
    (assert (equal (map-values-apply (lambda (v) (1+ v))
                                     map)
                   '(4 5 6))))
  (let ((vec [a b c]))
    (assert (equal (map-values-apply (lambda (v) (symbol-name v))
                                     vec)
                   '("a" "b" "c")))))

(ert-deftest test-map-filter ()
  (with-maps-do map
    (assert (equal (map-keys (map-filter (lambda (k v)
                                           (<= 4 v))
                                         map))
                   '(1 2)))
    (assert (null (map-filter (lambda (k v)
                                (eq 'd k))
                              map))))
  (assert (null (map-filter (lambda (k v)
                              (eq 3 v))
                            [1 2 4 5])))
  (assert (equal (map-filter (lambda (k v)
                               (eq 3 k))
                             [1 2 4 5])
                 '((3 . 5)))))

(ert-deftest test-map-remove ()
  (with-maps-do map
    (assert (equal (map-keys (map-remove (lambda (k v)
                                           (>= v 4))
                                         map))
                   '(0)))
    (assert (equal (map-keys (map-remove (lambda (k v)
                                           (eq 'd k))
                                         map))
                   (map-keys map))))
  (assert (equal (map-remove (lambda (k v)
                               (eq 3 v))
                             [1 2 4 5])
                 '((0 . 1)
                   (1 . 2)
                   (2 . 4)
                   (3 . 5))))
  (assert (null (map-remove (lambda (k v)
                              (>= k 0))
                            [1 2 4 5]))))

(ert-deftest test-map-empty-p ()
  (assert (map-empty-p nil))
  (assert (not (map-empty-p '((a . b) (c . d)))))
  (assert (map-empty-p []))
  (assert (not (map-empty-p [1 2 3])))
  (assert (map-empty-p (make-hash-table)))
  (assert (not (map-empty-p "hello")))
  (assert (map-empty-p "")))

(ert-deftest test-map-contains-key-p ()
  (assert (map-contains-key-p '((a . 1) (b . 2)) 'a))
  (assert (not (map-contains-key-p '((a . 1) (b . 2)) 'c)))
  (assert (map-contains-key-p '(("a" . 1)) "a"))
  (assert (not (map-contains-key-p '(("a" . 1)) "a" #'eq)))
  (assert (map-contains-key-p [a b c] 2))
  (assert (not (map-contains-key-p [a b c] 3))))

(ert-deftest test-map-some-p ()
  (with-maps-do map
    (assert (equal (map-some-p (lambda (k v)
                                 (eq 1 k))
                               map)
                   (cons 1 4)))
    (assert (not (map-some-p (lambda (k v)
                               (eq 'd k))
                             map))))
  (let ((vec [a b c]))
    (assert (equal (map-some-p (lambda (k v)
                                 (> k 1))
                               vec)
                   (cons 2 'c)))
    (assert (not (map-some-p (lambda (k v)
                               (> k 3))
                             vec)))))

(ert-deftest test-map-every-p ()
  (with-maps-do map
    (assert (map-every-p (lambda (k v)
                           k)
                         map))
    (assert (not (map-every-p (lambda (k v)
                                nil)
                              map))))
  (let ((vec [a b c]))
    (assert (map-every-p (lambda (k v)
                           (>= k 0))
                         vec))
    (assert (not (map-every-p (lambda (k v)
                                (> k 3))
                              vec)))))

(ert-deftest test-map-into ()
  (let* ((alist '((a . 1) (b . 2)))
         (ht (map-into alist 'hash-table)))
    (assert (hash-table-p ht))
    (assert (equal (map-into (map-into alist 'hash-table) 'list)
                   alist))
    (assert (listp (map-into ht 'list)))
    (assert (equal (map-keys (map-into (map-into ht 'list) 'hash-table))
                   (map-keys ht)))
    (assert (equal (map-values (map-into (map-into ht 'list) 'hash-table))
                   (map-values ht)))
    (assert (null (map-into nil 'list)))
    (assert (map-empty-p (map-into nil 'hash-table)))
    (should-error (map-into [1 2 3] 'string))))

(provide 'map-tests)
;;; map-tests.el ends here
