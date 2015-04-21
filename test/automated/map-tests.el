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

(defmacro with-maps-do (alist-name vec-name ht-name &rest body)
  (declare (indent 3))
  `(let ((,alist-name '((a . 2)
                        (b . 3)
                        (c . 4)))
         (,vec-name (make-vector 3 nil))
         (,ht-name (make-hash-table)))
     (aset ,vec-name 0 'a)
     (aset ,vec-name 1 'b)
     (aset ,vec-name 2 'c)
     (puthash 'a 2 ,ht-name)
     (puthash 'b 3 ,ht-name)
     (puthash 'c 4 ,ht-name)
     (progn
       ,@body)))

(ert-deftest test-map-elt ()
  (with-maps-do alist vec ht
    (assert (= 2 (map-elt alist 'a)))
    (assert (= 3 (map-elt alist 'b)))
    (assert (= 4 (map-elt alist 'c)))
    (assert (null (map-elt alist 'd)))
    (assert (= 2 (map-elt ht 'a)))
    (assert (= 3 (map-elt ht 'b)))
    (assert (= 4 (map-elt ht 'c)))
    (assert (null (map-elt ht 'd)))
    (assert (eq 'a (map-elt vec 0)))
    (assert (eq 'b (map-elt vec 1)))
    (assert (eq 'c (map-elt vec 2)))
    (assert (null (map-elt vec 3)))))

(ert-deftest test-map-elt-default ()
  (with-maps-do alist vec ht
    (assert (= 5 (map-elt alist 'd 5)))
    (assert (= 5 (map-elt vec 4 5)))
    (assert (= 5 (map-elt ht 'd 5)))))

(ert-deftest test-map-put ()
  (with-maps-do alist vec ht
    (map-put alist 'd 4)
    (assert (= (map-elt alist 'd) 4))
    (map-put alist 'd 5)
    (assert (= (map-elt alist 'd) 5))
    (map-put ht 'd 4)
    (assert (= (map-elt ht 'd) 4))
    (map-put ht 'd 5)
    (assert (= (map-elt ht 'd) 5))
    (map-put vec 0 'd)
    (assert (eq (map-elt vec 0) 'd))
    (should-error (map-put vec 4 'd))))

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
  (with-maps-do alist vec ht
    (map-delete alist 'a)
    (assert (null (map-elt alist 'a)))
    (map-delete ht 'a)
    (assert (null (map-elt ht 'a)))
    (map-delete vec 2)
    (assert (null (map-elt vec 2)))))

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
  (with-maps-do alist vec ht
    (assert (map-p alist))
    (assert (map-p vec))
    (assert (map-p ht))
    (assert (not (map-p 1)))
    (assert (not (map-p 'hello)))))

(ert-deftest test-map-keys ()
  (with-maps-do alist vec ht
    (assert (equal (map-keys alist) '(a b c)))
    (assert (equal (map-keys vec) '(0 1 2)))
    (assert (equal (map-keys ht) '(a b c)))))

(ert-deftest test-map-values ()
  (with-maps-do alist vec ht
    (assert (equal (map-values alist) '(2 3 4)))
    (assert (equal (map-values vec) '(a b c)))
    (assert (equal (map-values ht) '(2 3 4)))))

(ert-deftest test-map-pairs ()
  (with-maps-do alist vec ht
    (assert (equal (map-pairs alist) alist))
    (assert (equal (map-pairs vec) '((0 . a)
                                     (1 . b)
                                     (2 . c))))
    (assert (equal (map-pairs ht) alist))))

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
  (with-maps-do alist vec ht
    (dolist (map (list alist vec ht))
      (let ((copy (map-copy map)))
        (assert (equal (map-keys map) (map-keys copy)))
        (assert (equal (map-values map) (map-values copy)))
        (assert (not (eq map copy)))))))

(ert-deftest test-map-apply ()
  (with-maps-do alist vec ht
    (dolist (map (list alist ht))
      (assert (equal (map-apply (lambda (k v) (cons (symbol-name k) v))
                                map)
                     '(("a" . 2) ("b" . 3) ("c" . 4)))))
    (assert (equal (map-apply (lambda (k v) (cons (1+ k) v))
                              vec)
                   '((1 . a)
                     (2 . b)
                     (3 . c))))))

(ert-deftest test-map-keys-apply ()
  (with-maps-do alist vec ht
    (dolist (map (list alist ht))
      (assert (equal (map-keys-apply (lambda (k) (symbol-name k))
                                map)
                     '("a" "b" "c"))))
    (assert (equal (map-keys-apply (lambda (k) (1+ k))
                              vec)
                   '(1 2 3)))))

(ert-deftest test-map-values-apply ()
  (with-maps-do alist vec ht
    (dolist (map (list alist ht))
      (assert (equal (map-values-apply (lambda (v) (1+ v))
                                map)
                     '(3 4 5))))
    (assert (equal (map-values-apply (lambda (v) (symbol-name v))
                              vec)
                   '("a" "b" "c")))))

(ert-deftest test-map-filter ()
  (with-maps-do alist vec ht
    (dolist (map (list alist ht))
      (assert (equal (map-keys (map-filter (lambda (k v)
                                             (<= 3 v))
                                           map))
                     '(b c)))
      (assert (null (map-filter (lambda (k v)
                                  (eq 'd k))
                                map))))
    (assert (null (map-filter (lambda (k v)
                                (eq 3 v))
                              [1 2 4 5])))
    (assert (equal (map-filter (lambda (k v)
                                 (eq 3 k))
                               [1 2 4 5])
                   '((3 . 5))))))

(ert-deftest test-map-remove ()
  (with-maps-do alist vec ht
    (dolist (map (list alist ht))
      (assert (equal (map-keys (map-remove (lambda (k v)
                                             (<= 3 v))
                                           map))
                     '(a)))
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
                              [1 2 4 5])))))

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
  (with-maps-do alist vec ht
    (dolist (map (list alist ht))
      (assert (equal (map-some-p (lambda (k v)
                                   (eq 'a k))
                                 map)
                     (cons 'a 2)))
      (assert (not (map-some-p (lambda (k v)
                                 (eq 'd k))
                               map))))
    (assert (equal (map-some-p (lambda (k v)
                                 (> k 1))
                               vec)
                   (cons 2 'c)))
    (assert (not (map-some-p (lambda (k v)
                               (> k 3))
                             vec)))))

(ert-deftest test-map-every-p ()
  (with-maps-do alist vec ht
    (dolist (map (list alist ht vec))
      (assert (map-every-p (lambda (k v)
                             k)
                           map))
      (assert (not (map-every-p (lambda (k v)
                                  nil)
                                map))))
    (assert (map-every-p (lambda (k v)
                           (>= k 0))
                         vec))
    (assert (not (map-every-p (lambda (k v)
                               (> k 3))
                              vec)))))

(ert-deftest test-map-into ()
  (with-maps-do alist vec ht
    (assert (hash-table-p (map-into alist 'hash-table)))
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
