;;; editfns-tests.el --- tests for editfns.rs functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-char-before ()
  (let ((char1 97)
        (char2 949)
        (marker (make-marker)))
    (with-temp-buffer
      (should (eq (char-before) nil))
      (should (eq (char-before -1) nil))
      (should (eq (char-before 100) nil))
      (insert "a")
      (should (char-equal (char-before) char1))
      (insert "ε")
      (should (char-equal (char-before) char2))
      (should (char-equal (char-before 2) char1))
      (set-marker marker 2)
      (should (char-equal (char-before marker) char1))
      (should-error (char-before (current-buffer)) :type 'wrong-type-argument))))

(ert-deftest test-id-functions ()
  (dolist (id (list
               (user-uid)
               (user-real-uid)
               (group-gid)
               (group-real-gid)
               (emacs-pid)))
    (should
     (or
      (integerp id)
      (floatp id)))))

(ert-deftest test-buffer-string ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (should (equal (buffer-string) payload)))))

