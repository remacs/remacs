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

(ert-deftest test-message-box ()
  (should-error (message-box)))

(ert-deftest test-delete-region--start<end ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (delete-region 5 12)
      (should (equal (buffer-string) "test contents")))))

(ert-deftest test-delete-region--start=end ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (delete-region 10 10)
      (should (equal (buffer-string) "test buffer contents")))))

(ert-deftest test-delete-region--start>end ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (delete-region 12 5)
      (should (equal (buffer-string) "test contents")))))

(ert-deftest test-delete-and-extract-region--start<end ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (should (equal (delete-and-extract-region 5 12) " buffer"))
      (should (equal (buffer-string) "test contents")))))

(ert-deftest test-delete-and-extract-region--start=end ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (should (equal (delete-and-extract-region 10 10) ""))
      (should (equal (buffer-string) "test buffer contents")))))

(ert-deftest test-delete-and-extract-region--start>end ()
  (let ((payload "test buffer contents"))
    (with-temp-buffer
      (insert payload)
      (should (equal (delete-and-extract-region 12 5) " buffer"))
      (should (equal (buffer-string) "test contents")))))

(ert-deftest time-comparison ()
  (let ((fixed-time '(23580 5248 742594 205000)))
    (should (time-less-p fixed-time (current-time)))))

(ert-deftest time-arithmetic ()
  (let ((fixed-time '(23580 5248 742594 205000))
        (more-time '(23580 5258 742594 205000))
        (less-time '(23580 5228 742594 205000)))
    (should (equal fixed-time (time-add fixed-time 0)))
    (should (equal fixed-time (time-subtract fixed-time 0)))
    (should (equal more-time (time-add fixed-time '(0 10))))
    (should (equal less-time (time-subtract fixed-time '(0 20))))))
