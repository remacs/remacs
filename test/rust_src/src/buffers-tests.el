;;; buffers-tests.el --- tests for buffers.rs functions -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest test-buffer-base-buffer-indirect ()
  (let* ((base (get-buffer-create "base"))
         (ind-buf (make-indirect-buffer base "indbuf")))
    (should (eq (buffer-base-buffer ind-buf) base))))

(ert-deftest test-buffer-base-buffer-non-indirect ()
  (let ((buf (get-buffer-create "buf")))
    (should (eq (buffer-base-buffer buf) nil))))

(ert-deftest test-buffer-overlay-properties ()
  "Tests the overlay-properties function"
  (should-error (eval '(overlay-properties)) :type 'wrong-number-of-arguments)
  (should-error (eval '(overlay-properties "ab")) :type 'wrong-type-argument)
  (let ((overlay (make-overlay 1 1)))
    (should (null (overlay-properties overlay)))
    (overlay-put overlay 'priority 2)
    (should (equal (overlay-properties overlay) '(priority 2)))))

(ert-deftest test-get-buffer-create ()
  (should-error (eval '(get-buffer-create 23)) :type 'wrong-type-argument)
  (should-error (eval '(get-buffer-create nil)) :type 'wrong-type-argument)
  (let* ((new-name (generate-new-buffer-name "start"))
         (generated-buffer (generate-new-buffer new-name)))
    ;; Function call with a non-existing buffer name
    (should (bufferp generated-buffer))
    ;; Function call with a buffer
    (should (eq generated-buffer (get-buffer-create generated-buffer)))
    ;; Function call with an existing buffer name
    (should (eq generated-buffer (get-buffer-create new-name)))))
    