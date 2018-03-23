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
