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

(ert-deftest test-delete-overlay ()
  (let ((buf (get-buffer-create "test-delete-overlay")))
    (with-current-buffer buf
      (overlay-put (make-overlay (point-min) (point-max)) 'test "test")
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (delete-overlay (car (overlays-in (point-min) (point-max)))))
      (should (eq (overlays-in (point-min) (point-max)) nil))))

(ert-deftest test-delete-all-overlays ()
  (let ((buf (get-buffer-create "test-delete-all-overlays")))
    (with-current-buffer buf
      (overlay-put (make-overlay (point-min) (point-max)) 'test "test")
      (overlay-put (make-overlay (point-min) (point-max)) 'test "test")
      (should (= (length (overlays-in (point-min) (point-max))) 2))
      (delete-all-overlays)
      (should (eq (overlays-in (point-min) (point-max)) nil)))))

(ert-deftest test-erase-buffer ()
  (let ((buf (get-buffer-create "test-erase-buffer")))
    (with-current-buffer buf
      (insert "test")
      (erase-buffer)
      (should (string= (buffer-string) ""))
      (let (pos)
        (insert "test")
        (setq pos (point))
        (insert "narrowed")
        (narrow-to-region pos (point-max))
        (erase-buffer)
        ;; ensure widen is called
        (widen)
        (should (string= (buffer-string) ""))))))

(ert-deftest test-buffer-list-for-frame-is-unique ()
  (get-buffer-create "foo")
  (get-buffer-create "bar")
  (get-buffer-create "baz")
  (let ((the-buffers (buffer-list (selected-frame))))
    (should (equal (delq nil (delete-dups the-buffers))
                   the-buffers))))

(ert-deftest test-rename-buffer ()
    (let ((buf (get-buffer-create "test-rename-buffer")))
      (with-current-buffer buf
        (rename-buffer "test-rename-buffer-foo")
        (should (string= (buffer-name buf) "test-rename-buffer-foo")))))

(ert-deftest test-rename-buffer-empty ()
    (let ((buf (get-buffer-create "test-rename-buffer-empty")))
      (with-current-buffer buf
        (should-error (rename-buffer "")))))

(ert-deftest test-rename-buffer-existing ()
    (let ((buf (get-buffer-create "test-rename-buffer-existing")))
      (with-current-buffer buf
        (should-error (rename-buffer "test-rename-buffer-foo")))))

(ert-deftest test-rename-buffer-unique ()
    (let ((buf (get-buffer-create "test-rename-buffer")))
      (with-current-buffer buf
        (rename-buffer "test-rename-buffer-foo" t)
        (should (string= (buffer-name buf) "test-rename-buffer-foo<2>")))))

(ert-deftest test-generate-new-buffer-name ()
  (let ((buf-name "test-generate-new-buffer-name"))
    (get-buffer-create buf-name)
    (should (string= (generate-new-buffer-name buf-name) (concat buf-name "<2>")))))

(ert-deftest test-generate-new-buffer-name-ignore ()
  (let ((buf-name "test-generate-new-buffer-name"))
    (get-buffer-create buf-name)
    (should (string= (generate-new-buffer-name buf-name buf-name) buf-name))))

(ert-deftest test-generate-new-buffer-name-space ()
  (let ((buf-name " test-generate-new-buffer-name"))
    (get-buffer-create buf-name)
    (let*((random-name (generate-new-buffer-name buf-name))
          ;; 'random-name' should have the format like " test-generate-new-buffer-name-XXXXXX"
          ;; For present implementation "XXXXXXX" is a random number greater than or equal to 0
          ;; and less than 1_000_000.
          (random-number (string-to-number (substring random-name (1+ (length buf-name))))))
      (should-not (string= random-name buf-name))
      (should (< 0 random-number 999999)))))

(provide 'buffers-tests)

;;; buffers-tests.el ends here
