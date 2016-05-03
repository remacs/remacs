(require 'xref)
(require 'cl-lib)

(defvar xref-tests-data-dir
  (expand-file-name "data/xref/"
                    (file-name-directory (or load-file-name (buffer-file-name)))))

(ert-deftest xref-collect-matches-finds-none-for-some-regexp ()
  (should (null (xref-collect-matches "zzz" "*" xref-tests-data-dir nil))))

(ert-deftest xref-collect-matches-finds-some-for-bar ()
  (let* ((matches (xref-collect-matches "bar" "*" xref-tests-data-dir nil))
         (locs (cl-sort (mapcar #'xref-item-location matches)
                        #'string<
                        :key #'xref-location-group)))
    (should (= 2 (length matches)))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (string-match-p "file2\\.txt\\'" (xref-location-group (nth 1 locs))))))

(ert-deftest xref-collect-matches-finds-two-matches-on-the-same-line ()
  (let* ((matches (xref-collect-matches "foo" "*" xref-tests-data-dir nil))
         (locs (mapcar #'xref-item-location matches)))
    (should (= 2 (length matches)))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 1 locs))))
    (should (equal 1 (xref-location-line (nth 0 locs))))
    (should (equal 1 (xref-location-line (nth 1 locs))))
    (should (equal 0 (xref-file-location-column (nth 0 locs))))
    (should (equal 4 (xref-file-location-column (nth 1 locs))))))

;; (ert-deftest xref-collect-matches-finds-an-empty-line-regexp-match ()
;;   (let* ((matches (xref-collect-matches "^$" "*" xref-tests-data-dir nil))
;;          (locs (mapcar #'xref-item-location matches)))
;;     (should (= 1 (length matches)))
;;     (should (string-match-p "file2\\.txt\\'" (xref-location-group (nth 0 locs))))
;;     (should (equal 1 (xref-location-line (nth 0 locs))))
;;     (should (equal 0 (xref-file-location-column (nth 0 locs))))))
