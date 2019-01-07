;;; strings-tests.el --- tests for strings.rs functions

;;; Code:

(require 'ert)

(ert-deftest stringp()
  (should (stringp "test")))

(ert-deftest multibyte-string-p()
  (should (multibyte-string-p "æøå"))
  (should (not (multibyte-string-p "abc")))
  (should (not (multibyte-string-p 1))))

(ert-deftest string-bytes()
  ;; Test regular ASCII string
  (should (eq (string-bytes "abc") 3))
  ;; Test multibyte unicode
  (should (eq (string-bytes "æ") 2)))

(ert-deftest string-equal()
  (should (string-equal "abc" "abc"))
  ;; Test for multibyte handling
  (should (string-equal "æøå" "æøå"))
  ;; Test for false positive on matching length
  (should (not (string-equal "abc" "abd")))
  ;; Test for false positive on substring match
  (should (not (string-equal "abc" "abcd")))
  ;; Test for false positive on same letters with different case
  (should (not (string-equal "abc" "ABC"))))

(ert-deftest string-width()
  ;; Simple unibyte string
  (should (eq (string-width "abc") 3))
  ;; Test tab expansion
  (should (eq (string-width "\t") 8))
  ;; Test single unicode character with multiple code-points
  (should (eq (string-width "é") 1)))

;;; strings-tests ends here
