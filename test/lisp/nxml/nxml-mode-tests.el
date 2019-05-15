;;; nxml-mode-tests.el --- Test NXML Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'nxml-mode)

(defun nxml-mode-tests-correctly-indented-string (str)
  (with-temp-buffer
    (nxml-mode)
    (insert str)
    (indent-region (point-min) (point-max))
    (equal (buffer-string) str)))

(ert-deftest nxml-indent-line-after-attribute ()
  (should (nxml-mode-tests-correctly-indented-string "
<settings
    xmlns=\"http://maven.apache.org/SETTINGS/1.0.0\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xsi:schemaLocation=\"http://maven.apache.org/SETTINGS/1.0.0
                        https://maven.apache.org/xsd/settings-1.0.0.xsd\">
  <mirrors>
    ...
  </mirrors>
</settings>
"))
  (should (nxml-mode-tests-correctly-indented-string "\
<x>
  <abc xx=\"x/x/x/x/x/x/x/
           y/y/y/y/y/y/
           \">
    <zzz/>
  </abc>
  <nl>&#10;</nl>
</x>
")))

(ert-deftest nxml-balanced-close-start-tag-inline ()
  (with-temp-buffer
    (nxml-mode)
    (insert "<a><b c=\"\"</a>")
    (search-backward "</a>")
    (nxml-balanced-close-start-tag-inline)
    (should (equal (buffer-string) "<a><b c=\"\"></b></a>"))))

(ert-deftest nxml-mode-font-lock-quotes ()
  (with-temp-buffer
    (nxml-mode)
    (insert "<x a=\"dquote attr\" b='squote attr'>\"dquote text\"'squote text'</x>")
    (font-lock-ensure)
    (let ((squote-txt-pos (search-backward "squote text"))
          (dquote-txt-pos (search-backward "dquote text"))
          (squote-att-pos (search-backward "squote attr"))
          (dquote-att-pos (search-backward "dquote attr")))
      ;; Just make sure that each quote uses the same face for quoted
      ;; attribute values, and a different face for quoted text
      ;; outside tags.  Don't test `font-lock-string-face' vs
      ;; `nxml-attribute-value' here.
      (should (equal (get-text-property squote-att-pos 'face)
                     (get-text-property dquote-att-pos 'face)))
      (should (equal (get-text-property squote-txt-pos 'face)
                     (get-text-property dquote-txt-pos 'face)))
      (should-not (equal (get-text-property squote-txt-pos 'face)
                         (get-text-property dquote-att-pos 'face))))))

(provide 'nxml-mode-tests)
;;; nxml-mode-tests.el ends here
